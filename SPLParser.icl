implementation module SPLParser

import ParserCombinators
import Token
import Misc

import Control.Applicative
import Control.Monad

import StdArray

instance zero MetaData where
	zero = { pos  = zero
		   , type = Nothing
		   }

pSatisfyTokenType :: TokenType -> Parser Token Token
pSatisfyTokenType type = pSatisfy (\(Token other_type _ _). type == other_type)
	
pSatisfyTokenTypeString :: TokenType [String] -> Parser Token Token
pSatisfyTokenTypeString type strings = pSatisfy (\(Token other_type string _). (type == other_type) && (isMember string strings))

pSatisfyStringToken :: [String] -> Parser Token Token
pSatisfyStringToken strings = pSatisfyTokenTypeString StringToken strings

pSatisfyBrace :: BraceType BraceStyle -> Parser Token Token
pSatisfyBrace btype bstyle = pSatisfy (\(Token type _ _) = 
					case type of
						(Brace other_btype other_bstyle) = other_btype == btype && other_bstyle == bstyle 
						_	= False)

pMaybe :: (Parser a t) -> Parser a (Maybe t)
pMaybe parse = (Just <$> parse) <<|> (pYield Nothing)

pBetweenBrackets :: BraceStyle (Parser Token t) -> Parser Token t
pBetweenBrackets bstyle parse = pSatisfyBrace Open bstyle *> parse <* pSatisfyBrace Close bstyle

parser :: [Token] -> ([(AST, [Token])], [Error])
parser tokens = runParser parseAST tokens

funType = [
	(Token StringToken "tempDay" {line=21 ,col=31}),
	(Token Operator "-" {line=21 ,col=39}),
	(Token Operator "-" {line=21 ,col=41}),
	(Token Operator "-" {line=21 ,col=43}),
	(Token Operator "-" {line=21 ,col=45}),
	(Token Operator "-" {line=21 ,col=47}),
	(Token StringToken "dcLengthOfMonth" {line=21 ,col=49})
	]

pars :: Parser Token [Expr]
pars =
		(parseExp										>>= \e.
		pMany (pSatisfyTokenType Comma >>| parseExp)	>>= \es.
		return [e:es])
	<<|>
		(return [])

Start = runParser pars funType 


parseAST :: Parser Token AST
parseAST =	pMany parseDecl				>>= \decls.
			pSatisfyTokenType EOFToken	>>|
			return decls

parseDecl :: Parser Token Decl
parseDecl = (
					parseVarDecl >>= \v. pYield (Var v)
				) <<|> (
					parseFunDecl >>= \f. pYield (Fun f)
				)

parseVarDecl :: Parser Token VarDecl
parseVarDecl = VarDecl <$> parseVarType <*> parseId <* pSatisfyTokenType Assignment
				<*> parseExp <* pSatisfyTokenType TerminatorToken
where
	parseVarType = (pSatisfyStringToken ["var"] >>| return Nothing)
				   <<|>
				   (Just <$> parseType)

parseFunDecl :: Parser Token FunDecl
parseFunDecl = FunDecl <$> parseId <*> pBetweenBrackets Round parseIds
	<*> pMaybe (pSatisfyTokenType TypeIndicator >>| parseFunType)
	<* pSatisfyBrace Open Curly <*> pMany parseVarDecl
	<*> pSome parseStmt <* pSatisfyBrace Close Curly
				
parseIds :: Parser Token [Id]
parseIds = (
						parseId
					>>= \t. pMany (
							pSatisfyTokenType Comma
						>>| parseId)
					>>= \l. pYield [t : l]
				) <<|> (
					pYield []
				)

parseFunType :: Parser Token Type
parseFunType =
	pMany parseType									>>= \argTypes.
	pSatisfyTokenType TypeArrow						>>|
	(
			(pSatisfyStringToken ["Void"] >>|
			 pYield (FuncType argTypes Nothing)				)
		<<|>
			( parseType	>>= \retType.
			  pYield (FuncType argTypes (Just retType))		)
	)

parseType :: Parser Token Type
parseType = (BasicType <$> parseBasicType) <<|> parseTupleType <<|> parseArrayType <<|> parseIdentType
where
	parseTupleType =
		pSatisfyBrace Open Round	>>|
		parseType					>>= \t1.
		pSatisfyTokenType Comma		>>|
		parseType					>>= \t2.
		pSatisfyBrace Close Round	>>|
		return (TupleType t1 t2)
	parseArrayType = ArrayType <$> pBetweenBrackets Square parseType
	parseIdentType = IdentType <$> parseId

parseBasicType :: Parser Token BasicType
parseBasicType = (pSatisfyStringToken ["Int"] >>| pYield (IntType)) <<|>
					(pSatisfyStringToken ["Bool"] >>| pYield (BoolType)) <<|>
					(pSatisfyStringToken ["Char"] >>| pYield (CharType))

parseStmt :: Parser Token Stmt
parseStmt = parseStmtIf <<|>
			parseStmtWhile <<|>
			parseStmtAss <<|>
			parseStmtReturn <<|>
			parseStmtFunCall // this one should be at last, else return (1); is interpreted as a function call
		where parseStmtFunCall = StmtFunCall <$> (parseFunCall <* pSatisfyTokenType TerminatorToken)

parseStmtIf :: Parser Token Stmt
parseStmtIf = pSatisfyStringToken ["if"]	>>|
			pBetweenBrackets Round parseExp					>>= \exp.
			pBetweenBrackets Curly (pMany parseStmt)		>>= \body.
			pMaybe elsepart									>>= \maybeElseBody.
			return (StmtIf exp body maybeElseBody)
where
	elsepart =	pSatisfyStringToken ["else"]	>>|
				pBetweenBrackets Curly (pMany parseStmt)
			
parseStmtWhile :: Parser Token Stmt
parseStmtWhile =  pSatisfyStringToken ["while"]
			>>| pSatisfyBrace Open Round
			>>| parseExp
			>>= \exp. pSatisfyBrace Close Round
			>>| pSatisfyBrace Open Curly
			>>| pMany parseStmt
			>>= \stmts. pSatisfyBrace Close Curly
			>>| pYield (StmtWhile exp stmts)
			
parseStmtAss :: Parser Token Stmt
parseStmtAss = parseIdentWithFields
				>>= \i. pSatisfyTokenType Assignment
				>>| parseExp
				>>= \e. pSatisfyTokenType TerminatorToken
				>>| pYield (StmtAss i e)
				
parseStmtFun :: Parser Token Stmt
parseStmtFun = parseFunCall
				>>= \f. pSatisfyTokenType TerminatorToken
				>>| pYield (StmtFunCall f)
				
parseStmtReturn :: Parser Token Stmt
parseStmtReturn =
	pSatisfyStringToken ["return"] >>| (
			(	parseExp							>>= \exp.
				pSatisfyTokenType TerminatorToken	>>|
				pYield (StmtRet exp)						)	
			<<|>
			(	pSatisfyTokenType TerminatorToken	>>|
				pYield (StmtRetV)								)
	)

parseFunCall :: Parser Token FunCall
parseFunCall = pSatisfyTokenType StringToken
			>>= \(Token StringToken name _). pSatisfyBrace Open Round
			>>| parseActArgs
			>>= \args. pSatisfyBrace Close Round
			>>| pYield (FunCall name args)

parseActArgs :: Parser Token [Expr]
parseActArgs =
		( parseExp										>>= \e.
		pMany (pSatisfyTokenType Comma >>| parseExp)	>>= \es.
		return [e:es]) 
	<<|>
		( return [])

/*
Operators in increasing binding power:
Op1  ::=  ||
Op2  ::=  &&
Op3  ::=  == | < | > | <= | >= | !=
Op4  ::=  :
Op5  ::=  + | -
Op6  ::=  * | / | %
OpUn ::=  ! | -

Adjusted grammar:
Exp		::= Exp1
Exp1 ::= Exp2  (Op1 Exp2 )*
Exp2 ::= Exp3  (Op2 Exp3 )*
Exp3 ::= Exp4  (Op3 Exp4 )*
Exp4 ::= Exp5  [Op4 Exp4 ]
Exp5 ::= Exp6  (Op5 Exp6 )*
Exp6 ::= ExpUn (Op6 ExpUn)*
ExpUn ::= OpUn ExpUn | ExpAtom
ExpAtom = ExpIdent  | ExpInt     | ExpChar 	     | ExpBool
        | ExpNested | ExpFunCall | ExpEmptyArray | ExpTuple

We parse the list comprehension operator as right associative operator, 
because 1 : 2 : [] should be parsed as 1 : (2 : [])

We parse all other operators as left associative operators. This is
mandatory for - and /, because 1 - 2 - 3 should be interpreted as
(1 - 2) - 3.
It could be that the logical operators are more efficient if they
would be right associative. In other languages (C, Java and mathematics),
these operators are left associative, so we have chosen to stick to that
convention.

We have chosen : to have a  binding power between == and +, because then
we can compare lists and we can make lists of summed integers {like
1 + 2 : [] == [] is ((1 + 2) : []) == [] }. Haskell has the same precedence
of these operators, so in this way we also stick to the conventions.

Interesting cases:
1 < 2 : 3 > 4 : [] == 1 < 2 : 4 > 5 : []
[] == [] : []
[] == ([] : []) :: Bool
([] == []) : [] :: [Bool]
*/

// -- Parse Operators

instance fromString BinOp where
	fromString s = case s of
		"||"	= OpOr
		"&&"	= OpAnd
		"=="	= OpEquals
		"!="	= OpNE
		"<"		= OpLT
		">"		= OpGT
		"<="	= OpLTE
		">="	= OpGTE
		"+"		= OpPlus
		"-"		= OpMinus
		"*"		= OpMult
		"/"		= OpDiv
		"%"		= OpMod
		":"		= OpConcat

instance fromString UnOp where
	fromString s = case s of
		"!" = OpNot
		"-" = OpNeg

op1  :== ["||"]
op2  :== ["&&"]
op3  :== ["==", "<", ">", "<=", ">=", "!="]
op4  :== [":"]
op5  :== ["+", "-"]
op6  :== ["*", "/", "%"]
opUn :== ["!", "-"]

pBinOp :: [String] -> Parser Token BinOp
pBinOp ops = pSatisfyTokenTypeString Operator ops
			>>= \(Token _ op _). return (fromString op)

pUnOp :: [String] -> Parser Token UnOp
pUnOp ops = pSatisfyTokenTypeString Operator ops
			>>= \(Token _ op _). return (fromString op)


pOp1 = pBinOp op1
pOp2 = pBinOp op2
pOp3 = pBinOp op3
pOp4 = pBinOp op4
pOp5 = pBinOp op5
pOp6 = pBinOp op6
pOpUn = pUnOp opUn

// -- Parse Expression

pLeftAssocOps :: (Parser Token Expr) (Parser Token BinOp) -> Parser Token Expr
pLeftAssocOps parseItem parseOp =
	parseItem	>>= \item.
	pMany (	parseOp		>>= \op.
			parseItem	>>= \item.
			return (op,item)		) >>= \list.
	return (makeExp item list)
where
	makeExp item []		= item
	makeExp item [(op,i2):rest]
						= makeExp (ExpBinOp item op i2) rest
						
pRightAssocOps :: (Parser Token Expr) (Parser Token BinOp) -> Parser Token Expr
pRightAssocOps parseItem parseOp =
	parseItem	>>= \item.
		(	(parseOp							>>= \op.
			 pRightAssocOps parseItem parseOp	>>= \item2.
			 return (ExpBinOp item op item2)	)
		<<|>
			(return item)
		)

parseExp :: Parser Token Expr
parseExp = pExp1

pExp1 = pLeftAssocOps pExp2 pOp1
pExp2 = pLeftAssocOps pExp3 pOp2
pExp3 = pLeftAssocOps pExp4 pOp3
pExp4 = pRightAssocOps pExp5 pOp4
pExp5 = pLeftAssocOps pExp6 pOp5
pExp6 = pLeftAssocOps pExpUn pOp6
pExpUn =
		(ExpUnOp <$> pOpUn <*> pExpUn )
	<<|>
		(parseExpAtom)

parseExpAtom :: Parser Token Expr
parseExpAtom = 	parseExpFunCall
		<<|>	parseExpBool // else True and False are parsed as identifiers
		<<|>	parseExpIdent
		<<|>	parseExpInt
		<<|>	parseExpChar
		<<|>	parseExpNested
		<<|>	parseExpArray
		<<|>	parseExpTuple



parseExpIdent :: Parser Token Expr
parseExpIdent = parseIdentWithFields
			>>= (\ident. pYield (ExpIdent ident))

parseExpInt :: Parser Token Expr
parseExpInt = pSatisfyTokenType NumToken
			>>= (\(Token NumToken s _). pYield (ExpInt (toInt s)))
			
parseExpChar :: Parser Token Expr
parseExpChar =  pSatisfyTokenType SingleQuote
			>>| pSatisfy (\(Token type s _) = 
					case type of
						StringToken = (size s == 1)
						_	= False)
			>>= (\(Token StringToken s _). 
				pSatisfyTokenType SingleQuote
			>>| pYield (ExpChar (select s 0)))

parseExpBool :: Parser Token Expr
parseExpBool = pSatisfyStringToken ["True", "False"]
			>>= (\(Token StringToken s _). pYield (ExpBool (if (s == "True") True False)))
			
parseExpNested :: Parser Token Expr
parseExpNested = pSatisfyBrace Open Round
			>>| parseExp
			>>= \exp. pSatisfyBrace Close Round
			>>| pYield exp
			
parseExpFunCall :: Parser Token Expr
parseExpFunCall = parseFunCall
			>>= \funcall. pYield (ExpFunCall funcall)
			
parseExpArray :: Parser Token Expr
parseExpArray = pSatisfyBrace Open Square
				>>| pSatisfyBrace Close Square
				>>| pYield ExpEmptyArray


parseExpTuple :: Parser Token Expr
parseExpTuple = pSatisfyBrace Open Round
			>>| parseExp
			>>= \exp1. pSatisfyTokenType Comma
			>>| parseExp
			>>= \exp2. pSatisfyBrace Close Round
			>>| pYield (ExpTuple exp1 exp2)

// -- Parse Ident and fields

parseIdentWithFields :: Parser Token IdWithFields
parseIdentWithFields = parseId 
			>>= \id. pMany parseField
			>>= \fields.
				pYield (createFieldList (JustId id) fields)
			where
				createFieldList :: IdWithFields [Field] -> IdWithFields
				createFieldList id [] = id
				createFieldList id [field:fields] = createFieldList (WithField id field) fields  

parseField :: Parser Token Field
parseField = pSatisfyTokenType Dot
			>>| pSatisfyStringToken ["hd", "tl", "fst", "snd"]
			>>= (\(Token StringToken s _).
				let t = case s of
						"hd" 	= FieldHd
						"tl" 	= FieldTl
						"fst" 	= FieldFst
						"snd" 	= FieldSnd
				in pYield t
			)

parseId :: Parser Token Id
parseId = pSatisfy (\t. case t of
							(Token StringToken _ _) = True
							(Token _ _ _)		 	= False)
			>>= (\(Token StringToken s _). pYield s)









