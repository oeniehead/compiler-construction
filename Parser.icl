implementation module Parser

import ParserCombinators
import Token
import AST
//import Misc
//import CustomStdEnv
from Scanner import scanner

import Control.Applicative
import Control.Monad
from Data.Func import $
import StdArray

// Try to get the current position, return the default of zero if no position found
withPos :: (Position -> Parser Token a) -> Parser Token a
withPos f = (pGetPos >>= f) <<|> (f zero)

withMeta :: (MetaData -> Parser Token a) -> Parser Token a // make a MetaData with current pos and no type information
withMeta f = withPos (\p. f {pos = p, type = Nothing})

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

pMaybe :: (Parser t a) -> Parser t (Maybe a)
pMaybe parse = (Just <$> parse) <<|> (pYield Nothing)

pBetweenBrackets :: BraceStyle (Parser Token a) -> Parser Token a
pBetweenBrackets bstyle parse = pSatisfyBrace Open bstyle *> parse <* pSatisfyBrace Close bstyle

// Skip all tokens not in the list
// Skips all tokens if none of the tokens in the list have been found
skipTill :: [TokenType] ->  Parser Token [Token]
skipTill types = pMany ( pSatisfy \(Token t _ _) -> not (isMember t types) )
	
// Eats incorrect syntax up to and including a certain ending token, like '}' or ';'
// If no ending token has been found, skips all the characters up to EOF
// At least one token needs to be skipped. Skipping no tokens is not seen as the eating of a piece of incorrect syntax
eatIncorrectSyntax :: [TokenType] ->  Parser Token [Token]
eatIncorrectSyntax types =
	(
		skipTill types									>>= \skipped.
		pSatisfy (\(Token t _ _) -> isMember t types)	>>= \ending.
		return (skipped ++ [ending])
	) <<|> (
		skipTill [EOFToken]	>>= \skipped.
		case skipped of
			[]	= empty
			_	= return skipped
	)

// Parse an item, or skip to the token if parsing failed, and deliver Nothing
pWithRecover :: [TokenType] (Parser Token a) -> Parser Token (Either [Token] a)
pWithRecover types p =
	(Right <$> p) <<|> (Left <$> (eatIncorrectSyntax types))

parser :: [Token] -> (Maybe (Bool, AST), [Error])
parser tokens =
	case runParser parseAST tokens of
		([]		, log) = (Nothing, [makeError zero FATAL Parsing "No parse results":log])
		(results, log) = case [r \\ (r, []) <- results] of
			[(ok,ast): _]	= (Just (ok,ast), log)
			_		= (Nothing, [makeError zero FATAL Parsing "No parse results with a fully consumed input":log])

uptoParse :: String ([Error] -> Maybe a) ([Error] -> a) ([Error] -> Maybe a) -> Either a (AST,[Error])
uptoParse prog fscanErrors fparseFail fParserErrors =
	let (tokens,scanErrors) = scanner prog
	in case fscanErrors scanErrors of
		Just a = Left a
		Nothing =
			case parser tokens of
				(Nothing		   , log)	= Left $ fparseFail (scanErrors ++ log)
				(Just (True , ast) , log)	= Right (ast, scanErrors ++ log)
				(Just (False, ast) , log)	=
						case fParserErrors (scanErrors ++ log) of
							Nothing		= Right (ast, scanErrors ++ log)
							Just a		= Left a


/* Parse the declarations in the input
 * Returns:
 *	- Bool: was the parsing totally successfull?
 *	- AST: the declarations that could be parsed successfully
 */
parseAST :: Parser Token (Bool, AST)
parseAST =
	pMany //Here, it is important that pWithRecover will eat at least one token, else pMany will loop indefinately!
		(
			pGetPos		>>= \pos.
			pWithRecover [TerminatorToken, Brace Close Curly] parseDecl >>= \mRes.
			case mRes of
				Left tokens	=
					pLog (makeError pos ERROR Parsing (
						"Error while parsing declaration, skipping up to and including " +++ case last tokens of
							Token _ s p = s +++ "[" +++ (toString p.line) +++ ", " +++ (toString p.col) +++ "]"
						)) >>|
					return Nothing
				Right res			=
					return (Just res)
		)	>>= \mdecls.
		pSatisfyTokenType EOFToken >>|
		return (all isJust mdecls, [ fromJust d \\ d <- mdecls | isJust d])

parseDecl :: Parser Token Decl
parseDecl =
		(
			parseVarDecl >>= \v. pYield (Var v)
		) <<|> (
			parseFunDecl >>= \f. pYield (Fun f)
		)

parseVarDecl :: Parser Token VarDecl
parseVarDecl =  withPos							\pos.
			parseVarType						>>= \vartype.
			parseId								>>= \id.
			pSatisfyTokenType Assignment		>>|
			parseExp							>>= \exp.
			pSatisfyTokenType TerminatorToken	>>|
			return (VarDecl vartype id exp {pos=pos, type=vartype})
where
	parseVarType = (pSatisfyStringToken ["var"] >>| return Nothing)
				   <<|>
				   (Just <$> parseType)

parseFunDecl :: Parser Token FunDecl
parseFunDecl = withPos												\pos.
	parseId														>>= \name.
	pBetweenBrackets Round parseIds								>>= \args.
	pMaybe (pSatisfyTokenType TypeIndicator >>| parseFunType)	>>= \mType.
	pSatisfyBrace Open Curly									>>|
	pMany parseVarDecl											>>= \vardecls.
	pSome parseStmt												>>= \stmts.
	pSatisfyBrace Close Curly									>>|
	return (FunDecl name args mType vardecls stmts {pos=pos, typeScheme=Nothing})
				
parseIds :: Parser Token [Id]
parseIds =
	(
		parseId	>>= \t.
		pMany (	pSatisfyTokenType Comma >>| parseId) >>= \l.
		pYield [t : l]
	) <<|> (
		pYield []
	)

parseFunType :: Parser Token Type
parseFunType =
	pMany parseType					>>= \argTypes.
	pSatisfyTokenType TypeArrow		>>|
	parseType						>>= \retType.
	pYield (FuncType argTypes retType)

parseType :: Parser Token Type
parseType = pVoidType <<|> pBasicType <<|> pTupleType <<|> pArrayType <<|> pIdentType
where
	pVoidType =
		pSatisfyStringToken ["Void"] >>|
		return VoidType
	pBasicType = BasicType <$> parseBasicType
	pTupleType =
		pSatisfyBrace Open Round	>>|
		parseType					>>= \t1.
		pSatisfyTokenType Comma		>>|
		parseType					>>= \t2.
		pSatisfyBrace Close Round	>>|
		return (TupleType t1 t2)
	pArrayType =
		ArrayType <$> pBetweenBrackets Square parseType
	pIdentType = IdentType <$> parseId

parseBasicType :: Parser Token BasicType
parseBasicType =
	pSatisfyStringToken ["Int","Bool","Char"]	>>= \(Token _ string _).
	case string of
		"Int"	= return IntType
		"Bool"	= return BoolType
		"Char"	= return CharType

parseStmt :: Parser Token Stmt
parseStmt = parseStmtIf <<|>
			parseStmtWhile <<|>
			parseStmtAss <<|>
			parseStmtReturn <<|>
			parseStmtFunCall // this one should be at last, else return (1); is interpreted as a function call
where parseStmtFunCall = withMeta \meta.
			StmtFunCall <$> (parseFunCall <* pSatisfyTokenType TerminatorToken) <*> return meta

parseStmtIf :: Parser Token Stmt
parseStmtIf = withMeta										\meta.
			pSatisfyStringToken ["if"]					>>|
			pBetweenBrackets Round parseExp				>>= \exp.
			pBetweenBrackets Curly (pMany parseStmt)	>>= \body.
			pMaybe elsepart								>>= \maybeElseBody.
			pYield (StmtIf exp body maybeElseBody meta)
where
	elsepart =	pSatisfyStringToken ["else"]	>>|
				pBetweenBrackets Curly (pMany parseStmt)
			
parseStmtWhile :: Parser Token Stmt
parseStmtWhile =  withMeta							\meta.
			pSatisfyStringToken ["while"]		>>|
			pSatisfyBrace Open Round			>>|
			parseExp							>>= \exp.
			pSatisfyBrace Close Round			>>|
			pSatisfyBrace Open Curly			>>|
			pMany parseStmt						>>= \stmts.
			pSatisfyBrace Close Curly			>>|
			pYield (StmtWhile exp stmts meta)
			
parseStmtAss :: Parser Token Stmt
parseStmtAss = withMeta 					\meta.
	parseIdentWithFields				>>= \i.
	pSatisfyTokenType Assignment		>>|
	parseExp							>>= \e.
	pSatisfyTokenType TerminatorToken	>>|
	pYield (StmtAss i e meta)
				
parseStmtFun :: Parser Token Stmt
parseStmtFun = withMeta						\meta.
	parseFunCall						>>= \f.
	pSatisfyTokenType TerminatorToken	>>|
	pYield (StmtFunCall f meta)
				
parseStmtReturn :: Parser Token Stmt
parseStmtReturn = withMeta								\meta.
	pSatisfyStringToken ["return"] >>| (
			(	parseExp							>>= \exp.
				pSatisfyTokenType TerminatorToken	>>|
				pYield (StmtRet exp meta)						)	
			<<|>
			(	pSatisfyTokenType TerminatorToken	>>|
				pYield (StmtRetV meta)							)
	)

parseFunCall :: Parser Token FunCall
parseFunCall = withMeta							\meta.
		pSatisfyTokenType StringToken		>>= \(Token StringToken name _).
		pBetweenBrackets Round parseActArgs	>>= \args.
		pYield (FunCall name args meta)

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
			withMeta		\meta.
			parseItem	>>= \item.
			return (op,item,meta)		) >>= \list.
	return (makeExp item list)
where
	makeExp item []		= item
	makeExp item [(op,i2,meta):rest]
						= makeExp (ExpBinOp item op i2 meta) rest
						
pRightAssocOps :: (Parser Token Expr) (Parser Token BinOp) -> Parser Token Expr
pRightAssocOps parseItem parseOp =
	parseItem	>>= \item.
		(	(parseOp							>>= \op.
			 withMeta								\meta.
			 pRightAssocOps parseItem parseOp	>>= \item2.
			 pYield (ExpBinOp item op item2 meta)	)
		<<|>
			(pYield item)
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
		(pOpUn		>>= \op.
		 withMeta		\meta.
		 pExpUn		>>= \expun.
		 return (ExpUnOp op expun meta))
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
parseExpIdent = withMeta			\meta.
		parseIdentWithFields	>>= \ident.
		pYield (ExpIdent ident meta)

parseExpInt :: Parser Token Expr
parseExpInt = withMeta					\meta.
		pSatisfyTokenType NumToken	>>= \(Token NumToken s _).
		pYield (ExpInt (toInt s) meta)
			
parseExpChar :: Parser Token Expr
parseExpChar =  withMeta					\meta.
		pSatisfyTokenType SingleQuote	>>|
		pSatisfy (\(Token type s _) = 
					case type of
						StringToken = (size s == 1)
						_	= False)
										>>= \(Token StringToken s _).
		pSatisfyTokenType SingleQuote	>>|
		pYield (ExpChar (select s 0) meta)

parseExpBool :: Parser Token Expr
parseExpBool = withMeta							\meta.
	pSatisfyStringToken ["True", "False"]	>>= \(Token StringToken s _).
	pYield (ExpBool (s == "True") meta)
			
parseExpNested :: Parser Token Expr
parseExpNested = pSatisfyBrace Open Round
			>>| parseExp
			>>= \exp. pSatisfyBrace Close Round
			>>| pYield exp
			
parseExpFunCall :: Parser Token Expr
parseExpFunCall = withMeta		\meta.
		parseFunCall		>>= \funcall.
		pYield (ExpFunCall funcall meta)
			
parseExpArray :: Parser Token Expr
parseExpArray = withMeta \meta.
				pSatisfyBrace Open Square
			>>| pSatisfyBrace Close Square
			>>| pYield (ExpEmptyArray meta)


parseExpTuple :: Parser Token Expr
parseExpTuple = withMeta					\meta.
			pSatisfyBrace Open Round	>>| 
			parseExp					>>= \exp1.
			pSatisfyTokenType Comma		>>|
			parseExp					>>= \exp2.
			pSatisfyBrace Close Round	>>|
			pYield (ExpTuple exp1 exp2 meta)

// -- Parse Ident and fields

parseIdentWithFields :: Parser Token IdWithFields
parseIdentWithFields = withMeta 	\meta.
			parseId				>>= \id.
			pMany parseField	>>= \fields.
			return (createFieldList (JustId id meta) fields meta)
			where
				createFieldList :: IdWithFields [Field] MetaData -> IdWithFields
				createFieldList id [] meta = id
				createFieldList id [field:fields] meta = createFieldList (WithField id field meta) fields meta

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









