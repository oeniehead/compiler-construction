implementation module SPLParser

import Simple.Core
import Token
import Misc
//import Error

import Control.Applicative
import Control.Monad
	
pSatisfyTokenType :: TokenType -> Parser Token Token
pSatisfyTokenType type = pSatisfy (\(Token other_type _ _). type == other_type)
	
pSatisfyTokenTypeString :: TokenType [String] -> Parser Token Token
pSatisfyTokenTypeString type strings = pSatisfy (\(Token other_type string _). (type == other_type) && (isMember string strings))

pSatisfyBrace :: BraceType BraceStyle -> Parser Token Token
pSatisfyBrace btype bstyle = pSatisfy (\(Token type _ _) = 
					case type of
						(Brace other_btype other_bstyle) = other_btype == btype && other_bstyle == bstyle 
						_	= False)


parser :: [Token] -> ([(AST, [Token])], [String])
parser tokens = runParser parseAST tokens

//parse2op :: [Token] -> ([(Expr, [Token])], [Error])
//parse2op tokens = runParser parseAST_Exp_Int tokens

/*
expTokens :: [Token]
expTokens =  [
		(Token NumToken "1" zero),
		(Token Operator "+" zero),
		(Token NumToken "2" zero),
		(Token Operator "*" zero),
		(Token NumToken "3" zero),
		(Token Operator "+" zero),
		(Token NumToken "4" zero),
		(Token EOFToken "" zero)]
		
expIdent :: [Token]
expIdent = [
		(Token StringToken "Fred123" zero),
		(Token Dot "." zero),
		(Token StringToken "hd" zero),
		(Token Dot "." zero),
		(Token StringToken "tl" zero),
		(Token Dot "." zero),
		(Token StringToken "snd" zero),
		(Token Dot "." zero),
		(Token StringToken "snd" zero),
		(Token Dot "." zero),
		(Token StringToken "fst" zero),
		(Token EOFToken "" zero)]



Start = runParser parseIdent expIdent*/

parseAST :: Parser Token AST
parseAST = pMany parseDecl

parseDecl :: Parser Token Decl
parseDecl = (
					parseVarDecl >>= \v. pYield (VarDecl v)
				) <<|> (
					parseFunDecl >>= \f. pYield (FunDecl f)
				)

parseVarDecl :: Parser Token VarDecl
parseVarDecl = (
						pSatisfyTokenTypeString StringToken ["var"]
					>>| parseId
					>>= \i. pSatisfyTokenType Assignment
					>>| parseAST_Exp
					>>= \e. pSatisfyTokenType TerminatorToken
					>>| pYield (VarUntyped i e)
				) <<|> (
						parseType
					>>= \t.parseId
					>>= \i. pSatisfyTokenType Assignment
					>>| parseAST_Exp
					>>= \e. pSatisfyTokenType TerminatorToken
					>>| pYield (VarTyped t i e)
				)

parseFunDecl :: Parser Token FunDecl
parseFunDecl = parseId
				>>= \i. pSatisfyBrace Open Round
				>>| parseIds
				>>= \a. ((
							pSatisfyTokenType TypeIndicator
						>>| parseFunType
						>>= \t. parseFunBody
											// Id Args Type Vars Stmts
						>>= \(v, s). pYield (FunTyped i a t v s)
					) <<|> (
							parseFunBody
											// Id Args Vars Stmts
						>>= \(v, s). pYield (FunUntyped i a v s)
				)) 
				
parseFunBody :: Parser Token ([VarDecl], [Stmt])
parseFunBody = 	pSatisfyBrace Open Curly
				>>| pMany parseVarDecl
				>>= \v. pSome parseStmt
				>>= \s. pYield (v, s)
				
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

parseFunType :: Parser Token FunType
parseFunType = pMany parseType
				>>= \t. pSatisfyTokenType TypeArrow
				>>| ((
						pSatisfyTokenTypeString StringToken ["Void"]
					>>| pYield (FunTypeVoid t)) <<|> (
						parseType
					>>= \r. pYield (FunType t r))
				)

parseType :: Parser Token Type
parseType = parseBasicType >>= \t. pYield (TypeBasic t) <<|>
				(
						pSatisfyBrace Open Round
					>>| parseType
					>>= \a. pSatisfyTokenType Comma
					>>| parseType
					>>= \b. pSatisfyBrace Close Round
					>>| pYield (TypeTuple a b)
				) <<|>
				(
						pSatisfyBrace Open Square
					>>| pMany parseType
					>>= \a. pSatisfyBrace Close Square
					>>| pYield (TypeArray a)
				) <<|>
				(
						parseId
					>>= \i. pYield (TypeIdent i)
				) 

parseBasicType :: Parser Token BasicType
parseBasicType = (pSatisfyTokenTypeString StringToken ["Int"] >>| pYield (IntType)) <<|>
					(pSatisfyTokenTypeString StringToken ["Bool"] >>| pYield (BoolType)) <<|>
					(pSatisfyTokenTypeString StringToken ["Char"] >>| pYield (CharType))

parseStmt :: Parser Token Stmt
parseStmt = parseStmtIf <<|>
				parseStmtWhile <<|>
				parseStmtAss <<|> // TODO: Maar 4 statements hier?!
				parseStmtReturn
				
parseStmtIf :: Parser Token Stmt
parseStmtIf = pSatisfyTokenTypeString StringToken ["if"]
			>>| pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \c. pSatisfyBrace Close Round
			>>| pSatisfyBrace Open Curly
			>>| pMany parseStmt
			>>= \a. pSatisfyBrace Close Curly
			>>| ((
					pSatisfyTokenTypeString StringToken ["else"]
				>>| pSatisfyBrace Open Curly
				>>| pMany parseStmt
				>>= \b. pSatisfyBrace Close Curly
				>>| pYield (StmtIfElse c a b)) <<|> (
					pYield (StmtIf c a)
				)
			)
			
parseStmtWhile :: Parser Token Stmt
parseStmtWhile =  pSatisfyTokenTypeString StringToken ["while"]
			>>| pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \c. pSatisfyBrace Close Round
			>>| pSatisfyBrace Open Curly
			>>| pMany parseStmt
			>>= \a. pSatisfyBrace Close Curly
			>>| pYield (StmtWhile c a)
			
parseStmtAss :: Parser Token Stmt
parseStmtAss = parseIdent
				>>= \i. pSatisfyTokenTypeString Operator ["="]
				>>| parseAST_Exp
				>>= \e. pSatisfyTokenType TerminatorToken
				>>| pYield (StmtAss i e)
				
parseStmtFun :: Parser Token Stmt
parseStmtFun = parseFunCall
				>>= \f. pSatisfyTokenType TerminatorToken
				>>| pYield (StmtFun f)
				
parseStmtReturn :: Parser Token Stmt
parseStmtReturn = pSatisfyTokenTypeString StringToken ["return"]
				>>| ((
						parseAST_Exp
					>>= \e. pSatisfyTokenType TerminatorToken
					>>| pYield (StmtRet e)) <<|> (
						pYield (StmtRetV)
					)
				)				

parseFunCall :: Parser Token FunCall
parseFunCall = pSatisfyTokenType StringToken
			>>= \(Token StringToken name _). pSatisfyBrace Open Round
			>>| pMany parseAST_Exp
			>>= \args. pSatisfyBrace Close Round
			>>| pYield (FunCall name args)

/*
	Dit blok parseert alle expressies. Eerst worden de binaire operatoren geprobeerd,
	daarna de unaire operatoren, en daarna wordt er teruggevallen op de overige expressies.
*/

/*
Operators in increasing binding power: (oud)
Op1  ::=  ||
Op2  ::=  &&
Op3  ::=  == | < | > | <= | >= | !=
Op4  ::=  * | / | %
Op5  ::=  :
OpUn ::=  ! | -

Adjusted grammar: (oud)
Exp  ::= Exp1
Exp1 ::= Exp2  [Op1 Exp1]
Exp2 ::= Exp3  [Op2 Exp2]
Exp3 ::= Exp4  [Op3 Exp3]
Exp4 ::= Exp5  [Op4 Exp4]
Exp5 ::= ExpUn [Op5 Exp5]
ExpUn ::= [Op1] ExpAtom
ExpAtom = ExpIdent  | ExpInt     | ExpChar | ExpBool
        | ExpNested | ExpFunCall | Array   | Tuple

Operators in increasing binding power:
Op1  r ::=   ||
Op2  ::=  &&
Op3  ::=  == | < | > | <= | >= | !=
Op4  ::=  + | -
Op5  ::=  * | / | %
Op6  ::=  :
OpUn ::=  ! | -
r and l mean right resp. left associative
3 different - : binary, unary and part of int

Adjusted grammar:
Exp   ::= Exp1
Exp1  ::= Exp2  [Op1 Exp1]
Exp2  ::= Exp3  [Op2 Exp2]
Exp3  ::= Exp4  [Op3 Exp3]
Exp4  ::= Exp5  [Op4 Exp4]
Exp5  ::= ExpUn [Op5 Exp5]
ExpUn ::= [Op1] ExpAtom
ExpAtom = ExpIdent  | ExpInt     | ExpChar | ExpBool
        | ExpNested | ExpFunCall | Array   | Tuple

*/

parseAST_Exp :: Parser Token Expr
parseAST_Exp = parseAST_Exp1

parseAST_Exp_Level :: (Parser Token Expr) (Parser Token Op2) (Parser Token Expr) -> Parser Token Expr
parseAST_Exp_Level parse_a parse_op parse_b = (
						parse_a 
					>>=	\lhs. parse_op
					>>= \op2. parse_b
					>>= \rhs. pYield (ExpBinOp lhs op2 rhs)
				) <<|> (
						parse_a
					>>= \exp. pYield exp
				)

parseAST_Exp1 :: Parser Token Expr
parseAST_Exp1 = parseAST_Exp_Level parseAST_Exp2 parseAST_Op2_1 parseAST_Exp1

parseAST_Exp2 :: Parser Token Expr
parseAST_Exp2 = parseAST_Exp_Level parseAST_Exp3 parseAST_Op2_2 parseAST_Exp2

parseAST_Exp3 :: Parser Token Expr
parseAST_Exp3 = parseAST_Exp_Level parseAST_Exp4 parseAST_Op2_3 parseAST_Exp3

parseAST_Exp4 :: Parser Token Expr
parseAST_Exp4 = parseAST_Exp_Level parseAST_Exp5 parseAST_Op2_4 parseAST_Exp4

parseAST_Exp5 :: Parser Token Expr
parseAST_Exp5 = parseAST_Exp_Level parseAST_Exp2 parseAST_Op2_5 parseAST_Exp5

parseAST_Exp_Un :: Parser Token Expr
parseAST_Exp_Un = (
						parseAST_Op1
					>>= \op1. parseAST_Exp_Atom
					>>= \rhs. pYield (ExpUnOp op1 rhs)
				) <<|> (
						parseAST_Exp_Atom
					>>= \exp. pYield exp
				)
				
parseAST_Exp_Atom :: Parser Token Expr
parseAST_Exp_Atom = 	parseAST_Exp_Ident
				<<|>	parseAST_Exp_Int
				<<|>	parseAST_Exp_Char
				<<|>	parseAST_Exp_Bool
				<<|>	parseAST_Exp_Nested
				<<|>	parseAST_Exp_FunCall
				<<|>	parseAST_Exp_Array
				<<|>	parseAST_Exp_Tuple

parseAST_Op2_1 :: Parser Token Op2
parseAST_Op2_1 = pSatisfyTokenTypeString Operator ["||"]
			>>= (\(Token StringToken s _).
				let t = case s of
						"||" 	= Op2Or
				in pYield t
			)

parseAST_Op2_2 :: Parser Token Op2
parseAST_Op2_2 = pSatisfyTokenTypeString Operator ["&&"]
			>>= (\(Token Operator s _).
				let t = case s of
						"&&" 	= Op2And
				in pYield t
			)

parseAST_Op2_3 :: Parser Token Op2
parseAST_Op2_3 = pSatisfyTokenTypeString Operator ["==", "<", ">", "<=", ">=", "!="]
			>>= (\(Token Operator s _).
				let t = case s of
						"==" 	= Op2Equals
						"<" 	= Op2LT
						">" 	= Op2GT
						"<=" 	= Op2LTE
						">=" 	= Op2GTE
						"!=" 	= Op2NE
				in pYield t
			)
			
parseAST_Op2_4 :: Parser Token Op2
parseAST_Op2_4 = pSatisfyTokenTypeString Operator ["*", "/", "%"]
			>>= (\(Token Operator s _).
				let t = case s of
						"*" 	= Op2Times
						"/" 	= Op2Divide
						"%" 	= Op2Modulus
				in pYield t
			)
			
parseAST_Op2_5 :: Parser Token Op2
parseAST_Op2_5 = pSatisfyTokenTypeString Operator [":"]
			>>= (\(Token Operator s _).
				let t = case s of
						":" 	= Op2Concat
				in pYield t
			)


parseAST_Op2 :: Parser Token Op2
parseAST_Op2 = pSatisfyTokenTypeString Operator ["+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "!=", "&&", "||", ":"]
			>>= (\(Token Operator s _).
				let t = case s of
						"+" 	= Op2Plus
						"-" 	= Op2Minus
						"*" 	= Op2Times
						"/" 	= Op2Divide
						"%" 	= Op2Modulus
						"==" 	= Op2Equals
						"<" 	= Op2LT
						">" 	= Op2GT
						"<=" 	= Op2LTE
						">=" 	= Op2GTE
						"!=" 	= Op2NE
						"&&" 	= Op2And
						"||" 	= Op2Or
						":" 	= Op2Concat
				in pYield t
			)

parseAST_Op1 :: Parser Token Op1
parseAST_Op1 = pSatisfyTokenTypeString Operator ["!", "-"]
			>>= (\(Token StringToken s _).
				let t = case s of
						"!" 	= Op1Not
						"-" 	= Op1Neg
				in pYield t
			)

parseIdent :: Parser Token IdWithFields
parseIdent = parseId 
			>>= \ast_id. pMany parseField
			>>= \ast_fields.
				pYield (IdWithFields ast_id ast_fields)

parseField :: Parser Token Field
parseField = pSatisfyTokenType Dot
			>>| pSatisfyTokenTypeString StringToken ["hd", "tl", "fst", "snd"]
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

parseAST_Exp_Ident :: Parser Token Expr
parseAST_Exp_Ident = parseIdent
			>>= (\ident. pYield (ExpIdent ident))

parseAST_Exp_Int :: Parser Token Expr
parseAST_Exp_Int = pSatisfyTokenType NumToken // parse -
			>>= (\(Token NumToken s _). pYield (ExpInt (toInt s)))
			
parseAST_Exp_Char :: Parser Token Expr
parseAST_Exp_Char = pSatisfy (\(Token type s _) = 
					case type of
						StringToken = (size s == 1)
						_	= False)
			>>= (\(Token StringToken s _). pYield (ExpChar s))
			
parseAST_Exp_Bool :: Parser Token Expr
parseAST_Exp_Bool = pSatisfyTokenTypeString StringToken ["True", "False"]
			>>= (\(Token StringToken s _). pYield (ExpBool (if (s == "True") True False)))
			
parseAST_Exp_Nested :: Parser Token Expr
parseAST_Exp_Nested = pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \exp. pSatisfyBrace Close Round
			>>| pYield (ExpNested exp)
			
parseAST_Exp_FunCall :: Parser Token Expr
parseAST_Exp_FunCall = parseFunCall
			>>= \funcall. pYield (ExpFunCall funcall)
			
parseAST_Exp_Array :: Parser Token Expr
parseAST_Exp_Array = pSatisfyBrace Open Square
			>>| pSatisfyBrace Close Square
			>>| pYield ExpArray

parseAST_Exp_Tuple :: Parser Token Expr
parseAST_Exp_Tuple = pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \exp1. pSatisfyTokenType Comma
			>>| parseAST_Exp
			>>= \exp2. pSatisfyBrace Close Round
			>>| pYield (ExpTuple exp1 exp2)
			

			












			
