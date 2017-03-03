implementation module SPLParser

import Text.Parsers.Simple.Core
import Token
import Misc

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


//parser :: [Token] -> ([(AST, [Token])], [Error])
//parser tokens = runParser parseSPL tokens

parse2op :: [Token] -> ([(AST_Exp, [Token])], [Error])
parse2op tokens = runParser parseAST_Exp_Int tokens

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



Start = runParser parseAST_Ident expIdent

parseAST :: Parser Token AST
parseAST = pMany parseAST_Dec

parseAST_Dec :: Parser Token AST_Dec
parseAST_Dec = (
					parseAST_Var >>= \v. pYield (Var v)
				) <<|> (
					parseAST_Fun >>= \f. pYield (Fun f)
				)

parseAST_Var :: Parser Token AST_Var
parseAST_Var = (
						pSatisfyTokenTypeString StringToken ["var"]
					>>| parseAST_Id
					>>= \i. pSatisfyTokenType Assignment
					>>| parseAST_Exp
					>>= \e. pSatisfyTokenType TerminatorToken
					>>| pYield (Var_Gen i e)
				) <<|> (
						parseAST_Type
					>>= \t.parseAST_Id
					>>= \i. pSatisfyTokenType Assignment
					>>| parseAST_Exp
					>>= \e. pSatisfyTokenType TerminatorToken
					>>| pYield (Var_Type t i e)
				)

parseAST_Fun :: Parser Token AST_Fun
parseAST_Fun = parseAST_Id
				>>= \i. pSatisfyBrace Open Round
				>>| parseAST_Ids
				>>= \a. ((
							pSatisfyTokenType TypeIndicator
						>>| parseAST_FunType
						>>= \t. parseAST_Block
											// Id Args Type Vars Stmts
						>>= \(v, s). pYield (Fun_Type i a t v s)
					) <<|> (
							parseAST_Block
											// Id Args Vars Stmts
						>>= \(v, s). pYield (Fun_Gen i a v s)
				)) 
				
parseAST_Block :: Parser Token ([AST_Var], [AST_Stmt])
parseAST_Block = 	pSatisfyBrace Open Curly
				>>| pMany parseAST_Var
				>>= \v. pSome parseAST_Stmt
				>>= \s. pYield (v, s)
				
parseAST_Ids :: Parser Token [AST_Id]
parseAST_Ids = (
						parseAST_Id
					>>= \t. pMany (
							pSatisfyTokenType Comma
						>>| parseAST_Id)
					>>= \l. pYield [t : l]
				) <<|> (
					pYield []
				)

parseAST_FunType :: Parser Token AST_FunType
parseAST_FunType = pMany parseAST_Type
				>>= \t. pSatisfyTokenType TypeArrow
				>>| ((
						pSatisfyTokenTypeString StringToken ["Void"]
					>>| pYield (FunTypeVoid t)) <<|> (
						parseAST_Type
					>>= \r. pYield (FunType t r))
				)

parseAST_Type :: Parser Token AST_Type
parseAST_Type = parseAST_BasicType >>= \t. pYield (TypeBasic t) <<|>
				(
						pSatisfyBrace Open Round
					>>| parseAST_Type
					>>= \a. pSatisfyTokenType Comma
					>>| parseAST_Type
					>>= \b. pSatisfyBrace Close Round
					>>| pYield (TypeTuple a b)
				) <<|>
				(
						pSatisfyBrace Open Square
					>>| pMany parseAST_Type
					>>= \a. pSatisfyBrace Close Square
					>>| pYield (TypeArray a)
				) <<|>
				(
						parseAST_Id
					>>= \i. pYield (TypeIdent i)
				) 

parseAST_BasicType :: Parser Token AST_BasicType
parseAST_BasicType = (pSatisfyTokenTypeString StringToken ["Int"] >>| pYield (IntType)) <<|>
					(pSatisfyTokenTypeString StringToken ["Bool"] >>| pYield (BoolType)) <<|>
					(pSatisfyTokenTypeString StringToken ["Char"] >>| pYield (CharType))

parseAST_Stmt :: Parser Token AST_Stmt
parseAST_Stmt = parseAST_Stmt_If <<|>
				parseAST_Stmt_While <<|>
				parseAST_Stmt_Ass <<|>
				parseAST_Stmt_Ret
				
parseAST_Stmt_If :: Parser Token AST_Stmt
parseAST_Stmt_If = pSatisfyTokenTypeString StringToken ["if"]
			>>| pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \c. pSatisfyBrace Close Round
			>>| pSatisfyBrace Open Curly
			>>| pMany parseAST_Stmt
			>>= \a. pSatisfyBrace Close Curly
			>>| ((
					pSatisfyTokenTypeString StringToken ["else"]
				>>| pSatisfyBrace Open Curly
				>>| pMany parseAST_Stmt
				>>= \b. pSatisfyBrace Close Curly
				>>| pYield (StmtIfElse c a b)) <<|> (
					pYield (StmtIf c a)
				)
			)
			
parseAST_Stmt_While :: Parser Token AST_Stmt
parseAST_Stmt_While =  pSatisfyTokenTypeString StringToken ["while"]
			>>| pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \c. pSatisfyBrace Close Round
			>>| pSatisfyBrace Open Curly
			>>| pMany parseAST_Stmt
			>>= \a. pSatisfyBrace Close Curly
			>>| pYield (StmtWhile c a)
			
parseAST_Stmt_Ass :: Parser Token AST_Stmt
parseAST_Stmt_Ass = parseAST_Ident
				>>= \i. pSatisfyTokenTypeString Operator ["="]
				>>| parseAST_Exp
				>>= \e. pSatisfyTokenType TerminatorToken
				>>| pYield (StmtAss i e)
				
parseAST_Stmt_Fun :: Parser Token AST_Stmt
parseAST_Stmt_Fun = parseAST_FunCall
				>>= \f. pSatisfyTokenType TerminatorToken
				>>| pYield (StmtFun f)
				
parseAST_Stmt_Ret :: Parser Token AST_Stmt
parseAST_Stmt_Ret = pSatisfyTokenTypeString StringToken ["return"]
				>>| ((
						parseAST_Exp
					>>= \e. pSatisfyTokenType TerminatorToken
					>>| pYield (StmtRet e)) <<|> (
						pYield (StmtRetV)
					)
				)				

parseAST_FunCall :: Parser Token AST_FunCall
parseAST_FunCall = pSatisfyTokenType StringToken
			>>= \(Token StringToken name _). pSatisfyBrace Open Round
			>>| pMany parseAST_Exp
			>>= \args. pSatisfyBrace Close Round
			>>| pYield (FunCall name args)

/*
	Dit blok parseert alle expressies. Eerst worden de binaire operatoren geprobeerd,
	daarna de unaire operatoren, en daarna wordt er teruggevallen op de overige expressies.
*/
parseAST_Exp :: Parser Token AST_Exp
parseAST_Exp = parseAST_Exp1

parseAST_Exp_Level :: (Parser Token AST_Exp) (Parser Token AST_Op2) (Parser Token AST_Exp) -> Parser Token AST_Exp
parseAST_Exp_Level parse_a parse_op parse_b = (
						parse_a 
					>>=	\lhs. parse_op
					>>= \op2. parse_b
					>>= \rhs. pYield (ExpBinOp lhs op2 rhs)
				) <<|> (
						parse_a
					>>= \exp. pYield exp
				)

parseAST_Exp1 :: Parser Token AST_Exp
parseAST_Exp1 = parseAST_Exp_Level parseAST_Exp2 parseAST_Op2_1 parseAST_Exp1

parseAST_Exp2 :: Parser Token AST_Exp
parseAST_Exp2 = parseAST_Exp_Level parseAST_Exp3 parseAST_Op2_2 parseAST_Exp2

parseAST_Exp3 :: Parser Token AST_Exp
parseAST_Exp3 = parseAST_Exp_Level parseAST_Exp4 parseAST_Op2_3 parseAST_Exp3

parseAST_Exp4 :: Parser Token AST_Exp
parseAST_Exp4 = parseAST_Exp_Level parseAST_Exp5 parseAST_Op2_4 parseAST_Exp4

parseAST_Exp5 :: Parser Token AST_Exp
parseAST_Exp5 = parseAST_Exp_Level parseAST_Exp2 parseAST_Op2_5 parseAST_Exp5

parseAST_Exp_Un :: Parser Token AST_Exp
parseAST_Exp_Un = (
						parseAST_Op1
					>>= \op1. parseAST_Exp_Atom
					>>= \rhs. pYield (ExpUnOp op1 rhs)
				) <<|> (
						parseAST_Exp_Atom
					>>= \exp. pYield exp
				)
				
parseAST_Exp_Atom :: Parser Token AST_Exp
parseAST_Exp_Atom = 	parseAST_Exp_Ident
				<<|>	parseAST_Exp_Int
				<<|>	parseAST_Exp_Char
				<<|>	parseAST_Exp_Bool
				<<|>	parseAST_Exp_Nested
				<<|>	parseAST_Exp_FunCall
				<<|>	parseAST_Exp_Array
				<<|>	parseAST_Exp_Tuple

parseAST_Op2_1 :: Parser Token AST_Op2
parseAST_Op2_1 = pSatisfyTokenTypeString Operator ["||"]
			>>= (\(Token StringToken s _).
				let t = case s of
						"||" 	= Op2Or
				in pYield t
			)

parseAST_Op2_2 :: Parser Token AST_Op2
parseAST_Op2_2 = pSatisfyTokenTypeString Operator ["&&"]
			>>= (\(Token Operator s _).
				let t = case s of
						"&&" 	= Op2And
				in pYield t
			)

parseAST_Op2_3 :: Parser Token AST_Op2
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
			
parseAST_Op2_4 :: Parser Token AST_Op2
parseAST_Op2_4 = pSatisfyTokenTypeString Operator ["*", "/", "%"]
			>>= (\(Token Operator s _).
				let t = case s of
						"*" 	= Op2Times
						"/" 	= Op2Divide
						"%" 	= Op2Modulus
				in pYield t
			)
			
parseAST_Op2_5 :: Parser Token AST_Op2
parseAST_Op2_5 = pSatisfyTokenTypeString Operator [":"]
			>>= (\(Token Operator s _).
				let t = case s of
						":" 	= Op2Concat
				in pYield t
			)


parseAST_Op2 :: Parser Token AST_Op2
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

parseAST_Op1 :: Parser Token AST_Op1
parseAST_Op1 = pSatisfyTokenTypeString Operator ["!", "-"]
			>>= (\(Token StringToken s _).
				let t = case s of
						"!" 	= Op1Not
						"-" 	= Op1Neg
				in pYield t
			)

parseAST_Ident :: Parser Token AST_Ident
parseAST_Ident = parseAST_Id 
			>>= \ast_id. pMany parseAST_Field
			>>= \ast_fields.
				pYield (Ident ast_id ast_fields)

parseAST_Field :: Parser Token AST_Field
parseAST_Field = pSatisfyTokenType Dot
			>>| pSatisfyTokenTypeString StringToken ["hd", "tl", "fst", "snd"]
			>>= (\(Token StringToken s _).
				let t = case s of
						"hd" 	= FieldHd
						"tl" 	= FieldTl
						"fst" 	= FieldFst
						"snd" 	= FieldSnd
				in pYield t
			)

parseAST_Id :: Parser Token AST_Id
parseAST_Id = pSatisfy (\t. case t of
							(Token StringToken _ _) = True
							(Token _ _ _)		 	= False)
			>>= (\(Token StringToken s _). pYield s)

parseAST_Exp_Ident :: Parser Token AST_Exp
parseAST_Exp_Ident = parseAST_Ident
			>>= (\ident. pYield (ExpIdent ident))

parseAST_Exp_Int :: Parser Token AST_Exp
parseAST_Exp_Int = pSatisfyTokenType NumToken
			>>= (\(Token NumToken s _). pYield (ExpInt (toInt s)))
			
parseAST_Exp_Char :: Parser Token AST_Exp
parseAST_Exp_Char = pSatisfy (\(Token type s _) = 
					case type of
						StringToken = (size s == 1)
						_	= False)
			>>= (\(Token StringToken s _). pYield (ExpChar s))
			
parseAST_Exp_Bool :: Parser Token AST_Exp
parseAST_Exp_Bool = pSatisfyTokenTypeString StringToken ["True", "False"]
			>>= (\(Token StringToken s _). pYield (ExpBool (if (s == "True") True False)))
			
parseAST_Exp_Nested :: Parser Token AST_Exp
parseAST_Exp_Nested = pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \exp. pSatisfyBrace Close Round
			>>| pYield (ExpNested exp)
			
parseAST_Exp_FunCall :: Parser Token AST_Exp
parseAST_Exp_FunCall = parseAST_FunCall
			>>= \funcall. pYield (ExpFunCall funcall)
			
parseAST_Exp_Array :: Parser Token AST_Exp
parseAST_Exp_Array = pSatisfyBrace Open Square
			>>| pSatisfyBrace Close Square
			>>| pYield ExpArray

parseAST_Exp_Tuple :: Parser Token AST_Exp
parseAST_Exp_Tuple = pSatisfyBrace Open Round
			>>| parseAST_Exp
			>>= \exp1. pSatisfyTokenType Comma
			>>| parseAST_Exp
			>>= \exp2. pSatisfyBrace Close Round
			>>| pYield (ExpTuple exp1 exp2)
			

			












			
