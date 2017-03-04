definition module SPLParser

import Token
import Misc

parser :: [Token] -> ([(AST, [Token])], [String])

// SPL
:: AST :== [AST_Dec]
// Decl
:: AST_Dec
	= Var AST_Var
	| Fun AST_Fun
// VarDecl
:: AST_Var
	= Var_Gen AST_Id AST_Exp 			// 'var' type
	| Var_Type AST_Type AST_Id AST_Exp 	// typed variable
// FunDecl
:: AST_Fun
	= Fun_Gen AST_Id [AST_Id] [AST_Var] [AST_Stmt]				// generic function
	| Fun_Type AST_Id [AST_Id] AST_FunType [AST_Var] [AST_Stmt]	// typed function (with :: a -> b)
// FunType
:: AST_FunType
	= FunType [AST_Type] AST_Type
	| FunTypeVoid [AST_Type]
// Type
:: AST_Type
	= TypeBasic AST_BasicType
	| TypeTuple AST_Type AST_Type
	| TypeArray [AST_Type]
	| TypeIdent AST_Id
// Basic types
:: AST_BasicType
	= IntType
	| BoolType
	| CharType
// Stmt
:: AST_Stmt
	= StmtIf AST_Exp [AST_Stmt]
	| StmtIfElse AST_Exp [AST_Stmt] [AST_Stmt]
	| StmtWhile AST_Exp [AST_Stmt]
	| StmtAss AST_Ident AST_Exp
	| StmtFun AST_FunCall
	| StmtRet AST_Exp
	| StmtRetV
// Exp
:: AST_Exp
	= ExpIdent AST_Ident
	| ExpBinOp AST_Exp AST_Op2 AST_Exp
	| ExpUnOp AST_Op1 AST_Exp
	| ExpInt Int
	| ExpChar String
	| ExpBool Bool
	| ExpNested AST_Exp
	| ExpFunCall AST_FunCall
	| ExpArray
	| ExpTuple AST_Exp AST_Exp
// FunCall
:: AST_FunCall
	= FunCall AST_Id [AST_Exp]
// Op2
:: AST_Op2
	= Op2Plus
	| Op2Minus
	| Op2Times
	| Op2Divide
	| Op2Modulus
	| Op2Equals
	| Op2LT
	| Op2GT
	| Op2LTE
	| Op2GTE
	| Op2NE
	| Op2And
	| Op2Or
	| Op2Concat
// Op1
:: AST_Op1
	= Op1Not
	| Op1Neg
// id Field
:: AST_Ident
	= Ident AST_Id [AST_Field]
// Field
:: AST_Field
	= FieldHd
	| FieldTl
	| FieldFst
	| FieldSnd
// id
:: AST_Id :== String