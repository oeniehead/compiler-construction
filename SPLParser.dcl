definition module SPLParser

import Token
import Misc
import Error

parser :: [Token] -> ([(AST, [Token])], [Error])

// -- SPL
:: AST :== [Decl]

// -- Declarations
:: Decl
	= Var VarDecl
	| Fun FunDecl

/* oud
:: VarDecl
	= VarTyped   Type Id Expr 	// typed variable
	| VarUntyped      Id Expr 	// 'var' type
*/
:: VarDecl
	= VarDecl (Maybe Type) Id Expr

/* oud
:: FunDecl
	= FunTyped   Id [Id] FunType [VarDecl] [Stmt]		// typed function (with :: a -> b)
	| FunUntyped Id [Id]         [VarDecl] [Stmt]		// without typing information
*/
:: FunDecl
	= FunDecl Id [Arg] (Maybe FunType) [VarDecl] [Stmt]
:: Arg :== Id


// -- Types
:: FunType
	= FunType     [Type] Type
	| FunTypeVoid [Type]

:: Type
	= BasicType BasicType
	| TupleType Type Type
//	| TypeArray [Type] Het is een arraytype, geen array van types! :)
	| ArrayType Type
	| IdentType Id // polymorphic type

:: BasicType
	= IntType
	| BoolType
	| CharType


// -- Statement

:: Stmt
	= StmtIf Expr [Stmt] (Maybe [Stmt]) // If statement with possible else 
	| StmtWhile Expr [Stmt]
	| StmtAss IdWithFields Expr
	| StmtFunCall FunCall
	| StmtRet Expr
	| StmtRetV


// -- Expression

:: Expr
	= ExpIdent IdWithFields
	| ExpBinOp Expr BinOp Expr
	| ExpUnOp UnOp Expr
	| ExpInt Int
	| ExpChar Char // Was ExpChar String
	| ExpBool Bool
	| ExpNested Expr // TODO: deze moet verwijderd worden
	| ExpFunCall FunCall
	| ExpEmptyArray
	| ExpTuple Expr Expr

:: FunCall
	= FunCall Id [Expr]

:: BinOp
	= OpPlus
	| OpMinus
	| OpMult
	| OpDiv
	| OpMod
	| OpEquals
	| OpLT
	| OpGT
	| OpLTE
	| OpGTE
	| OpNE
	| OpAnd
	| OpOr
	| OpConcat

:: UnOp
	= OpNot
	| OpNeg

:: IdWithFields
	= IdWithFields Id [Field]

:: Field
	= FieldHd
	| FieldTl
	| FieldFst
	| FieldSnd

:: Id :== String