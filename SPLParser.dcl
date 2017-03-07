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

:: VarDecl
	= VarDecl (Maybe Type) Id Expr

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
	| ArrayType Type
	| IdentType Id

:: BasicType
	= IntType
	| BoolType
	| CharType

// -- Statement

:: Stmt
	= StmtIf Expr [Stmt] (Maybe [Stmt])
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
	| ExpChar Char
	| ExpBool Bool
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