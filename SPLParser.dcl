definition module SPLParser

import Token
import Misc
import Error

parser :: [Token] -> ([(AST, [Token])], [Error])

// -- SPL
:: AST :== [Decl]

:: MetaData = { pos		:: Position
			  , type	:: Maybe Type
			  }

// -- Declarations
:: Decl
	= Var VarDecl
	| Fun FunDecl

:: VarDecl
	= VarDecl (Maybe Type) Id Expr

:: FunDecl
	= FunDecl Id [Arg] (Maybe Type) [VarDecl] [Stmt]
:: Arg :== Id


// -- Types
:: Type
	= BasicType BasicType
	| TupleType Type Type
	| ArrayType Type
	| IdentType Id
	| FuncType [Type] (Maybe Type) // list of arguments and return type

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
	= WithField IdWithFields Field
	| JustId	Id

:: Field
	= FieldHd
	| FieldTl
	| FieldFst
	| FieldSnd

:: Id :== String