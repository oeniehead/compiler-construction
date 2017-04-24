definition module SPLParser

import Token
import Misc
import Error
from Data.Either import :: Either
import GenEq
import GenString

parser :: [Token] -> Either [Error] AST

// -- SPL
:: AST :== [Decl]

:: MetaData = { pos		:: Position
			  , type	:: Maybe Type
			  }

instance zero MetaData

setMetaType :: MetaData Type -> MetaData

// -- Declarations
:: Decl
	= Var VarDecl		MetaData
	| Fun FunDecl		MetaData

:: VarDecl
	= VarDecl (Maybe Type) Id Expr		MetaData

:: FunDecl
	= FunDecl Id [Arg] (Maybe Type) [VarDecl] [Stmt]	MetaData
:: Arg :== Id


// -- Types
:: Type
	= BasicType BasicType
	| TupleType Type Type
	| ArrayType Type
	| IdentType Id
	| FuncType [Type] (Maybe Type) // list of arguments and return type(voor verslag)

:: BasicType
	= IntType
	| BoolType
	| CharType

bIntType  :== BasicType IntType
bBoolType :== BasicType BoolType
bCharType :== BasicType CharType

// -- Statement

:: Stmt
	= StmtIf Expr [Stmt] (Maybe [Stmt])	MetaData
	| StmtWhile Expr [Stmt]				MetaData
	| StmtAss IdWithFields Expr			MetaData
	| StmtFunCall FunCall				MetaData
	| StmtRet Expr						MetaData
	| StmtRetV							MetaData


// -- Expression

:: Expr
	= ExpIdent IdWithFields		MetaData
	| ExpBinOp Expr BinOp Expr	MetaData
	| ExpUnOp UnOp Expr			MetaData
	| ExpInt Int				MetaData
	| ExpChar Char				MetaData
	| ExpBool Bool				MetaData
	| ExpFunCall FunCall		MetaData
	| ExpEmptyArray				MetaData
	| ExpTuple Expr Expr		MetaData

:: FunCall
	= FunCall Id [Expr]		MetaData

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
	= WithField IdWithFields Field		MetaData
	| JustId	Id						MetaData

:: Field
	= FieldHd
	| FieldTl
	| FieldFst
	| FieldSnd

:: Id :== String

derive gEq		MetaData, Decl, VarDecl, FunDecl, Type, BasicType,
				Stmt, Expr, FunCall, BinOp, UnOp, IdWithFields, Field
derive gString 	MetaData, Decl, VarDecl, FunDecl, Type, BasicType,
				Stmt, Expr, FunCall, BinOp, UnOp, IdWithFields, Field


