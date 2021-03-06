definition module AST

import Misc
from Data.Set import :: Set


// -- Types
:: Type
	= BasicType BasicType
	| TupleType Type Type
	| ArrayType Type
	| IdentType Id
	| FuncType [Type] Type // list of arguments and return type
	| VoidType

:: TypeScheme = TS (Set TypeVar) Type // the type with the bounded variables

:: TypeVar  :== Id

:: BasicType
	= IntType
	| BoolType
	| CharType

bIntType  :== BasicType IntType
bBoolType :== BasicType BoolType
bCharType :== BasicType CharType

// -- SPL
:: MetaData = { pos		:: Position
			  , type	:: Maybe Type
			  }

:: MetaDataTS = { pos			:: Position
				, typeScheme	:: Maybe TypeScheme
				}

instance zero MetaData

setMetaType :: MetaData   Type			-> MetaData
setMetaTS	:: MetaDataTS TypeScheme	-> MetaDataTS

class getMeta a :: a -> MetaData
class setMeta a :: a MetaData -> a 

instance getMeta Stmt, Expr, IdWithFields

class mapMeta a :: (MetaData -> MetaData) (MetaDataTS -> MetaDataTS) a -> a

instance mapMeta Decl, VarDecl, FunDecl, Stmt, Expr, FunCall, IdWithFields
instance mapMeta [a] | mapMeta a

:: AST :== [Decl]

// -- Declarations
:: Decl
	= Var VarDecl
	| Fun FunDecl

:: VarDecl
	= VarDecl (Maybe Type) Id Expr		MetaData

:: FunDecl
	= FunDecl Id [Arg] (Maybe Type) [VarDecl] [Stmt]	MetaDataTS
:: Arg :== Id




// -- Statement

:: Stmt
	= StmtIf Expr [Stmt] (Maybe [Stmt])	MetaData //Type zal nothing zijn, want statements
	| StmtWhile Expr [Stmt]				MetaData // hebben geen type
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

derive gEq		MetaData, MetaDataTS, Decl, VarDecl, FunDecl, Type, TypeScheme, BasicType,
				Stmt, Expr, FunCall, BinOp, UnOp, IdWithFields, Field, Set
derive gString 	MetaData, MetaDataTS, Decl, VarDecl, FunDecl, Type, TypeScheme, BasicType,
				Stmt, Expr, FunCall, BinOp, UnOp, IdWithFields, Field, Set
// Note: je kunt nu ook prettyPrint gebruiken

