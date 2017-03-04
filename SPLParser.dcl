definition module SPLParser

import Token
import Misc

parser :: [Token] -> ([(AST, [Token])], [String])

// -- SPL
:: AST :== [Decl]

// -- Declarations
:: Decl
	= VarDecl VarDecl
	| FunDecl FunDecl

:: VarDecl
	= VarTyped   Type Id Expr 	// typed variable
	| VarUntyped      Id Expr 	// 'var' type
/* Suggestie :
:: VarDecl
	= VarDecl (Maybe Type) Id Expr
*/

:: FunDecl
	= FunTyped   Id [Id] FunType [VarDecl] [Stmt]		// typed function (with :: a -> b)
	| FunUntyped Id [Id]         [VarDecl] [Stmt]		// without typing information
/* Suggestie :
:: FunDecl
	= FunTyped   Id [Arg] (Maybe FunType) [VarDecl] [Stmt]
:: Arg :== Id
*/


// -- Types

:: FunType
	= FunType     [Type] Type
	| FunTypeVoid [Type]
/* Suggestie :
:: FunType
	= FunType [Type] (Maybe Type) // List of argument types and return type (if not Void)
*/

:: Type
	= TypeBasic BasicType
	| TypeTuple Type Type
	| TypeArray [Type] //TODO: Veranderen naar TypeArray Type. Het is een arraytype, geen array van types! :)
	| TypeIdent Id

:: BasicType
	= IntType
	| BoolType
	| CharType


// -- Statement

:: Stmt
	= StmtIf Expr [Stmt]
	| StmtIfElse Expr [Stmt] [Stmt] // Hier zou je ook een maybe kunnen gebruiken
	| StmtWhile Expr [Stmt]
	| StmtAss IdWithFields Expr
	| StmtFun FunCall
	| StmtRet Expr // Kan ook met maybe
	| StmtRetV


// -- Expression

:: Expr
	= ExpIdent IdWithFields
	| ExpBinOp Expr Op2 Expr
	| ExpUnOp Op1 Expr
	| ExpInt Int
	| ExpChar String // TODO: veranderen naar ExpChar Char?
	| ExpBool Bool
	| ExpNested Expr // TODO: deze moet verwijderd worden
	| ExpFunCall FunCall
	| ExpArray
	| ExpTuple Expr Expr

:: FunCall
	= FunCall Id [Expr]

:: Op2
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

:: Op1
	= Op1Not
	| Op1Neg

:: IdWithFields
	= IdWithFields Id [Field]

:: Field
	= FieldHd
	| FieldTl
	| FieldFst
	| FieldSnd

:: Id :== String