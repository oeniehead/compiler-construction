definition module PrettyPrinter

import AST
from Indentation import :: Show

prettyPrint :: a -> String | toShow a

class toShow a 	:: a 	-> Show

instance toShow AST, Decl, VarDecl, FunDecl, Stmt, Expr, FunCall, Type, TypeScheme,
	IdWithFields, Field, BasicType, BinOp, UnOp