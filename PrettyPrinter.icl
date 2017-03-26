implementation module PrettyPrinter

import Indentation
import SPLParser
import Misc

prettyPrint :: a -> String | toShow a
prettyPrint item = show (toShow item)

concat :: [a] Show -> Show | toShow a
concat [] 		_ = zero
concat [a] 		_ = toShow a
concat items glue = foldl (glueFunction) (toShow (hd items)) (map (toShow) (tl items))
	where
		glueFunction = \a b. a + glue + b

instance toShow AST where
	toShow []		= zero
	toShow [a: b] 	= toShow a + nl + toShow b
	
instance toShow Decl where
	toShow (Var varDecl _)	= toShow varDecl
	toShow (Fun funDecl _)	= toShow funDecl
	
instance toShow VarDecl where
	toShow (VarDecl mType id expr _) = 
		typeShow + rtrn id + rtrn " = " + toShow expr + rtrn ";"
	where
		typeShow = case mType of
			(Just type)	= toShow type + rtrn " "
			Nothing		= rtrn "var "
			
instance toShow FunDecl where
	toShow (FunDecl id args mType varDecls stmts _) =
		rtrn id + rtrn "( " + concat args (rtrn ", ") + rtrn " )" + type + nl + rtrn "{" + 
			( indentBlock (
				(concat varDecls nl) + separator + (concat stmts (nl)) 
			)) + rtrn "}" 
	where
		type = case mType of
			(Just type) = rtrn " :: " + toShow type
			Nothing		= zero
		separator = if (length varDecls * length stmts > 0) (nl) (zero)
	
instance toShow Arg where
	toShow a	= rtrn a
		
instance toShow Stmt where
	toShow (StmtIf cond stmtA mStmtB _) = rtrn "if ( " + toShow cond + rtrn " ) {" + 
		(indentBlock (concat stmtA nl)) + rtrn "}" + stmtB
		where 
			stmtB = case mStmtB of
				(Just stmt) = nl + rtrn "else {" + indentBlock (concat stmt nl) + rtrn "}"
				Nothing		= zero
	toShow (StmtWhile cond stmt _) = rtrn "while ( " + toShow cond + rtrn " ) {" +
		(indentBlock (concat stmt nl)) + rtrn "}"
	toShow (StmtAss id expr _) = toShow id + rtrn " = " + toShow expr + rtrn ";"
	toShow (StmtFunCall funCall _) = toShow funCall + rtrn ";"
	toShow (StmtRet expr _) = rtrn "return " + toShow expr + rtrn ";"
	toShow (StmtRetV _) = rtrn "return;"

//	instance toShow [Expr] where
//		toShow exprs = foldl (plusWithComma) zero (map (toShow) exprs)
//		where
//			plusWithComma = \a b. a + rtrn ", " + b //dead code, incorrect because it will start with a comma

instance toShow Expr where
	toShow (ExpIdent id _) = toShow id
	toShow (ExpBinOp exprA op exprB _) = rtrn "(" + toShow exprA + toShow op + toShow exprB + rtrn ")"
	toShow (ExpUnOp op exprA _) = rtrn "(" + toShow op + toShow exprA + rtrn ")"
	toShow (ExpInt int _) = rtrn (toString int)
	toShow (ExpBool bool _) = rtrn (toString bool)
	toShow (ExpChar char _) = rtrn "'" + rtrn (toString char) + rtrn "'"
	toShow (ExpFunCall funCall _) = toShow funCall
	toShow (ExpEmptyArray _) = rtrn "[]"
	toShow (ExpTuple exprA exprB _) = rtrn "(" + toShow exprA + rtrn ", " + toShow exprB + rtrn ")"
	
instance toShow FunCall where
	toShow (FunCall id args _) = rtrn id + rtrn "(" + concat args (rtrn ", ") + rtrn ")"

instance toShow Type where
	toShow type = toShow` type False
	where
		toShow` :: Type Bool -> Show // Bool indicates if brackets might be needed
		toShow` (BasicType type _) _				= toShow type
		toShow` (TupleType typeA typeB _) _		= rtrn "(" + toShow typeA + rtrn ", " + toShow typeB + rtrn ")"
		toShow` (ArrayType type _) _				= rtrn "[" + toShow type + rtrn "]"
		toShow` (IdentType id _) _				= rtrn id
		toShow` funcType				 True	= rtrn "(" + toShow` funcType False + rtrn ")"
		toShow` (FuncType args mType _)	 False	= concatArgs args + rtrn " -> " + 
													case mType of
														Nothing	= rtrn "Void"
														Just t	= toShow` t False
		concatArgs :: [Type] -> Show
		concatArgs []		= rtrn ""
		concatArgs [t]		= toShow` t True
		concatArgs [t:ts]	= toShow` t True + rtrn " " + concatArgs ts

instance toShow IdWithFields where
	toShow (WithField id field _) = (toShow id) + (toShow field)
	toShow (JustId id _) 			= rtrn id
				
instance toShow Field where
	toShow FieldHd = rtrn ".hd"
	toShow FieldTl = rtrn ".tl"
	toShow FieldFst = rtrn ".fst"
	toShow FieldSnd = rtrn ".snd"

instance toShow BasicType where
	toShow (IntType  _)	= rtrn "Int"
	toShow (BoolType _) = rtrn "Bool"
	toShow (CharType _) = rtrn "Char"

instance toShow BinOp where
	toShow OpPlus 	= rtrn " + "
	toShow OpMinus 	= rtrn " - "
	toShow OpMult 	= rtrn " * "
	toShow OpDiv 	= rtrn " / "
	toShow OpMod 	= rtrn " % "
	toShow OpEquals = rtrn " == "
	toShow OpLT 	= rtrn " < "
	toShow OpGT 	= rtrn " > "
	toShow OpLTE 	= rtrn " <= "
	toShow OpGTE 	= rtrn " >= "
	toShow OpNE 	= rtrn " != "
	toShow OpAnd 	= rtrn " && "
	toShow OpOr 	= rtrn " || "
	toShow OpConcat = rtrn " : "
	
instance toShow UnOp where
	toShow OpNot = rtrn "!"
	toShow OpNeg = rtrn "-"
