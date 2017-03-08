implementation module PrettyPrinter

import Indentation
import SPLParser
import Misc

prettyPrint :: a -> String | toShow a
prettyPrint item = show (toShow item)

concat :: [a] Show -> Show | toShow a
concat [] 		_ = zero
concat [a: []] 	_ = toShow a
concat items glue = foldl (glueFunction) (toShow (hd items)) (map (toShow) (tl items))
	where
		glueFunction = \a b. a + glue + b

instance toShow AST where
	toShow []		= zero
	toShow [a: b] 	= toShow a + nl + toShow b
	
instance toShow Decl where
	toShow (Var varDecl)	= toShow varDecl
	toShow (Fun funDecl)	= toShow funDecl
	
instance toShow VarDecl where
	toShow (VarDecl mType id expr) = 
		typeShow + rtrn id + rtrn " = " + toShow expr
	where
		typeShow = case mType of
			(Just type)	= toShow type + rtrn " "
			Nothing		= rtrn "var "
			
instance toShow FunDecl where
	toShow (FunDecl id args mType varDecls stmts) =
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

instance toShow FunType where
	toShow (FunType argTypes type) 	= concat argTypes (rtrn " ") + rtrn " -> " + toShow type
	toShow (FunTypeVoid argTypes)	= concat argTypes (rtrn " ") + rtrn " -> Void"
		
instance toShow Stmt where
	toShow (StmtIf cond stmtA mStmtB) = rtrn "if ( " + toShow cond + rtrn " ) {" + 
		(indentBlock (concat stmtA nl)) + rtrn "}" + stmtB
		where 
			stmtB = case mStmtB of
				(Just stmt) = nl + rtrn "else {" + indentBlock (concat stmt nl) + rtrn "}"
				Nothing		= zero
	toShow (StmtWhile cond stmt) = rtrn "while ( " + toShow cond + rtrn " ) {" +
		(indentBlock (concat stmt nl)) + rtrn "}"
	toShow (StmtAss id expr) = toShow id + rtrn " = " + toShow expr + rtrn ";"
	toShow (StmtFunCall funCall) = toShow funCall + rtrn ";"
	toShow (StmtRet expr) = rtrn "return " + toShow expr + rtrn ";"
	toShow (StmtRetV) = rtrn "return;"

instance toShow [Expr] where
	toShow exprs = foldl (plusWithComma) zero (map (toShow) exprs)
	where
		plusWithComma = \a b. a + rtrn ", " + b

instance toShow Expr where
	toShow (ExpIdent id) = toShow id
	toShow (ExpBinOp exprA op exprB) = rtrn "(" + toShow exprA + toShow op + toShow exprB + rtrn ")"
	toShow (ExpUnOp op exprA) = rtrn "(" + toShow op + toShow exprA + rtrn ")"
	toShow (ExpInt int) = rtrn (toString int)
	toShow (ExpBool bool) = rtrn (toString bool)
	toShow (ExpChar char) = rtrn (toString char)
	toShow (ExpFunCall funCall) = toShow funCall
	toShow (ExpEmptyArray) = rtrn "[]"
	toShow (ExpTuple exprA exprB) = rtrn "(" + toShow exprA + rtrn ", " + toShow exprB + rtrn ")"
	
instance toShow FunCall where
	toShow (FunCall id args) = rtrn id + rtrn "(" + concat args (rtrn ", ") + rtrn ")"

instance toShow Type where
	toShow (BasicType type) 		= toShow type
	toShow (TupleType typeA typeB) 	= rtrn "(" + toShow typeA + rtrn ", " + toShow typeB + rtrn ")"
	toShow (ArrayType type) 		= rtrn "[" + toShow type + rtrn "]"
	toShow (IdentType id)			= rtrn id
	
instance toShow IdWithFields where
	toShow (IdWithFields id fields) = rtrn id + (concat fields (zero))
				
instance toShow Field where
	toShow FieldHd = rtrn ".hd"
	toShow FieldTl = rtrn ".tl"
	toShow FieldFst = rtrn ".fst"
	toShow FieldSnd = rtrn ".snd"

instance toShow BasicType where
	toShow IntType 	= rtrn "Int"
	toShow BoolType = rtrn "Bool"
	toShow CharType = rtrn "Char"

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
