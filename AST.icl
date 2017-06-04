implementation module AST

import Misc
import CustomStdEnv
from Data.Func import $
from Data.Set import :: Set

instance zero MetaData where
	zero = { pos  = zero
		   , type = Nothing
		   }

setMetaType :: MetaData Type -> MetaData
setMetaType meta type = {meta & type = Just type}

setMetaTS :: MetaDataTS TypeScheme -> MetaDataTS
setMetaTS meta ts = {meta & typeScheme = Just ts}

instance getMeta IdWithFields where
	getMeta (WithField _ _ m) = m
	getMeta (JustId _ m) = m
	
instance getMeta Expr where
	getMeta (ExpIdent _ m) = m
	getMeta (ExpBinOp _ _ _ m) = m
	getMeta (ExpUnOp _ _ m) = m
	getMeta (ExpInt _ m) = m
	getMeta (ExpChar _ m) = m
	getMeta (ExpBool _ m) = m
	getMeta (ExpFunCall _ m) = m
	getMeta (ExpEmptyArray m) = m
	getMeta (ExpTuple _ _ m) = m

instance getMeta Stmt where
	getMeta (StmtIf _ _ _ m)        = m
	getMeta (StmtWhile _ _ m)       = m
	getMeta (StmtAss _ _ m)         = m
	getMeta (StmtFunCall _ m)       = m
	getMeta (StmtRet _ m)           = m
	getMeta (StmtRetV m)            = m


derive gEq		MetaData, MetaDataTS, Decl, VarDecl, FunDecl, Type, TypeScheme, BasicType,
				Stmt, Expr, FunCall, BinOp, UnOp, IdWithFields, Field, Set
derive gString 	MetaData, MetaDataTS, Decl, VarDecl, FunDecl, Type, TypeScheme, BasicType,
				Stmt, Expr, FunCall, BinOp, UnOp, IdWithFields, Field, Set


//instance mapMeta AST where
//	mapMeta f g ast = map (mapMeta f g) ast

instance mapMeta Decl where
	mapMeta f g (Var varDecl)	= Var $ mapMeta f g varDecl
	mapMeta f g (Fun funDecl)	= Fun $ mapMeta f g funDecl
	
instance mapMeta VarDecl where
	mapMeta f g (VarDecl mType id expr m) = VarDecl mType id (mapMeta f g expr) (f m)
			
instance mapMeta FunDecl where
	mapMeta f g (FunDecl id args mType varDecls stmts m) =
		FunDecl id args mType (mapMeta f g varDecls) (mapMeta f g stmts) (g m)
		
instance mapMeta Stmt where
	mapMeta f g (StmtIf cond stmtA mStmtB m)	=
		StmtIf (mapMeta f g cond) (mapMeta f g stmtA) b (f m)
		where
			b = case mStmtB of
					Nothing		= Nothing
					Just stmtB	= Just (mapMeta f g stmtB)
	mapMeta f g (StmtWhile cond stmt m)			= StmtWhile (mapMeta f g cond) (mapMeta f g stmt) (f m)
	mapMeta f g (StmtAss id expr m)				= StmtAss (mapMeta f g id) (mapMeta f g expr) (f m)
	mapMeta f g (StmtFunCall funCall m)			= StmtFunCall (mapMeta f g funCall) (f m)
	mapMeta f g (StmtRet expr m)				= StmtRet (mapMeta f g expr) (f m)
	mapMeta f g (StmtRetV m)					= StmtRetV (f m)

instance mapMeta Expr where
	mapMeta f g (ExpIdent id m)				= ExpIdent (mapMeta f g id) (f m)
	mapMeta f g (ExpBinOp exprA op exprB m)	= ExpBinOp (mapMeta f g exprA) op  (mapMeta f g exprB) (f m)
	mapMeta f g (ExpUnOp op exprA m)		= ExpUnOp op  (mapMeta f g exprA) (f m)
	mapMeta f g (ExpInt int m)				= ExpInt int (f m)
	mapMeta f g (ExpBool bool m)			= ExpBool bool (f m)
	mapMeta f g (ExpChar char m)			= ExpChar char (f m)
	mapMeta f g (ExpFunCall funCall m)		= ExpFunCall (mapMeta f g funCall) (f m)
	mapMeta f g (ExpEmptyArray m)			= ExpEmptyArray (f m)
	mapMeta f g (ExpTuple exprA exprB m)	= ExpTuple (mapMeta f g exprA) (mapMeta f g exprB) (f m)
	
instance mapMeta FunCall where
	mapMeta f g (FunCall id args m) = FunCall id (mapMeta f g args) (f m)

instance mapMeta IdWithFields where
	mapMeta f g (WithField id field m)	= WithField (mapMeta f g id) field (f m)
	mapMeta f g (JustId id m) 			= JustId id (f m)

instance mapMeta [a] | mapMeta a where
	mapMeta f g list = map (mapMeta f g) list


