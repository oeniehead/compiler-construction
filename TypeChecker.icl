implementation module TypeChecker

import CustomStdEnv
import SPLParser
import Misc
import Error
import Spec

from StdList import and
from Data.Func import $
import Data.Set
import Data.Either
import Data.Maybe
from StdMaybe import mapMaybe
import Data.Functor
import Control.Applicative
import Control.Monad
import StdDebug

import qualified Data.Map as m

:: TypeScheme = TS (Set Id) Type // the type with the bounded variables
:: Env = { varTypes		:: Id -> Type
		 , funcTypes	:: Id -> Type
		 }
:: Subst	:== Id -> Type // Type substitutions assign types to type variables

// Is t1 less specific than or as specific as t2?
(leq) infix 4 :: Type Type -> Bool
(leq) t1 t2 = toBeImplemented

class (^^) infixr 8 a :: a Subst -> a //Map a substitution over a type

// I want to hide the actual types of Subst and Env,
// because I don't know if I need to use the Data.Map type or just
// ordinary functions.

// Hiding the actual substitution types:

instance ^^ Type where
	(^^) type subst = case type of
		IdentType id	= subst id
		BasicType b		= BasicType b
		TupleType t1 t2	= TupleType (t1 ^^ subst) (t2 ^^ subst)
		ArrayType t		= ArrayType (t ^^ subst)
		FuncType args r = FuncType (map (\arg.arg ^^ subst) args) (mapMaybe (\type.type ^^ subst) r)

// Hiding the actual environment type:

instance ^^ Env where
	(^^) {varTypes,funcTypes} subst =
		{ varTypes  = \id.(varTypes  id) ^^ subst
		, funcTypes = \id.(funcTypes id) ^^ subst
		}

typeOfVar :: Env Id -> Type
typeOfVar {varTypes} id = varTypes id

typeOfFunc :: Env Id -> Type
typeOfFunc {funcTypes} id = funcTypes id

setVarType :: Env Id Type -> Env
setVarType env id type = {env & varTypes = set_ env.varTypes id type}

setFuncType :: Env Id Type -> Env
setFuncType env id type = {env & funcTypes = set_ env.funcTypes id type}

//deleteVar :: Env Id -> Env
//deleteVar env id = {env & varTypes = set_ env.varTypes id (abort "Variable type not in environment")}

set_ f id type :== \id2. if (id === id2) type (f id)

substitute :: Id Type -> Subst // Subsitute Id with Type and leave the rest the same
substitute id type = \i2.if (i2 === id) type (IdentType i2)

idSubst :: Subst //identity
idSubst = \id -> IdentType id

(O) infixr 9 :: Subst Subst -> Subst
(O) a b = \id. (b id) ^^ a // The 'o' in the slides is not to be read as pure function composition

// Determining the free variables (TV in the slides)
class freeVars a :: a -> Set Id
instance freeVars Type where
	freeVars type = case type of
		BasicType _					= newSet
		TupleType t1 t2				= union (freeVars t1) (freeVars t2)
		ArrayType t					= freeVars t
		IdentType id				= singleton id
		FuncType argTypes retType	= union x (unions (map freeVars argTypes))
			where
				x = case retType of
					Just type	= freeVars type
					Nothing		= newSet

instance freeVars TypeScheme where
	freeVars (TS boundedVars type) = difference (freeVars type) boundedVars

instance freeVars Env where
	freeVars _ = toBeImplemented


// --
// -- Unification
// --
unify :: Type Type -> Maybe Subst
unify (BasicType b1) (BasicType b2)			= if (b1 === b2) (Just idSubst) Nothing
unify (TupleType t1 t2) (TupleType t1` t2`)	=
	unify t1 t1`								>>= \subst.
	unify (t2 ^^ subst) (t2` ^^ subst)	>>= \subst2.
	return (subst2 O subst)
unify (ArrayType t1) (ArrayType t2)			= unify t1 t2
// IdentType cases
unify (IdentType i) (BasicType b)			= Just $ substitute i (BasicType b)
unify (IdentType i1) (IdentType i2)			= Just $ substitute i1 (IdentType i2)
unify (IdentType i) t	= if (member i (freeVars t))
									(/*trace_n (i <+++ "is a member of" <+++ t)*/ Nothing)
									(Just (substitute i t))
// IdentType in the other argument is symmetric:
unify t (IdentType i)						= unify (IdentType i) t
/*unify t (IdentType i)	= if (member i (freeVars t))
									Nothing
									(Just (substitute i t))*/
unify f1=:(FuncType _ _) f2=:(FuncType _ _) = uFuncType f1 f2 idSubst
where
	uFuncType (FuncType [a:as] mr1) (FuncType [b:bs] mr2) prevSubst = // a helper function with the substitution until now
		unify (a ^^ prevSubst) (b ^^ prevSubst)		>>= \subst.
		let newSubst = subst O prevSubst in
		(uFuncType (FuncType as mr1) (FuncType bs mr2) newSubst)
	uFuncType (FuncType [] mr1) (FuncType [] mr2) prevSubst =
		case (mr1,mr2) of
		(Just _ , Nothing)	= Nothing
		(Nothing, Just _ )	= Nothing
		(Nothing, Nothing)	= Just prevSubst
		(Just r1, Just r2)	= unify (r1 ^^ prevSubst) (r2 ^^ prevSubst) >>= \subst.
				return (subst O prevSubst)
	uFuncType _ _ _ = Nothing
//unify _				_						= Nothing
unify a b = /*trace_n ("cannot unify (" <+++ a <+++ ") and (" <+++ b <+++ ")" )*/ Nothing

instance toString Type where toString t = gString{|*|} t

prop_unify :: Type Type -> Bool
prop_unify type1 type2 = case unify type1 type2 of
	Nothing		= /*trace_n ("could not unify " +++ (toString type1) +++ " and " +++ (toString type2))*/ False
	Just subst	= //trace_n ((trace_ type1 subst) +++ ", " +++ (trace_ type2 subst))
			(type1 ^^ subst === type2 ^^ subst)

//trace_ type subst :== "subst (" +++ (toString type) +++ ") = (" +++ (toString (subst type)) +++ ")"
(<+++) infixl 9 :: !String !a -> String | toString a
(<+++) str x = str +++ toString x

Start = runTests unify_tests
unify_tests =
	[ Testcase "\nSame Types:" $
		assert $ and [prop_unify x x \\ y <- [IntType, BoolType, CharType]
									  , x <- [BasicType y, TupleType a b, ArrayType a,
											  a, FuncType [a,b] Nothing, FuncType [] (Just a)]
					 ]
	, Testcase "\nDifferent IdentType:" $
		assert $ prop_unify a b
	, Testcase "\nUnifiable basic types:" $
		assert $ not $ prop_unify (BasicType IntType) (BasicType BoolType)
	, Testcase "\nUnifiable composite types:" $
		assert $ prop_unify comp1 comp2
	, Testcase "\nUnunifiable composite types:" $
		assert $ not $ prop_unify comp1 compBad
	, Testcase "\nFunctype Return and no return:" $
		assert $ not $ prop_unify compRet comp1
	, Testcase "\nFunctype nr of arguments wrong:" $
		assert $ not $ prop_unify compArg comp1
	]
where
	a = IdentType "a"
	b = IdentType "b"
	c = IdentType "c"
	d = IdentType "d"
	e = IdentType "e"
	f = IdentType "f"
	compBad	= FuncType [TupleType a a, ArrayType c				, e] (Just (BasicType IntType))
	comp1	= FuncType [TupleType a a, c						, e] (Just (BasicType IntType))
	comp2	= FuncType [b,			 ArrayType (ArrayType d)	, b] (Just f)
	compArg	= FuncType [TupleType a a, c						   ] (Just (BasicType IntType))
	compRet	= FuncType [TupleType a a, c						, e] Nothing

// --
// -- The Match function (M in the slides)
// --

// A monad for the M function
:: MMonad a = M (MState -> (Maybe a, MState))
:: MState = { counter	:: Int
			, errors	:: [Error]
			}

mBind :: (MMonad a) (a -> MMonad b) -> MMonad b
mBind (M ma) f = M \st.
	case ma st of
		(Just a, st2)	= let (M mb) = f a in mb st2
		(Nothing,st2)	= (Nothing,st2)

mAp :: (MMonad (a -> b)) (MMonad a) -> MMonad b
mAp (M mf) (M ma) = M \st.
	case mf st of
		(Just f, st2)	= case ma st2 of
				(Just a, st3)	= (Just (f a), st3)
				(Nothing,st3)	= (Nothing, st3)
		(Nothing,st2)	= case ma st2 of	//Difference with bind: ap does ma st2 even if mf st failed, to be able to collect collect more errors in the error field of MState
				(Just _, st3)	= (Nothing, st3)
				(Nothing,st3)	= (Nothing, st3)

fail :: MMonad a
fail = M \st.(Nothing,st)

error :: Position String -> MMonad a
error pos msg = M \st.(Nothing, {st & errors = [makeError pos ERROR TypeChecking msg: st.errors]})

makeFreshVar :: MMonad Id
makeFreshVar = M \st.(Just $ (toString st.counter) +++ "var", {st & counter=st.counter + 1})
// Door verse type variabelen te beginnen met een cijfer, voorkom je interferentie
// met de naamgeving die in het programma wordt gebruikt(ik geef toe, het is een beetje een hack)

withFreshVar :: (Id -> MMonad a) -> MMonad a
withFreshVar f = makeFreshVar >>= f

mUnify :: Type Type -> MMonad Subst
mUnify t1 t2 = case unify t1 t2 of
	Just subst	= return subst
	Nothing		= fail

mRun :: (MMonad a) -> (Maybe a, [Error])
mRun (M ma) = let (maybeA, st) = ma {counter = 0, errors = []} in (maybeA, st.errors)

//AMF instances
instance Monad MMonad where
	bind a f = mBind a f
instance Applicative MMonad where
	pure a = return a
	(<*>) mf ma = mAp mf ma
instance Functor MMonad where
	fmap f ma = mAp (return f) ma


// The actual M function
// @param Env:		The environment built up so far
// @param a:		The part from the AST to type-check
// @param Type:		The current information known from the context
// @return:			An MMonad with either
// 					- a substitution and the AST with updated type information
//					- 
class matchT a :: Env a Type -> MMonad (Subst, a)

// A version of match for when you don't have any information about the type
class match a :: Env a -> MMonad (Subst, a)

instance matchT Expr where
	matchT env expr t = case expr of
		ExpIdent idWithFields	meta = toBeImplemented
		ExpBinOp e1 binOp e2	meta = toBeImplemented
		ExpUnOp unOp e			meta = toBeImplemented
		ExpInt i				meta =
			mUnify t (BasicType IntType)	>>= \subst.
			return (subst, ExpInt  i (setMetaType meta (BasicType IntType )))
		ExpChar c				meta = 
			mUnify t (BasicType CharType)	>>= \subst.
			return (subst, ExpChar c (setMetaType meta (BasicType CharType)))
		ExpBool b				meta = toBeImplemented
		ExpFunCall funCall		meta = toBeImplemented
		ExpEmptyArray			meta = toBeImplemented
		ExpTuple e1 e2			meta = toBeImplemented

//instance matchT FunCall where

//instance matchT IdWithFields where

matchFunDecl :: Env FunDecl -> MMonad (Env, FunDecl)
matchFunDecl env f=:(FunDecl name args (Just fType=:(FuncType argTypes _)) varDecls stmts meta) =
	let env2 = setFuncType env name fType				in			// Set the specified type in the environment
	let env3 = setArgTypes env2 args argTypes			in			// Set the specified types of the arguments in the environment
	matchT env3 f fType							>>= \(subst,f`).	// Do the type inference for the body, setting the types in the metadata of the AST
	let derivedFuncType = fType ^^ subst				in			
	let derivedArgTypes = map (\at.at ^^ subst) argTypes	in
	if	(derivedFuncType === fType && derivedArgTypes === argTypes)		// The specified types may not be changed during traversal,
																		// else the function type defined is too general
			(return (env2, f`))										// Return the environment, omitting the types of the arguments of the function
			(error meta.MetaData.pos ("Specified type is to general: derived type is " <+++ derivedFuncType))
	where
		setArgTypes :: Env [Id] [Type] -> Env
		setArgTypes env [arg:args] [type:types] = setArgTypes (setVarType env arg type) args types
		setArgTypes env _ 			_			= env
matchFunDecl env f=:(FunDecl name args Nothing varDecls stmts meta) =
	withFreshVar								\fresh.				// Make a fresh type variable for the function
	let env2 = setFuncType env name (IdentType fresh)	in			// Set the type of this function to the new type variable in the environment
	withFreshVars_ env args					>>= \env3.				// Do the same for all arguments
	matchT env3 f (IdentType fresh)			>>= \(subst, f`).		// Do the type inference for the body, setting the types in the metadata of the AST
	return (env2, f`)												// Return the environment, omitting the types of the arguments of the function
	where
		withFreshVars_ :: Env [Id] -> MMonad Env
		withFreshVars_ env [arg:args] = withFreshVar \type.withFreshVars_ (setVarType env arg (IdentType type)) args
		withFreshVars_ env []		  = return env

instance matchT FunDecl where
	matchT :: Env FunDecl Type -> MMonad (Subst, FunDecl)
	matchT env f=:(FunDecl name args mType [varDecl:varDecls] stmts meta) t =
		matchVarDecl env varDecl										>>= \(env2, varDecl).
		matchT env2 (FunDecl name args mType varDecls stmts meta) t		>>= \(subst, (FunDecl name2 args2 mType2 varDecls2 stmts2 meta2)). // The updated type information
		let env3 = env2 ^^ subst in
		return (subst, FunDecl name2 args2 mType2 varDecls2 stmts2 meta2)
	matchT env f=:(FunDecl name args mType [] [stmt:stmts] meta) t =
		toBeImplemented

	
matchVarDecl :: Env VarDecl -> MMonad (Env, VarDecl)
matchVarDecl _ _ = toBeImplemented













