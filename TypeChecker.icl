implementation module TypeChecker

import CustomStdEnv
import SPLParser
import Misc
import Error
import Spec

from StdList import and, all, zip
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
from Data.Map import instance Functor (Map k)

:: TypeScheme = TS (Set Id) Type // the type with the bounded variables
:: Subst	:== Id -> Type // Type substitutions assign types to type variables

:: Env = { varTypes		:: 'm'.Map Id Type
		 , funcTypes	:: 'm'.Map Id TypeScheme
		 }

tbi :== toBeImplemented

// Is t1 more specific than t2?
(moreSpecificThan) infix 4 :: Type Type -> Bool
(moreSpecificThan) (IdentType _) (IdentType _) = False
(moreSpecificThan) _			 (IdentType _) = True
(moreSpecificThan) (TupleType t1 t2) (TupleType t1` t2`) = t1 moreSpecificThan t1` && t2 moreSpecificThan t2`
(moreSpecificThan) (ArrayType t) (ArrayType t`) = t moreSpecificThan t`
(moreSpecificThan) (FuncType a1 (Just r1)) (FuncType a2 (Just r2)) =
						(all (\(x1,x2).x1 moreSpecificThan x2) (zip (a1,a2)))
							&& r1 moreSpecificThan r2
(moreSpecificThan) (FuncType a1 Nothing) (FuncType a2 Nothing) =
						(all (\(x1,x2).x1 moreSpecificThan x2) (zip (a1,a2)))
(moreSpecificThan) _ _ = trace_n "(moreSpecificThan) uncomparable types" False //This should never occur in this application


class (^^) infixl 8 a :: a Subst -> a //Map a substitution over a type

// I want to hide the actual types of Subst and Env,
// because I don't know if I need to use the Data.Map type or just
// ordinary functions.

// Hiding the actual substitution types:

idSubst :: Subst //identity
idSubst = \id -> IdentType id

substitute :: Id Type -> Subst // Subsitute Id with Type and leave the rest the same
substitute id type = \i2.if (i2 === id) type (IdentType i2)

(O) infixr 9 :: Subst Subst -> Subst
(O) a b = \id. (b id) ^^ a // The 'o' in the slides is not to be read as pure function composition

instance ^^ Type where
	(^^) type subst = case type of
		IdentType id	= subst id
		BasicType b		= BasicType b
		TupleType t1 t2	= TupleType (t1 ^^ subst) (t2 ^^ subst)
		ArrayType t		= ArrayType (t ^^ subst)
		FuncType args r = FuncType (map (\arg.arg ^^ subst) args) (mapMaybe (\type.type ^^ subst) r)

instance ^^ TypeScheme where
	(^^) (TS boundedVars type) subst = TS boundedVars type` where
		type` = case type of
			IdentType id	= if (member id boundedVars) (IdentType id) (subst id) // Don't substitute the bounded type variables
			BasicType b		= BasicType b
			TupleType t1 t2	= TupleType (t1 ^^ subst) (t2 ^^ subst)
			ArrayType t		= ArrayType (t ^^ subst)
			FuncType args r = FuncType (map (\arg.arg ^^ subst) args) (mapMaybe (\type.type ^^ subst) r)

//instance ^^ Type where
//	(^^) type subst = type`
//		where TS _ type` = (TS [] type) ^^ subst

// Hiding the actual environment type:

newEnv :: Env
newEnv = { varTypes  = 'm'.newMap
		 , funcTypes = 'm'.newMap
		 }

typeOfVar :: Env Id -> Type
typeOfVar {varTypes} id = fromJust ('m'.get id varTypes)

//typeOfVar :: Env Id -> Maybe Type
//typeOfVar {varTypes} id = 'm'.get id varTypes

typeOfFunc :: Env Id -> TypeScheme
typeOfFunc {funcTypes} id = fromJust ('m'.get id funcTypes)

//typeOfFunc :: Env Id -> Maybe TypeScheme
//typeOfFunc {funcTypes} id = 'm'.get id funcTypes

setVarType :: Env Id Type -> Env
setVarType env id type = {env & varTypes = 'm'.put id type env.varTypes}

setFuncType :: Env Id TypeScheme -> Env
setFuncType env id ts = {env & funcTypes = 'm'.put id ts env.funcTypes}

//deleteVar :: Env Id -> Env
//deleteVar env id = {env & varTypes = set_ env.varTypes id (abort "Variable type not in environment")}

set_ f id type :== \id2. if (id === id2) type (f id)

instance ^^ Env where
	(^^) {varTypes,funcTypes} subst =
		{ varTypes  = 'm'.fmap (\t.t ^^ subst) varTypes
		, funcTypes = 'm'.fmap (\ts.ts ^^ subst) funcTypes
		}


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

instance freeVars Env where		//Here you need the map type
	freeVars {varTypes,funcTypes} = union
		(unions (map freeVars ('m'.elems varTypes)))
		(unions (map freeVars ('m'.elems funcTypes)))


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
			, env		:: Env
			, subst		:: Subst
			, label		:: String //labeling for local variables
			}

fail :: MMonad a
fail = M \st.(Nothing,st)

makeFreshVar :: MMonad Id
makeFreshVar = M \st.(Just $ "%var" +++(toString st.counter), {st & counter=st.counter + 1})
// The % is to avoid name clashes with the user defined type variables

makeFreshIdentType :: MMonad Type
makeFreshIdentType = fmap (\id.IdentType id) makeFreshVar

error :: Position String -> MMonad a
error pos msg = M \st.(Nothing, {st & errors = [makeError pos ERROR TypeChecking msg: st.errors]})

getEnv :: MMonad Env
getEnv = M \st.(Just st.env,st)

changeEnv :: (Env -> Env) -> MMonad Env
changeEnv f = M \st.(Just (f st.env),{st & env = f st.env})

getSubst :: MMonad Subst
getSubst = M \st.(Just st.subst, st)

addSubst :: Subst -> MMonad Subst
addSubst s = M \st.let s` = s O st.subst in (Just s`, {st & subst = s`})

setLabel :: String -> MMonad String
setLabel s = M \st.(Just s,{st & label = s})

getLabel :: MMonad String
getLabel = M \st.(Just st.label,st)

mRun :: (MMonad a) -> (Maybe a, [Error])
mRun (M ma) = let (maybeA, st) = ma {counter = 0, errors = [], label = "", env = newEnv, subst = idSubst} in (maybeA, st.errors)

//derived combinators
mUnify :: Type Type -> MMonad Subst // calculate the unification, immediately apply it to the environment and the substitution function.
mUnify t1 t2 = case unify t1 t2 of
	Just subst	=
			changeEnv (\env. env ^^ subst)	>>|
			addSubst subst
	Nothing		= fail

withFreshVar :: (Id -> MMonad a) -> MMonad a
withFreshVar f = makeFreshVar >>= f

withFreshIdentType :: (Type -> MMonad a) -> MMonad a
withFreshIdentType f = makeFreshIdentType >>= f

scope :: String (MMonad a) -> MMonad a
scope s ma =
	setLabel s >>| ma >>= \a.setLabel "" >>| return a

addVarType :: Id Type -> MMonad Type
addVarType id t =
	getLabel	>>= \label.
	changeEnv (\env. {env & varTypes = 'm'.put (label <+++ "@" <+++ id) t env.varTypes})
				>>| return t

addFuncType :: Id TypeScheme -> MMonad TypeScheme
addFuncType id ts =
	getLabel	>>= \label.
	changeEnv (\env. {env & funcTypes = 'm'.put (label <+++ "@" <+++ id) ts env.funcTypes})
				>>| return ts


getVarType :: Id -> MMonad Type
getVarType id =
	getEnv		>>= \env.
	getLabel	>>= \label.
	case 'm'.get (label <+++ "@" <+++ id ) env.varTypes of
		Just t	= return t
		_		= fail

getFuncType :: Id -> MMonad TypeScheme
getFuncType id =
	getEnv		>>= \env.
	getLabel	>>= \label.
	case 'm'.get (label <+++ "@" <+++ id ) env.funcTypes of
		Just t	= return t
		_		= fail

//AMF instances
mAp :: (MMonad (a -> b)) (MMonad a) -> MMonad b
mAp (M mf) (M ma) = M \st.
	case mf st of
		(Just f, st2)	= case ma st2 of
				(Just a, st3)	= (Just (f a), st3)
				(Nothing,st3)	= (Nothing, st3)
		(Nothing,st2)	= case ma st2 of	//Difference with bind: ap does ma st2 even if mf st failed, to be able to collect more errors in the error field of MState
				(Just _, st3)	= (Nothing, st3)
				(Nothing,st3)	= (Nothing, st3)

instance Monad MMonad where
	bind (M ma) f = M \st.
		case ma st of
			(Just a, st2)	= let (M mb) = f a in mb st2
			(Nothing,st2)	= (Nothing,st2)
instance Applicative MMonad where
	pure a = M \st.(Just a,st)
	(<*>) mf ma = mAp mf ma
instance Functor MMonad where
	fmap f ma = mAp (return f) ma

// Two type classes for the actual M function. 
// The first one is to be used if the AST object has a type. The second one is to be used if 
// the AST object does not have a type. (statements, declarations)

// @param env:		The environment built up so far
// @param a:		The AST object to type-check
// @param type:		The current information known from the context, if this AST object has a type
// @return:			An MMonad with:
//					- the AST with updated type information in the metadata
// 					- a substitution such that the object has the type type ^^ subst in env ^^ subst
//					MMonad fails or gives an error if the type inference fails
class match a :: a Type -> MMonad Subst

// @param env:		The environment built up so far
// @param a:		The AST object to type-check
// @return:			An MMonad with:
//					- the AST with updated type information in the metadata
// 					- the environment with the type of the functions, variables added
//					MMonad fails or gives an error if the type inference fails
//class matchN a :: Env a -> MMonad (Env, a)

/*
match :: AST -> (Maybe (Env,Subst), [Error])
match ast = mRun (match` newEnv ast idSubst)
where
	match` :: Env AST Subst -> MMonad (Env,Subst)
	match` env [Fun (FunDecl name args Nothing varDecls stmts _) _:decls] subst =
		makeNewTypeVariablesAndPutInEnvironment name args >>= \(fType, argTypes, env2).
		matchBody varDecls stmts ... >>= \subst.
		???*/

typeInference :: AST -> ((Maybe (AST, Subst)), [Error])
typeInference ast = mRun (matchAST ast)

setType :: MetaData Type -> MetaData //verplaatsen naar SPLParser
setType m t = {m & type = Just t}

matchAST :: AST -> MMonad (AST, Subst)
matchAST [Fun (FunDecl name args Nothing varDecls stmts m1) m2:decls] =
	makeFreshIdentType							>>= \fType.
	setLabel name								>>|	
			forM args (\arg -> freshInEnv arg)	>>|
			matchBody varDecls stmts decls name
	) >>= (varDecls`, stmts`, decls`, subst).
	addFuncType
	return [Fun (FunDecl name args Nothing varDecls` stmts` (setType m1 (fType ^^ subst` ^^ subst))) _:decls]
	

matchAST [Var (VarDecl Nothing name e meta) meta2:decls] =
	makeFreshIdentType					>>= \type.
	match e type						>>= \subst.
	addVarType name (type ^^ subst)		>>|
	matchAST  decls						>>= \(decls`,subst`).
	return	( [Var	(VarDecl Nothing name e (setType meta (type ^^ subst` ^^ subst)))
					(setType meta2 (type ^^ subst` ^^ subst)):decls`]
			, subst` O subst)

matchAST [] = return ([], idSubst)

freshInEnv :: Id -> MMonad Type
freshInEnv arg =
	makeFreshIdentType	>>= \t.
	addVarType arg t

matchBody :: [VarDecl] [Stmt] AST Id -> MMonad ([VarDecl], [Stmt], AST, Subst)
matchBody varDecls stmts decls fname =
	case varDecls of
		[VarDecl Nothing vname e meta:varDecls] =
			makeFreshIdentType							>>= \type.
			match e type								>>= \subst.
			addVarType vname (type ^^ subst)			>>|
			return (setVarType env
						(name +++ "@" +++ name)
						(type ^^ subst)				)		>>= \envWithVar.
			matchBody envWithVar varDecls stmts fname
		[] = case stmts of
			_ = tbi
			[]	= 
				setLabel ""						>>|
				getEnv							>>|
				addFuncType (TS (freeVars) ())	>>|
				matchAST decls		>>= \(ast`, subst`)
				
				return ([], [], ast`, subst`)

matchStmts :: Env [Stmt] -> MMonad Env
matchStmts env [stmt:stmts] =
	matchStmt  env  stmt	>>= \env`.
	matchStmts env` stmts
matchStmts env [] = return env

matchStmt :: Env Stmt -> MMonad Env
matchStmt env stmt = case stmt of
	StmtIf e ifStmts mElse	_ =
		match e (BasicType BoolType)		>>= \subst.
		matchStmts (env ^^ subst) ifStmts		>>= \env2.
		case mElse of
			Nothing			= return env2
			Just elseStmts	= matchStmts env2 elseStmts 
	StmtWhile e body		_ =
		match e (BasicType BoolType)		>>= \subst.
		matchStmts (env ^^ subst) body
	StmtAss idWithFields e	_ =
		tbi
	StmtFunCall funCall		_ =
		tbi
	StmtRet e				_ =
		tbi
	StmtRetV				_ =
		tbi

instance match Expr where
	match _ _ = tbi

instance match IdWithFields where
	match idwf t = tbi/*case idWithFields of
		WithField idwf2 field	= case field of
			..
		JustId id				=
			getVarType env id		>>= \t2.
			mUnify t t2				>>= \subst.*/
/*
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



instance match FunDecl where
	match :: Env FunDecl -> MMonad (Env, FunDecl)
	match env f=:(FunDecl name args (Just fType=:(FuncType argTypes _)) varDecls stmts meta) =
		let env2 = setFuncType env name (TS newSet fType)		in		// Set the specified type in the environment
		let env3 = setArgTypes env2 args argTypes				in		// Set the specified types of the arguments in the environment
		matchT env3 f fType							>>= \(subst,f`).	// Do the type inference for the body, setting the types in the metadata of the AST
		let derivedFuncType = fType ^^ subst					in			
		let derivedArgTypes = map (\at.at ^^ subst) argTypes	in
		if	(derivedFuncType moreSpecificThan fType)			// The derived function type may not be more specific than the specified type
				(error meta.MetaData.pos ("Specified type is to general: derived type is " <+++ derivedFuncType))
				(
					let free_vars = difference (freeVars fType) (freeVars (env ^^ subst)) in
					let fTypeScheme = TS (free_vars) fType	in			// Take the original environment with no types for the function or arguments of the
					let env4 = setFuncType (env ^^ subst) name fTypeScheme	in		// function set and bind all free type variables of the function type in that environment
					return (env4, f`)									// Return the environment, omitting the types of the arguments of the function
				)
				
		where
			setArgTypes :: Env [Id] [Type] -> Env
			setArgTypes env [arg:args] [type:types] = setArgTypes (setVarType env arg type) args types
			setArgTypes env _ 			_			= env
			
	match env f=:(FunDecl name args Nothing varDecls stmts meta) =
		withFreshIdentType								\fresh.			// Make a fresh type variable for the function
		let env2 = setFuncType env name (TS newSet fresh)	in			// Set the type of this function to the new type variable in the environment
		withFreshArgTypes env args					>>= \env3.			// Do the same for all arguments
		matchT env3 f fresh							>>= \(subst, f`).	// Do the type inference for the body, setting the types in the metadata of the AST
		let funcType = fresh ^^ subst						in
		let fTypeScheme = TS (freeVars funcType) funcType	in
		let env4 = setFuncType (env ^^ subst) name fTypeScheme	in
		return (env4, f`)												// Return the environment, omitting the types of the arguments of the function
		where
			withFreshArgTypes :: Env [Id] -> MMonad Env
			withFreshArgTypes env [arg:args]	= withFreshIdentType \type.withFreshArgTypes (setVarType env arg type) args
			withFreshArgTypes env []			= return env

instance matchT FunDecl where
	matchT :: Env FunDecl Type -> MMonad (Subst, FunDecl)
	matchT env f=:(FunDecl name args mType [varDecl:varDecls] stmts meta) t =
		match env varDecl												>>= \(env2, varDecl).
		matchT env2 (FunDecl name args mType varDecls stmts meta) t		>>= \(subst, (FunDecl name2 args2 mType2 varDecls2 stmts2 meta2)).
		let env3 = env2 ^^ subst in
		return (subst, FunDecl name2 args2 mType2 varDecls2 stmts2 meta2)
	matchT env f=:(FunDecl name args mType [] [stmt:stmts] meta) t =
		toBeImplemented

instance match VarDecl where
	match _ _ = toBeImplemented

/*instance match VarDecl where
	match :: Env VarDecl -> MMonad (Env, VarDecl)
	match env v=:(VarDecl mType name expr meta) =
		(case mType of
			(Just t)	= \f.f t
			Nothing		= withFreshIdentType)	\type.	// Make a fresh type if no type is specified
		matchT env v type			>>= \(subst, t2).
		let env2 = env ^^ subst in
		let derivedType = type ^^ subst in
		case mType of
			(Just t)	= if (t2 === t)
								(return (env2, ))
		return (env, v)*/
//match` :: 

/*class getSpecifiedType a :: a -> Maybe Type
instance getSpecifiedType VarDecl
instance getSpecifiedType FunDecl

matchM :: Env a -> MMonad (Subst, a) | matchT, getSpecifiedType a
matchM env a =
	case getSpecifiedType a of
		Just type	=
			..
		Nothing		= withFreshIdentType \id.
						setEnvThenMatch id.
where
	setEnvThenMatch id =
		set*/
	
instance matchT VarDecl where
	matchT :: Env VarDecl Type -> MMonad (Subst, VarDecl)
	matchT env v=:(VarDecl mType name expr meta) type =
		let env2 = setVarType env name type  in
		matchT env2 undef/*expr*/ type
*/









