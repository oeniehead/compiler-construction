implementation module TypeChecker

import CustomStdEnv
from Token import :: Token
from Parser import parser
import AST
import Misc
import Error
import Spec
import PrettyPrinter
import BindingAnalysis

from StdList import and, all, zip, ++, hd, isMember, zip2
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
:: Map k a :== 'm'.Map k a

:: Subst	:== TypeVar -> Type // Type substitutions assign types to type variables

:: Env = { varTypes		:: Map Id Type
		 , funcTypes	:: Map Id TypeScheme
		 }

instance toString Type			where toString t = prettyPrint t
instance toString TypeScheme	where toString t = prettyPrint t

class (^^) infixl 8 a :: a Subst -> a //Map a substitution over a type

// I want to hide the actual types of Subst and Env,
// because I don't know if I need to use the Data.Map type or just
// ordinary functions.

// Hiding the actual substitution types:

idSubst :: Subst //identity
idSubst = \id -> IdentType id

substitute :: TypeVar Type -> Subst // Subsitute Id with Type and leave the rest the same
substitute id type = \i2.if (i2 === id) type (IdentType i2)

(O) infixr 9 :: Subst Subst -> Subst
(O) a b = \id. (b id) ^^ a // The 'o' in the slides is not to be read as pure function composition

instance ^^ Type where
	(^^) type subst = case type of
		IdentType id	= subst id
		BasicType b		= BasicType b
		TupleType t1 t2	= TupleType (t1 ^^ subst) (t2 ^^ subst)
		ArrayType t		= ArrayType (t ^^ subst)
		FuncType args r = FuncType (map (\arg.arg ^^ subst) args) (r ^^ subst)
		VoidType		= VoidType

instance ^^ TypeScheme where
	(^^) (TS boundedVars type) subst = TS boundedVars (transform type) where
		transform type = case type of
			IdentType id	= if (member id boundedVars) (IdentType id) (subst id) // Don't substitute the bounded type variables
			BasicType b		= BasicType b
			TupleType t1 t2	= TupleType (transform t1) (transform t2)
			ArrayType t		= ArrayType (transform t)
			FuncType args r = FuncType (map transform args) (transform r)
			VoidType		= VoidType

// Hiding the actual environment type:

// TODO: clean this up
injectUtilities :: Map Id TypeScheme
injectUtilities = let
			map = 'm'.newMap
			map2 = 'm'.put ("print") (TS (fromList ["a"]) (FuncType [IdentType "a"] VoidType)) map
			map3 = 'm'.put ("read") (TS (fromList ["a"]) (FuncType [] (IdentType "a"))) map2
			map4 = 'm'.put ("isEmpty") (TS (fromList ["a"]) (FuncType [ArrayType (IdentType "a")] (BasicType BoolType))) map3
		in map4

newEnv :: Env
newEnv = { varTypes  = 'm'.newMap
		 , funcTypes = injectUtilities//'m'.newMap
		 }

setVarType :: Env Id Type -> Env
setVarType env id type = {env & varTypes = 'm'.put id type env.varTypes}

setFuncType :: Env Id TypeScheme -> Env
setFuncType env id ts = {env & funcTypes = 'm'.put id ts env.funcTypes}

instance ^^ Env where
	(^^) {varTypes,funcTypes} subst =
		{ varTypes  = 'm'.fmap (\t.t ^^ subst) varTypes
		, funcTypes = 'm'.fmap (\ts.ts ^^ subst) funcTypes
		}


// Determining the free variables (TV in the slides)
class freeVars a :: a -> Set TypeVar
instance freeVars Type where
	freeVars type = case type of
		BasicType _					= newSet
		TupleType t1 t2				= union (freeVars t1) (freeVars t2)
		ArrayType t					= freeVars t
		IdentType id				= singleton id
		FuncType argTypes retType	= unions (map freeVars [retType:argTypes])
		VoidType					= newSet

instance freeVars TypeScheme where
	freeVars (TS boundedVars type) = difference (freeVars type) boundedVars

instance freeVars Env where		//Here you need the map type
	freeVars {varTypes,funcTypes} = union
		(unions (map freeVars ('m'.elems varTypes)))
		(unions (map freeVars ('m'.elems funcTypes)))

// --
// -- Renamings
// --

:: Renaming :== Map TypeVar TypeVar

// Gives a renaming r of type variables such that r t1 = t2, if possible
class rename a :: a a -> Maybe Renaming
instance rename Type where
	rename t1 t2 = rename` t1 t2 'm'.newMap

instance rename TypeScheme where
	rename (TS bounded1 t1) (TS bounded2 t2) =
		renameAll
			[IdentType t \\ t <- toList bounded1]
			[IdentType t \\ t <- toList bounded2] 'm'.newMap
		>>= \e2.
		rename` t1 t2 e2

rename` :: Type Type Renaming -> Maybe Renaming
rename` (BasicType b1) (BasicType b2) env = if (b1 === b2) (Just env) Nothing 
rename` (TupleType t1 v1) (TupleType t2 v2) env =
	rename` t1 t2 env	>>= \e2. rename` v1 v2 e2
rename` (ArrayType t1) (ArrayType t2) env = rename` t1 t2 env
rename` (IdentType id1) (IdentType id2) env =
	case 'm'.get id1 env of
		Just id2`	= if (id2 == id2`) (Just env) Nothing
		Nothing		=
			if (isMember id2 ('m'.elems env))
				Nothing
				(Just ('m'.put id1 id2 env))
rename` (FuncType argTypes1 rType1) (FuncType argTypes2 rType2) env =
	rename` rType1 rType2 env >>= \e2.
	renameAll argTypes1 argTypes2 e2
rename` VoidType VoidType env = Just env
rename` _ _ env = Nothing

renameAll :: [Type] [Type] Renaming -> Maybe Renaming
renameAll [t1:t1s] [t2:t2s] env =
	rename` t1 t2 env >>= \e2.
		renameAll t1s t2s e2
renameAll [] [] env = Just env
renameAll _ _ env = Nothing

// Are t1 and t2 isomorphic? (That is, equivalent up to the renaming of type variables)
isomorphic :: a a -> Bool | rename a
isomorphic a b = isJust (rename a b)

// --
// -- Unification
// --
unify :: Type Type -> Maybe Subst
unify (BasicType b1) (BasicType b2)			= if (b1 === b2) (Just idSubst) Nothing
unify (TupleType t1 t2) (TupleType t1` t2`)	= unifyAll [(t1, t1`), (t2, t2`)]
unify (ArrayType t1) (ArrayType t2)			= unify t1 t2
// IdentType cases
unify (IdentType i) (BasicType b)			= Just $ substitute i (BasicType b)
unify (IdentType i1) (IdentType i2)			= Just $ substitute i1 (IdentType i2)
unify (IdentType i) t	= if (member i (freeVars t))
									(Nothing)
									(Just (substitute i t))
// IdentType in the other argument is symmetric:
unify t (IdentType i)						= unify (IdentType i) t
unify (FuncType a1 r1) (FuncType a2 r2)
| length a1 == length a2	= unifyAll [(r1, r2) : zip2 a1 a2]
| otherwise					= Nothing
unify VoidType		VoidType				= Just idSubst
unify _				_						= Nothing

unifyAll :: [(Type, Type)] -> Maybe Subst
unifyAll types =
	foldM 
		(\subst (t1,t2).
			unify (t1 ^^ subst) (t2 ^^ subst)	>>= \newSubst.
			return (newSubst O subst)
		)
		idSubst
		types

// Is there a substitution s such that t1 is isomorphic to t2 ^^ s?
class (instanceOf) infix 4 a :: a a -> Bool
instance instanceOf Type where
	(instanceOf) t1 t2 = case unify t1 t2 of
		Nothing	= False
		Just s	= isomorphic t1 (t2 ^^ s)

instance instanceOf TypeScheme where
	(instanceOf) (TS bounded1 t1) (TS bounded2 t2) = case unify t1 t2 of
		Nothing	= False
		Just s	= isomorphic (TS bounded1 t1) ((TS bounded2 t2) ^^ s)

instanceOf_tests =
	[ Testcase "a b [(b,int)] -> ([a], (b,int)) instanceOf a b [c] -> ([a], c)" $
		assert $ specificType instanceOf generalType
	, Testcase "A.a:a b [(b,int)] -> ([a], (b,int)) instanceOf A.a:a b [c] -> ([a], c)" $
		assert $ specificTypeScheme instanceOf generalTypeScheme
	, Testcase "" $ assert $
			(TS (fromList ["%var0"]) $ FuncType [IdentType "%var0"] (IdentType "%var0"))
			instanceOf
			(TS (fromList ["a"]) $ FuncType [IdentType "a"] (IdentType "a"))
	]
where
	specificType =
		FuncType [ IdentType "a", IdentType "b", ArrayType (TupleType (IdentType "b") bIntType)]
				(TupleType
					(ArrayType (IdentType "a"))
					(TupleType (IdentType "b") bIntType)
				)
	generalType =
			FuncType [ IdentType "a", IdentType "b", ArrayType (IdentType "c")]
					(TupleType
						(ArrayType (IdentType "a"))
						(IdentType "c")
					)
	specificTypeScheme	= TS (fromList ["a"]) specificType
	generalTypeScheme	= TS (fromList ["a"]) generalType
		

// Testing unification

prop_unify :: Type Type -> Bool
prop_unify type1 type2 = case unify type1 type2 of
	Nothing		= False
	Just subst	= (type1 ^^ subst === type2 ^^ subst)

unify_tests =
	[ Testcase "Same Types:" $
		assert $ and [prop_unify x x \\ y <- [IntType, BoolType, CharType]
									  , x <- [BasicType y, TupleType a b, ArrayType a,
											  a, FuncType [a,b] VoidType, FuncType [] a]
					 ]
	, Testcase "Different IdentType:" $
		assert $ prop_unify a b
	, Testcase "Unifiable basic types:" $
		assert $ not $ prop_unify (BasicType IntType) (BasicType BoolType)
	, Testcase "Unifiable composite types:" $
		assert $ prop_unify comp1 comp2
	, Testcase "Ununifiable composite types:" $
		assert $ not $ prop_unify comp1 compBad
	, Testcase "Functype Return and no return:" $
		assert $ not $ prop_unify compRet comp1
	, Testcase "Functype nr of arguments wrong:" $
		assert $ not $ prop_unify compArg comp1
	, Testcase "1" $
		case unify (IdentType "i") bIntType of
			Nothing = Failed "unification failed"
			Just s  = ((IdentType "i") ^^ s) shouldBe bIntType 
	]
where
	a = IdentType "a"
	b = IdentType "b"
	c = IdentType "c"
	d = IdentType "d"
	e = IdentType "e"
	f = IdentType "f"
	compBad	= FuncType [TupleType a a, ArrayType c				, e] bIntType
	comp1	= FuncType [TupleType a a, c						, e] bIntType
	comp2	= FuncType [b,			 ArrayType (ArrayType d)	, b] f
	compArg	= FuncType [TupleType a a, c						   ] bIntType
	compRet	= FuncType [TupleType a a, c						, e] VoidType


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
			, pos		:: Position
			}

fail :: MMonad a
fail = M \st.(Nothing,st)

makeFreshVar :: MMonad TypeVar
makeFreshVar = M \st.(Just $ "%var" +++(toString st.counter), {st & counter=st.counter + 1})
// The % is to avoid name clashes with the user defined type variables

makeFreshIdentType :: MMonad Type
makeFreshIdentType = fmap (\id.IdentType id) makeFreshVar

error :: String -> MMonad ()
error msg = M \st.(Just (), {st & errors = [makeError st.MState.pos ERROR TypeChecking msg: st.errors]})
debug msg = M \st.(Just (), {st & errors = [makeError st.MState.pos DEBUG TypeChecking msg: st.errors]})

logEnv :: MMonad ()
logEnv =
	getEnv		>>= \env.
	debug ("\nEnv vars:\n" +++ (mapToString env.varTypes) +++ "\nEnv funcs:\n" +++ (mapToString env.funcTypes))

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

getPos :: MMonad Position
getPos = M \st.(Just st.MState.pos, st)

setPos :: Position -> MMonad Position
setPos pos = M \st.(Just pos, {st & MState.pos = pos})

try :: (MMonad a) (MMonad a) -> MMonad a
try (M ma) (M me) = M \st.
	case ma st of
		(Nothing, st2)	= me st2
		justState		= justState

mRun :: (MMonad a) -> (Maybe a, [Error])
mRun (M ma) =
	let (maybeA, st) = ma {counter = 0, errors = [], label = "", env = newEnv, subst = idSubst, pos = zero}
	in (maybeA, reverse st.errors)

//derived combinators
mUnify :: Type Type -> MMonad Subst // calculate the unification, immediately apply it to the environment and the substitution function.
mUnify t1 t2 = case unify t1 t2 of
	Just subst	=
			changeEnv (\env. env ^^ subst)	>>|
			addSubst subst
	Nothing		= error ("Cannot unify " <++ t1 <++ " and " <++ t2) >>| fail

scope :: String (MMonad a) -> MMonad a
scope s ma =
	setLabel s >>| ma >>= \a.setLabel "" >>| return a

suffixedName label name = case label of
	""	= name
	_	= name +++ "@" +++ label

addVarType :: Id Type -> MMonad Type
addVarType id t =
	getLabel	>>= \label.
	changeEnv (\env. {env & varTypes = 'm'.put (suffixedName label id) t env.varTypes})
				>>| return t

addFuncType :: Id TypeScheme -> MMonad TypeScheme
addFuncType id ts =
	getLabel	>>= \label.
	changeEnv (\env. {env & funcTypes = 'm'.put (suffixedName label id) ts env.funcTypes})
				>>| return ts

getVarType :: Id -> MMonad Type
getVarType id =
	getEnv		>>= \env.
	getLabel	>>= \label.
	case 'm'.get (suffixedName label id) env.varTypes of
		Just t	= return t
		_		= case 'm'.get id env.varTypes of
			Just t	= return t
			_		=
				error ("Variable '" +++ id +++ "' not defined") >>| fail

getFuncType :: Id -> MMonad TypeScheme
getFuncType id =
	getEnv		>>= \env.
	getLabel	>>= \label.
	case 'm'.get (suffixedName label id) env.funcTypes of
		Just t	= return t
		_		= case 'm'.get id env.funcTypes of
			Just t	= return t
			_		= error ("Function '" +++ id +++ "' not defined") >>| fail 

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
class match a :: a Type -> MMonad (Subst, a)

// @param env:		The environment built up so far
// @param a:		The AST object to type-check
// @return:			An MMonad with:
//					- the AST with updated type information in the metadata
// 					- the environment with the type of the functions, variables added
//					MMonad fails or gives an error if the type inference fails
class matchN a :: a -> MMonad (Subst, a)

matchAll :: [(a,Type)] -> MMonad (Subst, [a]) | match a
matchAll list =
	foldM (\(subst,as) (a,t) -> 
			match a (t ^^ subst) >>= \(s1, a`).
			return (s1 O subst, [a`:as]))
		(idSubst,[]) list		>>= \(subst,as).
	return (subst, reverse as)

matchAllN :: [a] -> MMonad (Subst, [a]) | matchN a
matchAllN list =
	foldM (\(subst, as) a -> 
			matchN a >>= \(s1,a`).
			return (s1 O subst, [a`:as]))
		(idSubst, []) list		>>= \(subst, as).
	return (subst, reverse as)

inferenceEnv :: AST -> ((Maybe Env), [Error])
inferenceEnv ast = mRun (matchN ast >>| getEnv)

typeInference :: AST -> ((Maybe AST), [Error])
typeInference ast
# (maybeRes, log) = mRun (matchN ast)
| any (\e -> isMember e.severity [FATAL, ERROR]) log = (Nothing, log)
=	(case maybeRes of
		Nothing				= Nothing
		Just (subst, ast`)	= Just (mapSubst subst ast`)
	, log)
where
	mapSubst :: Subst AST -> AST
	mapSubst subst ast = mapMeta
		(\m. { m & type			= mapMaybe (\t .t  ^^ subst) m.type})
		(\m. { m & typeScheme	= mapMaybe (\ts.ts ^^ subst) m.typeScheme})
		ast

uptoTypeInference ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> a)
	([Error] -> a)
		-> Either a (AST, [Error])
uptoTypeInference prog fscanErrors fparseErrors fbindingErrors ftypeErrors =
	case uptoBinding prog fscanErrors fparseErrors fbindingErrors of//uptoBinding uptoParse
		Left a	= Left a
		Right (ast, bindingErrors) =
			let (mAST, typeErrors) = typeInference ast
			in case mAST of
				Nothing  = Left $ fparseErrors (bindingErrors ++ typeErrors)
				Just ast = Right (ast, bindingErrors ++ typeErrors)

instance matchN AST where
	matchN ast =
		addFuncType "print"		(TS (singleton "a") (FuncType [IdentType "a"]				VoidType))		>>|
		addFuncType "read"		(TS (singleton "a") (FuncType [] 							(IdentType "a")))	>>|
		addFuncType "isEmpty"	(TS (singleton "a") (FuncType [ArrayType (IdentType "a")]	bBoolType))		>>|
		addFuncType "main"		(TS newSet			(FuncType [] 							VoidType))		>>|
		matchAllN ast

instance matchN Decl where
	matchN d = case d of
		Var v =
			try
				(matchN v >>= \(s, v`). return (s, Var v`))
				(
					let (VarDecl _ name _ _) = v in
					makeFreshIdentType	>>= \a.
					addVarType name a	>>|
					return (idSubst, Var v)
				)
		Fun f =
			try
				(matchN f >>= \(s, f`). return (s, Fun f`))
				(
					let (FunDecl name args _ _ _ _) = f in
					forM args (\_ -> makeFreshVar)	>>= \argTypes.
					makeFreshVar					>>= \a.
					addFuncType name (TS
							(fromList [a:argTypes])
							(FuncType (map IdentType argTypes) (IdentType a))
						)	>>|
					return (idSubst, Fun f)
				)

instance matchN VarDecl where
	matchN v=:(VarDecl Nothing name e m) =
		setPos m.MetaData.pos						>>|
		makeFreshIdentType							>>= \type.
		//debug zero ("Fresh type " +++ (prettyPrint type) +++ " for " +++ (prettyPrint v)) >>|
		match e type								>>= \(subst, e`).
		addVarType name (type ^^ subst)				>>|
		return (subst, VarDecl Nothing name e` $ setMetaType m type)
	matchN (VarDecl (Just specifiedType) name e m) =
		setPos m.MetaData.pos				>>|
		matchN (VarDecl Nothing name e m)	>>= \(subst, VarDecl _ name` e` m`).
		getVarType name						>>= \derivedType.
		if (specifiedType instanceOf derivedType)
			(	addVarType name specifiedType >>|
			 	return (subst, VarDecl (Just specifiedType) name` e` m`)
			)
			(	error ("Specified type '" <++ specifiedType
					<++"' conflicts with derived type '" <++ derivedType <++ "'(specified type may be too general)") >>|
				makeFreshIdentType							>>= \fresh. // Continue type checking with fresh variable
				addVarType name fresh						>>|
			 	return (subst, VarDecl (Just specifiedType) name` e` m`)
			)

	/*
	Probleem bij recursief typechecken:
	var p = []; // Type [Bool]
	
	f(a,b,c){ // type: A.a: a Bool [Bool] -> a
		if(b){
			int x = f(10,False,[]);//Hier weet je het type van f nog niet goed genoeg!
			print(x);
			bool y = f(10,False,p);//Dit zou fout moeten gaan!
			c = True : [];
		} else {
			return a;
		}
	}
	Je kunt het return type bij regel 3 ook niet vaststellen op Int, omdat dat te specifiek is. We zouden dit evt. wel
	kunnen doen om het onszelf wat makkelijker te maken, omdat het anders wel heel lastig wordt. Ik kan ook geen praktisch
	voorbeeld bedenken waarbij het type van de recursieve aanroep anders is dan het meest generieke af te leiden type.
	
	als er een type is gespecificeerd kun je dit op 2 manieren oplossen:
	1. Zet het gespecificeerde type in de env en ga verder met typechecken
		voordeel hiervan is dat je direct het recursieve gebruik kunt checken
	2. Typecheck eerst met allemaal vers gegenereerde types en kijk dan of het gespecificeerde type niet te generiek is
		Je hebt hier het ene voordeel niet, maar misschien is het beter om het recursieve typechecken gewoon in alle
		gevallen hetzelfde te laten werken
	*/

returnVar	:== "%return"
noInfoType	:== IdentType "%noInfo"

instance matchN FunDecl where
	matchN fd=:(FunDecl fname args Nothing varDecls stmts m) =
		setPos m.MetaDataTS.pos					>>|
		forM args (\_ -> makeFreshIdentType) >>= \argTypes.
		let freshFuncType = FuncType argTypes noInfoType in
		matchFunDecl fd (TS newSet freshFuncType) freshFuncType
	matchN fd=:(FunDecl fname args (Just specifiedType) varDecls stmts m) =
		setPos m.MetaDataTS.pos				>>|
		forM args (\_ -> makeFreshIdentType) >>= \argTypes.
		let freshFuncType = FuncType argTypes noInfoType in
		matchFunDecl fd	(TS (freeVars specifiedType) specifiedType) (freshFuncType)
													>>= \(subst, FunDecl fname` args` n` varDecls` stmts` m`).
		getFuncType fname							>>= \derivedTS=:(TS _ derivedType).
		if (specifiedType instanceOf derivedType)
			(addFuncType fname 
				(TS (freeVars specifiedType) specifiedType) >>|
			return (subst, FunDecl fname` args` n` varDecls` stmts` $ setMetaTS m` (TS (freeVars specifiedType) specifiedType)))
			(error ("Specified type '" <++ specifiedType
					<++"' conflicts with derived type '" <++ derivedTS <++ "'(specified type may be too general)") >>|
			addFuncType fname 
				(TS (freeVars freshFuncType) freshFuncType) >>|
			return (subst, FunDecl fname` args` n` varDecls` stmts` $ setMetaTS m` (TS (freeVars specifiedType) specifiedType))
			)

// Arguments:
// - FunDecl:		the FunDecl
// - TypeScheme:	the typescheme that is used for recursive calls while type-checking the body
// - Type:	 		The types for the arguments and the implicit return type
matchFunDecl :: FunDecl TypeScheme Type -> MMonad (Subst, FunDecl)
matchFunDecl (FunDecl fname args declaredType varDecls stmts m) fInstanceTS (FuncType argTypes rType) =
	setPos m.MetaDataTS.pos							>>|
	getEnv											>>= \env.
	addFuncType fname fInstanceTS					>>|
	scope fname (
		addVarType returnVar rType										>>|
		forM (zip2 args argTypes) (\(arg,type) -> addVarType arg type)	>>|
		matchAllN varDecls		>>= \(s1, vs`).
		matchAllN stmts			>>= \(s2, ss`).
		return (s2 O s1, vs`, ss`)
	)					>>= \(subst, vs`, ss`).
	let argTypes` = map (\t.t ^^ subst) argTypes in
	getVarType (returnVar +++ "@" +++ fname)	>>= \returnVarType.
	//debug zero ("Final type of " +++ returnVar +++ "@" +++ fname +++ ": " +++ (prettyPrint returnVarType)) >>|
	let
		returnType =
			case returnVarType of
				IdentType "%noInfo"	= VoidType		//Don't use noInfoType here: clean will think that it is
				_					= returnVarType	// a new variable and will always pattern-match
		funcType = FuncType argTypes` returnType
		typeScheme = TS (difference
							(freeVars funcType)
							(freeVars env)
						) funcType
	in
	/*debug zero ("Final return type of " +++ fname +++ ": " +++ (case returnType of
					Nothing = "Void"
					Just type = prettyPrint type)) >>|*/
	//debug zero ("Free variables in env:" +++ (( printAllnl o toList o freeVars) env)) >>|
	//debug zero ("Free variables in " +++ (toString funcType) +++ ":" +++ (( printAllnl o toList o freeVars) funcType)) >>|
	addFuncType fname typeScheme >>|
	return (subst, FunDecl fname args declaredType vs` ss` $ setMetaTS m typeScheme)

instance matchN Stmt where
	matchN stmt =
		setPos (getMeta stmt).MetaData.pos >>|
		case stmt of
			StmtIf c body mElse	m =
				match c bBoolType			>>= \(s1, c`).
				matchAllN body				>>= \(s2, body`).
				let s3 = s2 O s1 in
				case mElse of
					Nothing		= return (s3, StmtIf c` body` Nothing m)
					Just else	=
						matchAllN else		>>= \(s4, else`).
						return (s4 O s3, StmtIf c` body` (Just else`) m)
			StmtWhile c body	m =
				match c bBoolType			>>= \(s1, c`).
				matchAllN body				>>= \(s2, body`).
				return (s2 O s1, StmtWhile c` body` m)
			StmtAss iwf e		m =
				makeFreshIdentType			>>= \a.
				match iwf a					>>= \(s1, iwf`).
				match e (a ^^ s1)			>>= \(s2, e`).
				return (s2 O s1, StmtAss iwf` e` m)
			StmtFunCall funCall	m =
				makeFreshIdentType			>>= \a.
				//debug zero ("Fresh type " +++ (prettyPrint a) +++ " for " +++ (prettyPrint stmt)) >>|
				match funCall a				>>= \(subst, funCall`).
				return (subst, StmtFunCall funCall` m) 
			StmtRet e			m =
				getVarType returnVar		>>= \rType.
				(case rType of
					IdentType "%noInfo"	=
						makeFreshIdentType >>= \a.
						addVarType returnVar a
					VoidType			= error "Non-matching return types" >>| fail
					type				= return type)
											>>= \a.
				match e a					>>= \(subst, e`).
				return (subst, StmtRet e` m)
			StmtRetV			m =
				getVarType returnVar		>>= \rType.
				case rType of
					IdentType "%noInfo"	= addVarType returnVar VoidType >>| return (idSubst, StmtRetV m)
					VoidType			= return (idSubst, StmtRetV m)
					_					= error "Non-matching return types" >>| fail


instance match Expr where
	match expression t =
		setPos (getMeta expression).MetaData.pos >>|
		case expression of
			ExpIdent iwf		m =
				match iwf t		>>= \(subst, iwf`).
				return (subst, ExpIdent iwf $ setMetaType m t)
			ExpBinOp e1 op e2	_ = case op of
					OpPlus		= match2 expression bIntType bIntType bIntType t
					OpMinus		= match2 expression bIntType bIntType bIntType t
					OpMult		= match2 expression bIntType bIntType bIntType t
					OpDiv		= match2 expression bIntType bIntType bIntType t
					OpMod		= match2 expression bIntType bIntType bIntType t
					OpEquals	= 
						makeFreshIdentType	>>= \a.
						match2 expression a a bBoolType t
					OpLT		= match2 expression bIntType bIntType bBoolType t
					OpGT		= match2 expression bIntType bIntType bBoolType t
					OpLTE		= match2 expression bIntType bIntType bBoolType t
					OpGTE		= match2 expression bIntType bIntType bBoolType t
					OpNE		= 
						makeFreshIdentType	>>= \a.
						match2 expression a a bBoolType t
					OpAnd		= match2 expression bBoolType bBoolType bBoolType t
					OpOr		= match2 expression bBoolType bBoolType bBoolType t
					OpConcat	=
						makeFreshIdentType	>>= \a.
						match2 expression a (ArrayType a) (ArrayType a) t
			ExpUnOp op e		_ = case op of
					OpNot		= match1 expression bBoolType bBoolType t
					OpNeg		= match1 expression bIntType  bIntType t
			ExpInt i			m =
				mUnify t bIntType					>>= \subst.
				return (subst, ExpInt i $ setMetaType m bIntType)
			ExpChar c			m =
				mUnify t bCharType					>>= \subst.
				return (subst, ExpChar c $ setMetaType m bCharType)
			ExpBool b			m =
				mUnify t bBoolType					>>= \subst.
				return (subst, ExpBool b $ setMetaType m bBoolType)
			ExpFunCall funCall	m =
				match funCall t						>>= \(subst, funCall`).
				return (subst, ExpFunCall funCall` $ setMetaType m t)
			ExpEmptyArray		m =
				makeFreshIdentType					>>= \a.
				mUnify t (ArrayType a)				>>= \subst.
				return (subst, ExpEmptyArray $ setMetaType m (ArrayType a))
			ExpTuple e1 e2		m =
				makeFreshIdentType					>>= \a.
				makeFreshIdentType					>>= \b.
				match e1 a					>>= \(s1, e1`).
				match e2 b					>>= \(s2, e2`).
				let s3 = s2 O s1 in
				mUnify (t ^^ s3) ((TupleType a b) ^^ s3)	>>= \s4.
				return (s4 O s3, ExpTuple e1` e2` $ setMetaType m (TupleType a b))
		where
			match1 :: Expr Type Type Type -> MMonad (Subst, Expr)
			match1 (ExpUnOp op e m) t1 resultType t =
				setPos m.MetaData.pos		>>|
				match e t1					>>= \(s1, e`).
				mUnify (t ^^ s1) resultType	>>= \s2.
				return (s2 O s1, ExpUnOp op e` $ setMetaType m resultType)
			
			match2 :: Expr Type Type Type Type -> MMonad (Subst, Expr)
			match2 (ExpBinOp e1 op e2 m) t1 t2 resultType t =
				setPos m.MetaData.pos		>>|
				match e1 t1					>>= \(s1, e1`).
				match e2 (t2 ^^ s1)			>>= \(s2, e2`).
				let s3 = s2 O s1 in
				mUnify (t ^^ s3) (resultType ^^ s3)	>>= \s4.
				return (s4 O s3, ExpBinOp e1` op e2` $ setMetaType m resultType)

instance match FunCall where
	match (FunCall name args m) t =
		setPos m.MetaData.pos								>>|
		getFuncType name									>>= \(TS boundedTypes fType).
		//debug zero ("Retrieved type of " +++ name +++ ": " +++ (toString (TS boundedTypes fType))) >>|
		makeInstanceFuncType (toList boundedTypes) fType	>>= \(FuncType argTypes rType).
		//debug zero ("Generated instance function type: " +++ (prettyPrint (FuncType argTypes rType))) >>|
		matchAll (zip2 args argTypes)						>>= \(subst, args`).
		//debug zero ("Unified   instance function type: " +++ (toString (TS boundedTypes fType))) >>|
		mUnify (t ^^ subst) (rType ^^ subst)				>>= \s1.
		return (s1 O subst, FunCall name args` $ setMetaType m t)
// twee doelen:
// checken of typing klopt 
//		f :: A.a:[Int] a [a] -> a
//		f([1], 1, ['c'])
// unification van unknown variabelen
//      var x = []; //lijst van int
//		f(x,1,[]);

makeInstanceFuncType :: [TypeVar] Type -> MMonad Type
makeInstanceFuncType [bounded:rest] fType =
	makeFreshIdentType		>>= \fresh.
	makeInstanceFuncType rest (fType ^^ substitute bounded fresh)
makeInstanceFuncType _ fType = return fType


instance match IdWithFields where
	match iwf t =
		setPos (getMeta iwf).MetaData.pos		>>|
		makeFreshIdentType	>>= \a.
		makeFreshIdentType	>>= \b.
		case iwf of
			WithField iwf2 field m	= case field of
				FieldHd =
					match iwf2 (ArrayType a)						>>= \(subst, iwf2`).
					mUnify (t ^^ subst) (a ^^ subst)				>>= \s2.
					return (s2 O subst, WithField iwf2` field $ setMetaType m t)
				FieldTl = 
					match iwf2 (ArrayType a)						>>= \(subst, iwf2`).
					mUnify (t ^^ subst) (ArrayType (a ^^ subst))	>>= \s2.
					return (s2 O subst, WithField iwf2` field $ setMetaType m t)
				FieldFst = 
					match iwf2 (TupleType a b)						>>= \(subst, iwf2`).
					mUnify (t ^^ subst) (a ^^ subst)				>>= \s2.
					return (s2 O subst, WithField iwf2` field $ setMetaType m t)
				FieldSnd = 
					match iwf2 (TupleType a b)						>>= \(subst, iwf2`).
					mUnify (t ^^ subst) (b ^^ subst)				>>= \s2.
					return (s2 O subst, WithField iwf2` field $ setMetaType m t)
			JustId id m				=
				getVarType id		>>= \t2.
				mUnify t t2			>>= \subst.
				return (subst, JustId id $ setMetaType m t)



// --
// -- Testing
// --

import qualified Scanner

checkProg :: String [(Id, Type)] [(Id, TypeScheme)] -> [Testcase]
checkProg prog vartypes funtypes =
	case 'Scanner'.scanner prog of
		(_,[e:es])	= thisFailed ("Scan error: \n" +++ (errorsToString [e:es]))
		(tokens,[])	= case parser tokens of
			Left es		= thisFailed ("Parse error: \n" +++ (errorsToString es))
			Right ast	= case inferenceEnv ast of
				(Just env, log)	=
					let results = checkEnv env vartypes funtypes
					in if (any isFailed results)
							[Testcase prog $ Failed $ concat
									[ "Log:\n"
									, errorsToString (reverse log)
									, "Variable types:\n"
									, mapToString env.varTypes
									, "Function types:\n"
									, mapToString env.funcTypes]
								: results]
							results
				(Nothing, es)	= thisFailed ("Type inference failed.\n" +++ (errorsToString es))
where
	thisFailed msg = [Testcase prog (Failed msg)]
	isFailed (Testcase _ x) = case x of
		Failed _	= True
		_			= False
	
	checkEnv env vList fList =
		(map (\(var,type) -> 
				Testcase ("Variable " +++ var) $ checkMap env.varTypes (var,type) (\t.t shouldBe type)
			  )
			vList)
		++
		(map (\(func,ts) ->
				Testcase ("Function " +++ func) $
					checkMap env.funcTypes (func,ts) (\ts` -> assert $ isomorphic ts ts`)
			 )
			fList)
	
checkMap :: (Map k a) (k, a) (a -> TestResult) -> TestResult | < k
checkMap map (var,type) f = 
	case 'm'.get var map of
		Nothing		= Failed "not in map"
		Just t		= f t

mapToString :: (Map k a) -> String | toString k & toString a
mapToString m = concat (
	map (\(k,a) -> "key:" +++ (toString k) +++ "\tvalue:" +++ (toString a) +++ "\n") ('m'.toList m)
	)

checkVars :: String [(Id, Type)] -> [Testcase]
checkVars prog vartypes = checkProg prog vartypes []

(shouldFailWith) :: Testcase String -> Testcase
(shouldFailWith) (Testcase descr result) expReason = case result of
	Passed			= Testcase descr (Failed "")
	Failed reason	= Testcase descr (reason shouldBe expReason)

instance == Type where (==) a b = a === b

match_tests =  (checkVars p1 	[ ("x", bIntType)])
			++ (checkVars p2	[ ("x", bIntType)
								, ("y", bCharType)
								])
			++ (checkVars p3	[ ("x1", bIntType)
								, ("x2", bBoolType)
								, ("x3", bBoolType)
								, ("x4", TupleType bIntType bIntType)
								])
			++ (checkVars p4	[ ("x1", ArrayType (TupleType bIntType bBoolType))
								, ("x2", ArrayType (TupleType bIntType bBoolType))
								, ("x3", TupleType bIntType bBoolType)
								, ("x4", bBoolType)
								])
			++ (checkProg p5	[ ("x1@f2", bIntType)
								, ("l@f4", ArrayType bIntType)
								]
								[ ("f1", TS (fromList []) $ FuncType [] bIntType)
								, ("f2", TS (fromList []) $ FuncType [] VoidType)
								, ("f3", TS (fromList []) $ FuncType [ArrayType bIntType] VoidType)
								, ("f4", TS (fromList []) $ FuncType [] VoidType)
								])
			++ (checkProg p6	[]
								[ ("f1", TS (fromList ["a"]) $ FuncType [IdentType "a"] (IdentType "a"))
								])
//			++ [(hd (checkVars pi [])) shouldFailWith ("Type inference failed."
//							+/ "ERROR[0,0] (TypeChecking): Cannot unify (BasicType IntType) and (BasicType CharType)")]
where
	//Var declarations
	p1 = "var x = 1;"
	p2 = "var x = 1;"
	  +/ "var y = 'c';"
	//Expressions
	p3 = "var x1 = 1 + 1;"
	  +/ "var x2 = 1 > 2;"
	  +/ "var x3 = True && False;"
	  +/ "var x4 = (1,2);"
	//Field selectors
	p4 = "var x1 = (1,True) : [];"
	  +/ "var x2 = x1.tl;"
	  +/ "var x3 = x1.hd;"
	  +/ "var x4 = x3.snd;"
	//Func declarations, funCalls(in expression and direct), local variables, arguments
	p5 = "f1(){return 1;}" //TODO: declared types
	  +/ "f2(){"
	  +/ "	var x1 = f1();"
	  +/ "	f1();"
	  +/ "	return;"
	  +/ "}"
	  +/ "f3(intList){"
	  +/ "	intList.hd = 1;"
	  +/ "	return;"
	  +/ "}"
	  +/ "f4(){"
	  +/ "	var l = [];"
	  +/ "	f3(l);"
	  +/ "	return;"
	  +/ "}"
	p6 = "f1(a){return a;}"
	pi = "var x = 1;"
	  +/ "f(){"
	  +/ "	x = 'c';"
	  +/ "	return;"
	  +/ "}"
	//Statements
	
(+/) infixr 5 :: String a -> String | toString a
(+/) s1 s2 = s1 +++ "\n" +++ (toString s2)




// --
// -- Setting the types in the AST
// --

typeCheckerTests :: [Testcase]
typeCheckerTests = unify_tests ++ instanceOf_tests ++ match_tests

Start w = runTests typeCheckerTests (const True) w



















