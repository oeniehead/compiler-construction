implementation module TypeChecker

import CustomStdEnv
import SPLParser
import Misc
import Error
import Spec
import PrettyPrinter

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
	(case (rType1, rType2) of
		(Nothing, Nothing)	= Just env
		(Just r1, Just r2)	= rename` r1 r2 env
		_					= Nothing
	) >>= \e2.
	renameAll argTypes1 argTypes2 e2
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
unify (TupleType t1 t2) (TupleType t1` t2`)	=
	unify t1 t1`								>>= \subst.
	unify (t2 ^^ subst) (t2` ^^ subst)	>>= \subst2.
	return (subst2 O subst)
unify (ArrayType t1) (ArrayType t2)			= unify t1 t2
// IdentType cases
unify (IdentType i) (BasicType b)			= Just $ substitute i (BasicType b)
unify (IdentType i1) (IdentType i2)			= Just $ substitute i1 (IdentType i2)
unify (IdentType i) t	= if (member i (freeVars t))
									(Nothing)
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
unify a b = Nothing

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
			(TS (fromList ["%var0"]) $ FuncType [IdentType "%var0"] (Just $ IdentType "%var0"))
			instanceOf
			(TS (fromList ["a"]) $ FuncType [IdentType "a"] (Just $ IdentType "a"))
	]
where
	specificType =
		FuncType [ IdentType "a", IdentType "b", ArrayType (TupleType (IdentType "b") bIntType)]
			(Just
				(TupleType
					(ArrayType (IdentType "a"))
					(TupleType (IdentType "b") bIntType)
				)
			)
	generalType =
			FuncType [ IdentType "a", IdentType "b", ArrayType (IdentType "c")]
				(Just
					(TupleType
						(ArrayType (IdentType "a"))
						(IdentType "c")
					)
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
											  a, FuncType [a,b] Nothing, FuncType [] (Just a)]
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

makeFreshVar :: MMonad TypeVar
makeFreshVar = M \st.(Just $ "%var" +++(toString st.counter), {st & counter=st.counter + 1})
// The % is to avoid name clashes with the user defined type variables

makeFreshIdentType :: MMonad Type
makeFreshIdentType = fmap (\id.IdentType id) makeFreshVar

error :: Position String -> MMonad a
error pos msg = M \st.(Nothing, {st & errors = [makeError pos ERROR TypeChecking msg: st.errors]})
debug pos msg = M \st.(Just (), {st & errors = [makeError pos DEBUG TypeChecking msg: st.errors]})

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
	Nothing		= error zero ("Cannot unify " <++ t1 <++ " and " <++ t2) // TODO: position

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
			_		= error zero ("Variable '" +++ id +++ "' not defined")//TODO: position

getFuncType :: Id -> MMonad TypeScheme
getFuncType id =
	getEnv		>>= \env.
	getLabel	>>= \label.
	case 'm'.get (suffixedName label id) env.funcTypes of
		Just t	= return t
		_		= case 'm'.get id env.funcTypes of
			Just t	= return t
			_		= error zero ("Variable '" +++ id +++ "' not defined")//TODO: position

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
class matchN a :: a -> MMonad Subst

matchAll :: [(a,Type)] -> MMonad Subst | match a
matchAll list =
	foldM (\subst (a,t) -> 
			match a (t ^^ subst) >>= \s1.
			return (s1 O subst))
		idSubst list

matchAllN :: [a] -> MMonad Subst | matchN a
matchAllN list =
	foldM (\subst a -> 
			matchN a >>= \s1.
			return (s1 O subst))
		idSubst list

typeInference :: AST -> ((Maybe Env), [Error])
typeInference ast = mRun (matchAST ast)

matchAST :: AST -> MMonad Env
matchAST ast =
	forM_ ast (\decl.case decl of
		Var varDecl = matchN varDecl
		Fun funDecl = matchN funDecl
		) >>| getEnv

instance matchN VarDecl where
	matchN v=:(VarDecl Nothing name e _) =
		makeFreshIdentType							>>= \type.
		debug zero ("Fresh type " +++ (prettyPrint type) +++ " for " +++ (prettyPrint v)) >>|
		match e type								>>= \subst.
		addVarType name (type ^^ subst)				>>|
		return subst
	matchN (VarDecl (Just specifiedType) name e m) =
		matchN (VarDecl Nothing name e m)	>>= \subst.
		getVarType name						>>= \derivedType.
		if (specifiedType instanceOf derivedType)
			(addVarType name specifiedType >>|
			 return subst						)
			(error m.MetaData.pos ("Specified type too generic, derived type is: " <++ derivedType) >>| fail)

returnVar	:== "%return"
noInfoType	:== IdentType "%noInfo"
voidType	:== IdentType "%void"

instance matchN FunDecl where
	matchN (FunDecl fname args Nothing varDecls stmts _) =
		getEnv											>>= \env.
		forM args (\_ -> makeFreshIdentType)			>>= \argTypes.
		//let tempfType = TS newSet (FuncType argTypes rType) in
		scope fname (
			addVarType returnVar noInfoType									>>|
			forM (zip2 args argTypes) (\(arg,type) -> addVarType arg type)	>>|
			matchAllN varDecls		>>= \s1.
			matchAllN stmts			>>= \s2.
			return (s2 O s1)
		)					>>= \subst.
		let argTypes` = map (\t.t ^^ subst) argTypes in
		getVarType (returnVar +++ "@" +++ fname)	>>= \returnVarType.
		debug zero ("Final type of " +++ returnVar +++ "@" +++ fname +++ ": " +++ (prettyPrint returnVarType)) >>|
		let returnType =
				case returnVarType of
					IdentType "%noInfo"	= Nothing //Don't use returnVar here: clean will think that it is
					IdentType "%void"	= Nothing // a new type variable and will always pattern-match
					_					= Just returnVarType
		in
		let funcType = FuncType argTypes` returnType in
		debug zero ("Final return type of " +++ fname +++ ": " +++ (case returnType of
						Nothing = "Void"
						Just type = prettyPrint type)) >>|
		
		debug zero ("Free variables in env:" +++ (( printAllnl o toList o freeVars) env)) >>|
		debug zero ("Free variables in " +++ (toString funcType) +++ ":" +++ (( printAllnl o toList o freeVars) funcType)) >>|
		addFuncType fname 
			(TS 
				(difference
					(freeVars funcType)
					(freeVars env)
				)
				funcType
			) >>|
		return subst
	/*
	Probleem bij recursief typechecken:
	f(a,b){ // type: A.a: a Bool -> a
		if(b){
			int x = f(10,False);//Hier weet je het type van f nog niet goed genoeg!
			print x;
			bool y = f(10,False);//Dit zou fout moeten gaan!
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
	matchN (FunDecl fname args (Just specifiedType) varDecls stmts m) =
		matchN (FunDecl fname args Nothing varDecls stmts m)	>>= \subst.
		getFuncType fname										>>= \derivedTS=:(TS _ derivedType).
		if (specifiedType instanceOf derivedType)
			(addFuncType fname 
				(TS (freeVars specifiedType) specifiedType) >>|
			return subst)
			(error m.MetaDataTS.pos ("Specified type too generic, derived type is: " <++ derivedTS) >>| fail)

instance matchN Stmt where
	matchN stmt = case stmt of
		StmtIf c body mElse	_ =
			match c bBoolType			>>= \s1.
			matchAllN body				>>= \s2.
			let s3 = s2 O s1 in
			maybe
				(return s3)
				(\else.matchAllN else	>>= \s4.
				return (s4 O s3))
				mElse
		StmtWhile c body	_ =
			match c bBoolType			>>= \s1.
			matchAllN body				>>= \s2.
			return (s2 O s1)
		StmtAss iwf e		_ =
			makeFreshIdentType			>>= \a.
			match iwf a					>>= \s1.
			match e (a ^^ s1)			>>= \s2.
			return (s2 O s1)
		StmtFunCall funCall	_ =
			makeFreshIdentType			>>= \a.
			debug zero ("Fresh type " +++ (prettyPrint a) +++ " for " +++ (prettyPrint stmt)) >>|
			match funCall a
		StmtRet e			m =
			getVarType returnVar		>>= \rType.
			(case rType of
				noInfoType	= makeFreshIdentType >>= \a.
					debug zero ("making fresh return type for " +++ (prettyPrint (StmtRet e m))) >>|
					addVarType returnVar a
				voidType	= error m.MetaData.pos "Non-matching return types"
				type		= return type)
										>>= \a.
			match e a
		StmtRetV			m =
			getVarType returnVar		>>= \rType.
			case rType of
				noInfoType	= addVarType returnVar voidType >>| return idSubst
				voidType	= return idSubst
				_			= error m.MetaData.pos "Non-matching return types"


instance match Expr where
	match expression t = case expression of
		ExpIdent iwf		_ = match iwf t
		ExpBinOp e1 op e2	_ = case op of
				OpPlus		= match2 e1 e2 bIntType bIntType bIntType t
				OpMinus		= match2 e1 e2 bIntType bIntType bIntType t
				OpMult		= match2 e1 e2 bIntType bIntType bIntType t
				OpDiv		= match2 e1 e2 bIntType bIntType bIntType t
				OpMod		= match2 e1 e2 bIntType bIntType bIntType t
				OpEquals	= 
					makeFreshIdentType	>>= \a.
					match2 e1 e2 a a bBoolType t
				OpLT		= match2 e1 e2 bIntType bIntType bBoolType t
				OpGT		= match2 e1 e2 bIntType bIntType bBoolType t
				OpLTE		= match2 e1 e2 bIntType bIntType bBoolType t
				OpGTE		= match2 e1 e2 bIntType bIntType bBoolType t
				OpNE		= 
					makeFreshIdentType	>>= \a.
					match2 e1 e2 a a bBoolType t
				OpAnd		= match2 e1 e2 bBoolType bBoolType bBoolType t
				OpOr		= match2 e1 e2 bBoolType bBoolType bBoolType t
				OpConcat	=
					makeFreshIdentType	>>= \a.
					match2 e1 e2 a (ArrayType a) (ArrayType a) t
		ExpUnOp op e		_ = case op of
				OpNot		= match1 e bBoolType bBoolType t
				OpNeg		= match1 e bIntType  bIntType t
		ExpInt i			_ = mUnify t bIntType
		ExpChar c			_ = mUnify t bCharType
		ExpBool b			_ = mUnify t bBoolType
		ExpFunCall funCall	_ = match funCall t
		ExpEmptyArray		_ =
			makeFreshIdentType					>>= \a.
			mUnify t (ArrayType a)
		ExpTuple e1 e2		_ =
			makeFreshIdentType					>>= \a.
			makeFreshIdentType					>>= \b.
			match2 e1 e2 a b (TupleType a b) t
	where
		match1 :: Expr Type Type Type -> MMonad Subst
		match1 e1 t1 resultType t =
			match e1 t1					>>= \s1.
			mUnify (t ^^ s1) resultType	>>= \s2.
			return (s2 O s1)
		
		match2 :: Expr Expr Type Type Type Type -> MMonad Subst
		match2 e1 e2 t1 t2 resultType t =
			match e1 t1					>>= \s1.
			match e2 (t2 ^^ s1)			>>= \s2.
			let s3 = s2 O s1 in
			mUnify (t ^^ s3) (resultType ^^s3)	>>= \s4.
			return (s4 O s3)

instance match FunCall where
	match (FunCall name args _) t =
		getFuncType name									>>= \(TS boundedTypes fType).
		debug zero ("Retrieved type of " +++ name +++ ": " +++ (toString (TS boundedTypes fType))) >>|
		makeInstanceFuncType (toList boundedTypes) fType	>>= \(FuncType argTypes rType).
		debug zero ("Generated instance function type: " +++ (prettyPrint (FuncType argTypes rType))) >>|
		matchAll (zip2 args argTypes)						>>= \subst.
		debug zero ("Unified   instance function type: " +++ (toString (TS boundedTypes fType))) >>|
		case rType of
			Nothing	= return subst
			Just r	= mUnify (t ^^ subst) (r ^^ subst)	>>= \s1.
					return (s1 O subst)
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
		makeFreshIdentType	>>= \a.
		makeFreshIdentType	>>= \b.
		case iwf of
			WithField iwf2 field _	= case field of
				FieldHd =
					match iwf2 (ArrayType a)						>>= \subst.
					mUnify (t ^^ subst) (a ^^ subst)				>>= \s2.
					return (s2 O subst)
				FieldTl = 
					match iwf2 (ArrayType a)						>>= \subst.
					mUnify (t ^^ subst) (ArrayType (a ^^ subst))	>>= \s2.
					return (s2 O subst)
				FieldFst = 
					match iwf2 (TupleType a b)						>>= \subst.
					mUnify (t ^^ subst) (a ^^ subst)				>>= \s2.
					return (s2 O subst)
				FieldSnd = 
					match iwf2 (TupleType a b)						>>= \subst.
					mUnify (t ^^ subst) (b ^^ subst)				>>= \s2.
					return (s2 O subst)
			JustId id _				=
				getVarType id		>>= \t2.
				mUnify t t2

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
			Right ast	= case typeInference ast of
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
	map (\(k,a) -> (toString k) +++ "\t" +++ (toString a) +++ "\n") ('m'.toList m)
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
								[ ("f1", TS (fromList []) $ FuncType [] (Just bIntType))
								, ("f2", TS (fromList []) $ FuncType [] Nothing)
								, ("f3", TS (fromList []) $ FuncType [ArrayType bIntType] Nothing)
								, ("f4", TS (fromList []) $ FuncType [] Nothing)
								])
			++ (checkProg p6	[]
								[ ("f1", TS (fromList ["a"]) $ FuncType [IdentType "a"] (Just $ IdentType "a"))
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

changeMeta :: MetaData Env Id -> MetaData
changeMeta m env name = setMetaType m (fromJust ('m'.get name env.varTypes))

changeMetaTS :: MetaDataTS Env Id -> MetaDataTS
changeMetaTS m env name = setMetaTS m (fromJust ('m'.get name env.funcTypes))

class setTypes a :: Env a -> a

instance setTypes AST where
	setTypes e ast = map (setTypes e) ast

instance setTypes Decl where
	setTypes e (Var v) = Var (setTypes e v)
//	setTypes e (Fun f) = Fun (setTypes e f)

instance setTypes VarDecl where
	setTypes e (VarDecl mType name expr m) = tbi//VarDecl mType name (setTypes e expr) (changeMeta m e name)


		
/*
instance setTypes FunDecl where
	setTypes e (FunDecl id args mType varDecls stmts _) =
		rtrn id + rtrn "( " + concat args (rtrn ", ") + rtrn " )" + type + nl + rtrn "{" + 
			( indentBlock (
				(concat varDecls nl) + separator + (concat stmts (nl)) 
			)) + rtrn "}" 
	where
		type = case mType of
			(Just type) = rtrn " :: " + setTypes type
			Nothing		= zero
		separator = if (length varDecls * length stmts > 0) (nl) (zero)
		
instance setTypes Stmt where
	setTypes e (StmtIf cond stmtA mStmtB _) = rtrn "if ( " + setTypes cond + rtrn " ) {" + 
		(indentBlock (concat stmtA nl)) + rtrn "}" + stmtB
		where 
			stmtB = case mStmtB of
				(Just stmt) = nl + rtrn "else {" + indentBlock (concat stmt nl) + rtrn "}"
				Nothing		= zero
	setTypes (StmtWhile cond stmt _) = rtrn "while ( " + setTypes cond + rtrn " ) {" +
		(indentBlock (concat stmt nl)) + rtrn "}"
	setTypes (StmtAss id expr _) = setTypes id + rtrn " = " + setTypes expr + rtrn ";"
	setTypes (StmtFunCall funCall _) = setTypes funCall + rtrn ";"
	setTypes (StmtRet expr _) = rtrn "return " + setTypes expr + rtrn ";"
	setTypes (StmtRetV _) = rtrn "return;"
*/

typed :: Env a -> (a,Type) | setTypes a & getMeta a
typed e a = let a` = setTypes e a in (a`, fromJust (getMeta a`).type)

/*
instance setTypes Expr where
	setTypes e expr = case expr of
		ExpIdent iwf		m = match iwf t
		ExpBinOp e1 op e2	m = case op of
			OpPlus		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Int)
			OpMinus		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Int)
			OpMult		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Int)
			OpDiv		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Int)
			OpMod		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Int)
			OpEquals	= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpLT		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpGT		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpLTE		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpGTE		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpNE		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpAnd		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpOr		= ExpBinOp (setTypes e e1) op (setTypes e e2) (setMetaType m Bool)
			OpConcat	=
				let (e1`,elType) = typed e e1 in 
				let (e2`,listType) = typed e e2 in
				ExpBinOp e1` op (setTypes e e2) (setMetaType m 
						if (listType instanceOf )
					)
		ExpUnOp op e		m = case op of
			OpNot		= match1 e bBoolType bBoolType t
			OpNeg		= match1 e bIntType  bIntType t
		ExpInt i			m = mUnify t bIntType
		ExpChar c			m = mUnify t bCharType
		ExpBool b			m = mUnify t bBoolType
		ExpFunCall funCall	m = match funCall t
		ExpEmptyArray		m =
			makeFreshIdentType					>>= \a.
			mUnify t (ArrayType a)
		ExpTuple e1 e2		m =
			makeFreshIdentType					>>= \a.
			makeFreshIdentType					>>= \b.
			match2 e1 e2 a b (TupleType a b) t
	where
		match1 :: Expr Type Type Type -> MMonad Subst
		match1 e1 t1 resultType t =
			match e1 t1					>>= \s1.
			mUnify (t ^^ s1) resultType	>>= \s2.
			return (s2 O s1)
		
		match2 :: Expr Expr Type Type Type Type -> MMonad Subst
		match2 e1 e2 t1 t2 resultType t =
			match e1 t1					>>= \s1.
			match e2 (t2 ^^ s1)			>>= \s2.
			let s3 = s2 O s1 in
			mUnify (t ^^ s3) (resultType ^^s3)	>>= \s4.
			return (s4 O s3)


instance setTypes FunCall where
	setTypes (FunCall id args _) = rtrn id + rtrn "(" + concat args (rtrn ", ") + rtrn ")"

instance setTypes IdWithFields where
	setTypes (WithField iwf field _) = 
		let iwf` = setTypes e iwf of
		
	setTypes (JustId id _) 			= rtrn id*/

typeCheckerTests :: [Testcase]
typeCheckerTests = unify_tests ++ instanceOf_tests ++ match_tests

Start w = runTests typeCheckerTests (const True) w



















