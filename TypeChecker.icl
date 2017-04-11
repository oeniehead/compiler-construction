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

// -- Unification
:: TypeScheme = TS  (Set Id) Type // the type with the bounded variables
:: Env :== Id -> Type

class freeVars a :: a -> Set Id // TV in the slides
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

:: IdSubst		:== Id -> Type
:: TypeSubst	:== Type -> Type

mapSubst :: IdSubst -> TypeSubst
mapSubst subst = \type.case type of
	IdentType id	= subst id
	BasicType b		= BasicType b
	TupleType t1 t2	= TupleType (mapSubst subst t1) (mapSubst subst t2)
	ArrayType t		= ArrayType (mapSubst subst t)
	FuncType args r = FuncType (map (mapSubst subst) args) (mapMaybe (mapSubst subst) r)

substitute :: Id Type -> TypeSubst
substitute id type = mapSubst (\i2.if (i2 === id) type (IdentType i2))

unify :: Type Type -> Maybe TypeSubst
unify (BasicType b1) (BasicType b2)			= if (b1 === b2) (Just id) Nothing
unify (TupleType t1 t2) (TupleType t1` t2`)	=
	unify t1 t1`					>>= \subst.
	unify (subst t2) (subst t2`)	>>= \subst2.
	return (subst2 o subst)
unify (ArrayType t1) (ArrayType t2)			= unify t1 t2
// IdentType cases
unify (IdentType i) (BasicType b)			= Just $ substitute i (BasicType b)
unify (IdentType i1) (IdentType i2)			= Just $ substitute i1 (IdentType i2)
unify (IdentType i) t	= if (member i (freeVars t))
									(/*trace_n (i <+++ "is a member of" <+++ t)*/ Nothing)
									(Just (substitute i t))
// IdentType in the other argument is symmetric
unify t (IdentType i)						= unify (IdentType i) t
/*unify t (IdentType i)	= if (member i (freeVars t))
									Nothing
									(Just (substitute i t))*/
unify f1=:(FuncType _ _) f2=:(FuncType _ _) = uFuncType f1 f2 id
where
	uFuncType (FuncType [a:as] mr1) (FuncType [b:bs] mr2) prevSubst = // a helper function with the substitution until now
		unify (prevSubst a) (prevSubst b)		>>= \subst.
		let newSubst = subst o prevSubst in
		(uFuncType (FuncType as mr1) (FuncType bs mr2) newSubst)
	uFuncType (FuncType [] mr1) (FuncType [] mr2) prevSubst =
		case (mr1,mr2) of
		(Just _ , Nothing)	= Nothing
		(Nothing, Just _ )	= Nothing
		(Nothing, Nothing)	= Just prevSubst
		(Just r1, Just r2)	= unify (prevSubst r1) (prevSubst r2) >>= \subst.
				return (subst o prevSubst)
	uFuncType _ _ _ = Nothing
//unify _				_						= Nothing
unify a b = /*trace_n ("cannot unify (" <+++ a <+++ ") and (" <+++ b <+++ ")" )*/ Nothing

instance toString Type where toString t = gString{|*|} t

prop_unify :: Type Type -> Bool
prop_unify type1 type2 = case unify type1 type2 of
	Nothing		= /*trace_n ("could not unify " +++ (toString type1) +++ " and " +++ (toString type2))*/ False
	Just subst	= //trace_n ((trace_ type1 subst) +++ ", " +++ (trace_ type2 subst))
			(subst type1 === subst type2)

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


// -- The M function

//makeFreshVar :: Env -> (Id,Env)
//makeFreshVar env = ("v" +++ toString env.counter, {env & counter=counter + 1})
