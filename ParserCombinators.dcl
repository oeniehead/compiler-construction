definition module ParserCombinators

// Adjusted version of Text.Parsers.Simple.Core from the iTasks SDK library

from Control.Applicative import class Applicative (..), class Alternative (..), *>, <*
from Control.Monad import class Monad (..), class MonadPlus (..)
from Data.Either import :: Either (..)
from Data.Functor import class Functor (..), <$>
import Error

from Token		import :: Token
from Misc		import :: Position

:: Parser t a

// AMF instances
instance Functor (Parser t)
instance Applicative (Parser t)
instance Alternative (Parser t)
instance Monad (Parser t)
instance MonadPlus (Parser t)

// Functions to run the parser
parse     :: (Parser t a) [t] -> Either [Error] a
runParser :: (Parser t a) [t] -> ([(a, [t])], [Error])

// Core combinators
pFail		:: Parser t a
pYield		:: a -> Parser t a
pSatisfy	:: (t -> Bool) -> Parser t t
pError		:: Error -> Parser t a
pLog		:: Error -> Parser t ()
pGetPos		:: Parser Token Position

// Derived convenience parsers
(@!) infixr 4 :: (Parser t a) Error -> Parser t a
(<$) infixl 6 :: a (Parser t b) -> Parser t a
($>) infixl 6 :: (Parser t b) a -> Parser t a

(<<|>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a
(<|>>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a

(<:>) infixr 6 :: (Parser s r) (Parser s [r]) -> Parser s [r]

pMany     :: (Parser s r) -> Parser s [r]
pSome     :: (Parser s r) -> Parser s [r]
pOptional :: (Parser s r) (r -> Parser s r) -> Parser s r
pOneOf    :: [t] -> Parser t t | == t
pChainl   :: (Parser t a) (Parser t (a a -> a)) a -> Parser t a
pChainl1  :: (Parser t a) (Parser t (a a -> a)) -> Parser t a
pToken    :: t -> Parser t t | == t
pSepBy    :: (Parser t a) (Parser t s) -> Parser t [a]
pSepBy1   :: (Parser t a) (Parser t s) -> Parser t [a]
pList :: [Parser t a] -> Parser t [a]



