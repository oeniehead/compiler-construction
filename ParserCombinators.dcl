definition module ParserCombinators

// Adjusted version of Text.Parsers.Simple.Core from the iTasks SDK library

from Control.Applicative import class Applicative (..), class Alternative (..), *>, <*
from Control.Monad import class Monad (..), class MonadPlus (..)
from Data.Either import :: Either (..)
from Data.Functor import class Functor (..), <$>
import Error

from Token		import :: Token
from Misc		import :: Position

:: Parser a

// AMF instances
instance Functor (Parser)
instance Applicative (Parser)
instance Alternative (Parser)
instance Monad (Parser)
instance MonadPlus (Parser)

// Functions to run the parser
parse     :: (Parser a) [Token] -> Either [Error] a
//runParser :: (Parser a) State -> ([(a, State)], [Error])

// Core combinators
pFail		:: Parser a
pYield		:: a -> Parser a
pSatisfy	:: (Token -> Bool) -> Parser Token
pError		:: Error -> Parser a
pGetPos		:: Parser Position

// Derived convenience parsers
(@!) infixr 4 :: (Parser a) Error -> Parser a
(<$) infixl 6 :: a (Parser b) -> Parser a
($>) infixl 6 :: (Parser b) a -> Parser a

(<<|>) infixr 4 :: (Parser a) (Parser a) -> Parser a
(<|>>) infixr 4 :: (Parser a) (Parser a) -> Parser a

(<:>) infixr 6 :: (Parser r) (Parser [r]) -> Parser [r]

pMany     :: (Parser r) -> Parser [r]
pSome     :: (Parser r) -> Parser [r]
//pOptional :: (Parser s r) (r -> Parser s r) -> Parser s r
//pOneOf    :: [t] -> Parser t t | == t
//pChainl   :: (Parser t a) (Parser t (a a -> a)) a -> Parser t a
//pChainl1  :: (Parser t a) (Parser t (a a -> a)) -> Parser t a
//pToken    :: t -> Parser t t | == t
//pSepBy    :: (Parser t a) (Parser t s) -> Parser t [a]
//pSepBy1   :: (Parser t a) (Parser t s) -> Parser t [a]
pList     :: [Parser a] -> Parser [a]



