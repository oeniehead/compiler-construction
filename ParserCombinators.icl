implementation module ParserCombinators

// Adjusted version of Text.Parsers.Simple.Core from the iTasks SDK library

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Func
import Data.Functor
import Data.List
from StdFunc import o, const
import Error

import Misc, Token

//:: PCont t a :== [t] -> ([(a, [t])], [Error])
//:: Parser t a = Parser (PCont t a)
:: Parser a = Parser (State -> ([(a, State)], [Error]) )
:: State = { tokens		:: [Token]
		   , prevPos	:: Position
		   }

// -- AMF instances
instance Functor (Parser) where
  fmap f p = pMap f p

instance Applicative (Parser) where
  pure x    = pYield x
  (<*>) l r = ap l r

instance Alternative (Parser) where
  empty     = pFail
  (<|>) l r = pChoice l r

instance Monad (Parser) where
  bind l r = pBind l r

instance MonadPlus (Parser) where
  mzero = pFail
  mplus l r = l <|> r

pBind :: (Parser a) (a -> Parser b) -> Parser b
pBind pl f = Parser (bind pl)
  where
  bind (Parser p) input
    # (rs1, es1) = p input
    # (rs`, es`) = unzip [  ((res2, input2), es1 ++ es2)
                         \\ (res1, input1) <- rs1
                         ,  let (rs2, es2) = runParser (f res1) input1
                         ,  (res2, input2) <- rs2
                         ]
    = (rs`, flatten es`)

pAp :: (Parser (a -> b)) (Parser a) -> Parser b
pAp pf pa
  =         pf
  >>= \f -> pa
  >>= \x -> pure (f x)

pChoice :: (Parser a) (Parser a) -> Parser a
pChoice pl pr = Parser (\st.
	let (r1,e1) = runParser pl st
		(r2,e2) = runParser pr st
	in (r1 ++ r2, e1 ++ e2) )

pMap :: (a -> b) (Parser a) -> Parser b
pMap f p = pure f <*> p


// -- Functions to run the parser
parse :: (Parser a) [Token] -> Either [Error] a
parse p tokens
  = case runParser p {tokens = tokens, prevPos = zero} of
      (rs, []) -> case [r \\ (r, st) <- rs | isEmpty st.tokens] of
                    [r : _] -> Right r
                    _       -> Left [{pos=zero, severity = FATAL, stage = Parsing,
                    				message="No parse results with a fully consumed input."}]
      (_, es)  -> Left es

runParser :: (Parser a) State -> ([(a, State)], [Error])
runParser (Parser f) xs = f xs

// -- Core combinators
pFail :: Parser a
pFail = Parser (\_ -> ([], []))

pYield :: a -> Parser a
pYield x = Parser (\input -> ([(x, input)], []))

pSatisfy :: (Token -> Bool) -> Parser Token
pSatisfy pred = Parser pSatisfy2
  where
  pSatisfy2 st=:{tokens = [token =:(Token _ _ tokenPos)  : rest]}
    | pred token = ([(token, {tokens = rest, prevPos = tokenPos})], [])
  pSatisfy2 _ = ([], [])

pError :: Error -> Parser a
pError err = Parser (\_ -> ([], [err]))

pGetPos :: Parser Position
pGetPos = Parser (\st=:{tokens} ->
	if (isEmpty tokens)
		([], [])
		(
			let (Token _ _ pos) = hd tokens
			in ([(pos,st)], [])
		)
	)


// -- Derived convenience parsers
(@!) infixr 4 :: (Parser a) Error -> Parser a
(@!) p err = p <<|> pError err

(<$) infixl 6 :: a (Parser b) -> Parser a
(<$) x p = pMap (const x) p

($>) infixl 6 :: (Parser b) a -> Parser a
($>) p x = pMap (const x) p

(<<|>) infixr 4 :: (Parser a) (Parser a) -> Parser a
(<<|>) pl pr = Parser pbl
  where
  pbl input = case runParser pl input of
                ([], _) -> runParser pr input
                res     -> res

(<|>>) infixr 4 :: (Parser a) (Parser a) -> Parser a
(<|>>) pl pr = pr <<|> pl

(<:>) infixr 6 :: (Parser r) (Parser [r]) -> Parser [r]
(<:>) p1 p2 = (\r rs -> [r : rs]) <$> p1 <*> p2

pMany :: (Parser r) -> Parser [r]
pMany p = pSome p <<|> pure []

pSome :: (Parser r) -> Parser [r]
pSome p = p <:> pMany p

//	pOptional :: (Parser s r) (r -> Parser s r) -> Parser s r
//	pOptional p1 p2 = p1 >>= \r -> p2 r <<|> pure r
//	
//	pOneOf :: [t] -> Parser t t | == t
//	pOneOf ts = pSatisfy (\x -> elem x ts)
//	
//	pChainl :: (Parser t a) (Parser t (a a -> a)) a -> Parser t a
//	pChainl p op a = pChainl1 p op <|> pure a
//	
//	pChainl1 :: (Parser t a) (Parser t (a a -> a)) -> Parser t a
//	pChainl1 p op = p >>= \a -> rest a
//	  where
//	  rest a = (op >>= \f -> p >>= \b -> rest (f a b))
//	       <|> pure a
//	
//	pToken :: t -> Parser t t | == t
//	pToken c = pSatisfy (\x -> c == x)
//	
//	pSepBy :: (Parser t a) (Parser t s) -> Parser t [a]
//	pSepBy pa psep = pSepBy1 pa psep <|> pure []
//	
//	pSepBy1 :: (Parser t a) (Parser t s) -> Parser t [a]
//	pSepBy1 pa psep = pa <:> pMany (psep >>| pa)

pList :: [Parser a] -> Parser [a]
pList xs = sequence xs





