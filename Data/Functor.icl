implementation module Data.Functor

from StdFunc import o, const
import Control.Applicative
import Control.Monad

instance Functor ((->) r) where
  fmap f g = \x -> (f o g) x

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

(<$>) infixl 4 :: (a -> b) (f a) -> (f b) | Functor f
(<$>) f fa = fmap f fa

(<$) infixl 4 :: a (f b) -> f a | Functor f
(<$) x fa = fmap (const x) fa

($>) infixl 4 :: (f b) a -> f a | Functor f
($>) fa x = x <$ fa

void :: (f a) -> f () | Functor f
void x = () <$ x
