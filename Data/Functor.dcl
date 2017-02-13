definition module Data.Functor

from System.IO import :: IO

class Functor f where
  fmap :: (a -> b) (f a) -> f b

instance Functor ((->) r)
instance Functor ((,) a)

(<$>) infixl 4 :: (a -> b) (f a) -> f b | Functor f

(<$) infixl 4 :: a (f b) -> f a | Functor f

($>) infixl 4 :: (f b) a -> f a | Functor f

void :: (f a) -> f () | Functor f
