definition module Control.Applicative

from Control.Monad import class Monad, class MonadPlus
from Data.Functor  import class Functor
from Data.Maybe    import :: Maybe
from Data.Monoid   import class Monoid, class Semigroup

:: Const a b = Const a
:: WrappedMonad m a = WrapMonad (m a)

unwrapMonad :: (WrappedMonad m a) -> m a

getConst :: (Const a b) -> a

instance Applicative ((->) r)
instance Applicative Maybe
instance Applicative []

instance Alternative Maybe
instance Alternative []

instance Functor (Const m)
instance Functor (WrappedMonad m) | Monad m
instance Applicative (Const m) | Monoid m
instance Applicative (WrappedMonad m) | Monad m
instance Monad (WrappedMonad m) | Monad m

instance Alternative (WrappedMonad m) | MonadPlus m

instance Semigroup (Const a b) | Semigroup a
instance Monoid (Const a b) | Monoid a

class Applicative f | Functor f where
  pure            :: a -> f a
  (<*>) infixl 4  :: (f (a -> b)) (f a) -> f b

class Alternative f | Applicative f where
  empty           :: f a
  (<|>) infixl 3  :: (f a) (f a) -> f a

some :: (f a) -> f [a] | Alternative f

many :: (f a) -> f [a] | Alternative f

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f

(<*) infixl 4 :: (f a) (f b) -> f a | Applicative f

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f

lift :: a -> f a | Applicative f

liftA :: (a -> b) (f a) -> f b | Applicative f

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f

liftA3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f

optional :: (f a) -> f (Maybe a) | Alternative f
