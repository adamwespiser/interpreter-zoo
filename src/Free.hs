module Free where
import Control.Applicative
import Control.Monad

data Free f a = Roll (f (Free f a)) | Return a

instance Functor f => Applicative (Free f) where 
  pure = return
  (<*>) = ap

instance Functor f => Functor (Free f) where 
  fmap f (Return a) = Return $ f a
  fmap f (Roll a) = Roll (fmap f <$> a)

instance Functor f => Monad (Free f ) where 
  return = Return 
  (Return a) >>= f = f a
  (Roll a) >>= f = Roll $ fmap (>>= f) a













