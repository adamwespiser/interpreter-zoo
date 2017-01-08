{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module RecursionScheme where

import Prelude hiding  (mapM, sequence, replicate, lookup, foldr, length)
import Control.Applicative (pure, many, empty, (<$>),(<*>),(<*),(*>),(<|>),(<$))
import Control.Arrow ((&&&),(***),(|||), first, second)
import Control.Monad hiding (mapM, sequence)
import Control.Monad.Reader hiding (mapM, sequence)
import Control.Monad.ST
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.List (break)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Numeric

import Data.Bool (bool)
import Data.Hashable
--import Data.Hashable.Class (HashTable)
--import qualified Data.Hashable.ST.Cuckoo as C
--import qualified Data.Hashable.Class as H
--import Text.ParserCombinators.Parsec hiding (space, many, (<|>))
--import Text.PrettyPrint.Leijen (Doc, Pretty, (<+>), text, space, pretty)
--import qualified Text.PrettyPrint.Leijen as PP


{-
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    fold :: Monoid m => t m -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (a -> b -> a) -> a -> t b -> a
    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a
data Tree a = Empty | Leaf a | Node (Tree a) (Tree a) deriving (Functor)

instance Foldable Tree where
  foldMap f Empty  = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

count :: Foldable t => t a -> Int
count = getSum . foldMap (const $ Sum 1)
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f  => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f  => t (f a) -> f (t a)
  mapM      :: Monad m        => (a -> m b) -> t a -> m (t b)
  sequence  :: Monad m        => t (m a) -> m (t a)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node k r) = Node <$> traverse f k <*> traverse f r

sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence = mapM id
-}

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

data ExprF r = Const Int
             | Var Id
             | Set Id r r
             | Add r r
             | Mul r r
             | IfNeg r r r
             deriving ( Show, Eq, Ord, Functor
                        , Foldable, Traversable )

type Id = String
type Expr = Fix ExprF

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)

{-
eval :: Env -> Expr -> Maybe Int
eval env = cata (evalAlg env)
evalAlg :: Env -> ExprF (Maybe Int) -> Maybe Int
evalAlg env = alg where
  alg (Const c) = pure c
  alg (Var i) = M.lookup i env
  alg (Add x y) = (+) <$> x <*> y
  alg (Mul x y) = (*) <$> x <*> y
  alg (IfNeg t x y) = t >>= bool x y . (<0)

freeVars :: Expr -> Set Id
freeVars = cata alg where
    alg :: ExprF (Set Id) -> Set Id
    alg (Var i) = S.singleton i
    alg e = F.fold e

substitute :: Map Id Expr -> Expr -> Expr
substitute env = cata alg where
  alg :: ExprF Expr -> Expr
  alg e@(Var i) = fromMaybe (Fix e) $ M.lookup i env
  alg e = Fix e


optAdd :: ExprF Expr -> Expr
optAdd (Add (Fix (Const 0)) e) = e
optAdd (Add e (Fix (Const 0))) = e
optAdd e = Fix e

optMul :: ExprF Expr -> Expr
optMul (Mul (Fix (Const 1)) e) = e
optMul (Mul e (Fix (Const 1))) = e
optMul e = Fix e

optimiseSlow :: Expr -> Expr
optimiseSlow = cata optAdd . cata optMul

optimiseFast :: Expr -> Expr
optimiseFast = cata (optMul . unFix . optAdd)
-}
fish ::(Monad m) =>  (b -> m c) -> (a -> m b) -> a -> m c
fish = (<=<)

cataM :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataM algM = algM <=< (mapM (cataM algM) . unFix)

type Env = Map Id Int

eval' :: Env -> Expr -> Maybe Int
eval' env = (`runReaderT` env) . cataM algM where 
  algM :: ExprF Int -> ReaderT Env Maybe Int
  algM (Const c) = return c
  algM (Var i) = ask >>= lift . M.lookup i
  algM (Set i x y) = local (M.insert i x) (return x)
  algM (Add x y) = return $ x + y
  algM (Mul x y)    = return $ x * y
  algM (IfNeg t x y) = return $ bool x y (t<0)

testEnv :: Env
testEnv = M.fromList [("a", 1), ("b", 2)]

eSet :: Expr
eSet = Fix (Set  "a" (Fix (Const 11)) (Fix (Var "a")))

eSet2 :: Expr
eSet2 = Fix (Set  "b" (Fix (Const 11)) e1) -- Just 11

eSet3 :: Expr
eSet3 = Fix (Add (Fix (Var "b")) -- Just 2
                 eSet2)  -- Just 11



e1 :: Expr
e1 = Fix (Mul (Fix (IfNeg
            (Fix (Mul (Fix (Const 1))  
                   (Fix (Var "a"))))
            (Fix (Add (Fix (Var "b"))
                   (Fix (Const 0))))
            (Fix (Add (Fix (Var "b"))
                    (Fix (Const 2))))))
      (Fix (Const 3)))

e2 :: Expr
e2 = Fix (IfNeg (Fix (Var "b")) e1 (Fix (Const 4)))


runExpr :: IO ()
runExpr = do 
  print $ eval' testEnv e2
  print $ eval' testEnv eSet
  print $ eval' testEnv eSet2











