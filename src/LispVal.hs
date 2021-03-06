{-# LANGUAGE OverloadedStrings,
             GADTs,
             DataKinds,
             GeneralizedNewtypeDeriving #-} 

module LispVal where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

{- 
 - here we have the standard LispVal S-Expression notation
 -}
data LispVal = Atom T.Text
              | List [LispVal] 
              | Int Integer
              | Bool Bool 
              | NullVal
{-
 - This is our reduced form of Lisp Expression
 - It Allows for Apply, Let, and Lambda forms 
 - in the Abstract Syntax Tree
 -}
data LispForm =   PrimAtom T.Text 
                | PrimInt Integer
                | PrimBool Bool 
                | Lambda [LispForm] LispForm
                | Apply LispForm [LispForm] 
                | Let [LispForm] LispForm
                | Define LispForm LispForm
                | NullForm deriving (Show, Eq)



{- 
 - Stack evaluation
 - var(n) will be the nth last pushed stack value
 - Let (x 1 y 2) (+ x y)
 - Apply (Lambda (x y) (+ x y)) (1 2)
 -
 - To evaluation strategy 1, push down stack 
 -              : push eval x -> stack  
 -              push eval y -> stack 
 -              convert (+ x y) -> (+ (var 1) (var 0)) 
 -              eval (+ var(1) var(0))
 -
 - evaluation strategy 2, monad transfomer stack 
 -              stack = stack ++ ("x", eval 1) ++ ("y", eval 2) 
 -              PrimFun :: [LispVal] -> Eval LispVal, Map.Map T.Text LispVal
 -              fn = (params -> body) stack :: PrimFun
 -              evalFn  fn args
 -}

{- Stack construction :: Proof of Concept -}

data Stack = Stack { unS :: [LispForm] }
newtype MStack a = MStack { unStack :: State Stack a } 
  deriving (Functor, Applicative, Monad, MonadState Stack)

emptyState :: MStack ()
emptyState = put $ Stack []

runStack :: Stack -> MStack () -> Stack
runStack = flip (execState . unStack)


fToStack :: LispForm -> MStack ()
fToStack x@(PrimAtom _) = do
  stk <- get
  put $ formToStack x stk

{- 
 - (+ 1 2) 
 - [lit 1, lit2, FUNCTION]
 - (+ (+ 1 2) (+ 3 4))
 - [lit 1, lit2, FUNCTION, lit1, lit2, FUNCTION, FUNCTION]
 -}
pushStack :: LispForm -> Stack -> Stack
pushStack val (Stack stack) = Stack $ val : stack

combStack :: Stack -> Stack -> Stack
combStack (Stack s1) (Stack s2) = Stack $ s1 ++ s2


formToStack :: LispForm -> Stack -> Stack
formToStack x@(PrimAtom _) = pushStack x 
formToStack x@(PrimInt _) = pushStack x 
formToStack x@(PrimBool _) = pushStack x 
formToStack x@(Define a b) = pushStack x 
formToStack x@(Lambda args expr) = (\stack -> foldl1 combStack $ fmap ((flip formToStack) (formToStack expr stack)) args)



 {-
 - evaluation strategy 3, a la oleg
 - typed tagless implementation:
 - GLambda
 - http://okmij.org/ftp/tagless-final/course/lecture.pdf
 - Apply (Lambda (x y) (+ x y)) (1 2)
 -     class AddInt where
 -        addInt :: PrimInt -> PrimInt -> PrimInt
 -
 -     instance AddInt (PrimInt) where
 -        addInt  (PrimInt x) (PrimInt y) = PrimInt $ x + y
 - Apply (Lambda (x y) (addInt x y)) (1 2)
 -      [x -> PrimInt 1, y -> PrimInt 2, fn -> PrimInt] (fn x y)
 -      (addInt PrimInt 1 $ PrimInt 2) => PrimInt 3
 -}



-- our conversion function 
valToForm :: LispVal -> LispForm
valToForm (Atom txt) = PrimAtom txt
valToForm (Int i) = PrimInt i
valToForm (Bool b) = PrimBool b
valToForm (NullVal) = NullForm
valToForm (List [Atom "let", List binds, body]) = Let (fmap valToForm binds) $ valToForm body
valToForm (List [Atom "lambda", List binds, body]) = Lambda (fmap valToForm binds) $ valToForm body
valToForm (List ((:) (Atom fnCall) args)) = Apply (PrimAtom fnCall) $ fmap valToForm args

-- what about moving Let -> Lambda ? 

type Env = Map.Map T.Text LispVal
newtype EvalReaderT a = EvalReaderT { unEval :: ReaderT Env IO a} deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO)

