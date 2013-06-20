module Calculus where

import Data.Maybe (fromJust)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

data ReducibleSettings = ReducibleSettings {maxSteps :: Int -> Int}

stdReducibleSettings = ReducibleSettings {maxSteps = (* 10)}

type Eval a = ReaderT ReducibleSettings (ErrorT String Identity) a

class Eq a => Reducible a where
  loReduce :: a -> Maybe a

  lgh :: a -> Int

  evalWith :: a -> (Int -> Int) -> Eval a
  evalWith x stepFun
    | length trace < steps = return $ fromJust $ last trace
    | otherwise = throwError "Can't be reduced!"
    where steps = stepFun $ lgh x
          trace = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x

  runEval :: Eval a -> ReducibleSettings -> Either String a
  runEval ev = runIdentity . runErrorT . runReaderT ev

  eval :: a -> Either String a
  eval x = runEval ((asks maxSteps) >>= (evalWith x)) stdReducibleSettings
