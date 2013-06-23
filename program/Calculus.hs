module Calculus where

import Data.Maybe (fromJust)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

data CalculusSettings = CalculusSettings {maxSteps :: Int -> Int}

stdCalculusSettings = CalculusSettings {maxSteps = (* 10)}

type Eval a = ReaderT CalculusSettings (ErrorT String Identity) a

class Eq a => Reducible a where
  loReduce :: a -> Maybe a

  lgh :: a -> Int

  evalWith :: a -> (Int -> Int) -> Eval a
  evalWith x stepFun
    | length trace < steps = return $ fromJust $ last trace
    | otherwise = throwError "Can't be reduced!"
    where steps = stepFun $ lgh x
          trace = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x

  runEval :: Eval a -> CalculusSettings -> Either String a
  runEval ev = runIdentity . runErrorT . runReaderT ev

  eval :: a -> Either String a
  eval x = runEval ((asks maxSteps) >>= (evalWith x)) stdCalculusSettings
