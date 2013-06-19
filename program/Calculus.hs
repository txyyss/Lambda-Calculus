module Calculus where

import Data.Maybe (fromJust)
import Control.Monad.Identity
import Control.Monad.Error

data ReducibleArgs = ReducibleArgs {maxSteps :: Int -> Int}

stdReducibleArgs = ReducibleArgs {maxSteps = (* 10)}

type Eval a = ErrorT String Identity a

class Eq a => Reducible a where
  loReduce :: a -> Maybe a

  lgh :: a -> Int

  evalWith :: ReducibleArgs -> a -> Eval a
  evalWith args x
    | length trace < steps = return $ fromJust $ last trace
    | otherwise = throwError "Can't be reduced!"
    where steps = maxSteps args $ lgh x
          trace = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x

  eval :: a -> Either String a
  eval = runIdentity . runErrorT . evalWith stdReducibleArgs
