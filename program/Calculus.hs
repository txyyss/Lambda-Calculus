module Calculus where

data ReducibleArgs = ReducibleArgs {maxSteps :: Int -> Int}

stdReducibleArgs = ReducibleArgs {maxSteps = (* 10)}

class Eq a => Reducible a where
  loReduce :: a -> Maybe a

  lgh :: a -> Int

  evalWith :: ReducibleArgs -> a -> Maybe a
  evalWith args x
    | length trace < steps = last trace
    | otherwise = Nothing
    where steps = maxSteps args $ lgh x
          trace = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x

  eval :: a -> Maybe a
  eval = evalWith stdReducibleArgs
