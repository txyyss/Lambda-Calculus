module Calculus where

import Data.Maybe (fromJust)

class Eq a => Reducible a where
  loReduce :: a -> Maybe a

  riskEval :: a -> a
  riskEval = fromJust . last . takeWhile (/=Nothing) . iterate (>>= loReduce) . Just
