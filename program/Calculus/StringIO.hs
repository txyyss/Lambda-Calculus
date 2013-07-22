{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calculus.StringIO where

import Control.Arrow (second)
import Control.Monad.State

quitCommand = ":q"

class (Monad m) => StringIO m where
  inputStr  :: m String
  outputStr :: String -> m ()

type MockIO = State ([String], [String])

instance StringIO MockIO where
  inputStr = do
    s@(input, output) <- get
    case input of
      (s:strs) -> put (strs, output) >> return s
      [] -> return quitCommand
  outputStr s = modify $ second ((:) s)

execMockIO :: [String] -> MockIO a -> [String]
execMockIO input mockIO = reverse . snd $ execState mockIO (input, [])

runMockIO :: [String] -> MockIO a -> (a, ([String], [String]))
runMockIO input mockIO = runState mockIO (input, [])
