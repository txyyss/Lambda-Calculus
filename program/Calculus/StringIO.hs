{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calculus.StringIO where

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
  outputStr s = modify $ \(input, output) -> (input, s:output)

runMockIO :: [String] -> MockIO a -> [String]
runMockIO input mockIO = reverse . snd $ execState mockIO (input, [])
