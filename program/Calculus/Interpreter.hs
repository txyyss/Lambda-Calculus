module Interpreter where

import Parser
import PureLambda
import qualified Data.Map as Map
import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import System.IO

data CalculusSettings = CalculusSettings {maxSteps :: Int -> Int}

stdCalculusSettings = CalculusSettings {maxSteps = (* 1000)}

-- evaluation code

type InterpM = StateT LambdaState (ReaderT CalculusSettings (ErrorT String Identity))

runInterpM :: CalculusSettings -> LambdaState -> InterpM a -> Either String (a, LambdaState)
runInterpM settings state x = runIdentity . runErrorT $ runReaderT (runStateT x state) settings

type Value = TermL

class InterpC t where
  interp :: t -> InterpM Value

instance (InterpC t1, InterpC t2) => InterpC (Either t1 t2) where
  interp (Left x)  = interp x
  interp (Right x) = interp x

instance InterpC TermL where
  interp input = do
    state <- get
    let replacedInput = replaceFreeVars state input
    stepF <- asks maxSteps
    case limitedReduce stepF replacedInput of
      Just x -> return x
      Nothing -> throwError (show input ++ " can't be reduced!")

instance InterpC TermA where
  interp (Asg v t) = do
    state <- get
    if Map.member v state
      then throwError (v ++ " has been defined already!")
      else do
        let freeVs = freeVars t
        if v `elem` freeVs
          then throwError (v ++ " is recursively defined!")
          else if any (`Map.notMember` state) freeVs
               then throwError (show t ++ " contains free variables")
               else do
                 put $ Map.insert v t state
                 return t

readExpr :: String -> InterpM LambdaCalculus
readExpr input = case parseCalculus input of
  Left err -> throwError err
  Right x -> return x

interpStr :: String -> InterpM Value
interpStr input = readExpr input >>= interp  

evalOnce :: String -> Either String (Value, LambdaState)
evalOnce input = runInterpM stdCalculusSettings Map.empty (interpStr input)

evalString :: LambdaState -> String ->  Either String (Value, LambdaState)
evalString state input = runInterpM stdCalculusSettings state (interpStr input)

-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

continueEval :: LambdaState -> String -> (String, LambdaState)
continueEval state input =
  case evalString state input of
    Left err -> (err, state)
    Right (v, newS) -> (show v, newS)

runREPLWith :: LambdaState -> IO ()
runREPLWith state = do
  input <- readPrompt "Lambda> "
  unless (input == ":q") $
    do
      let (result, newS) = continueEval state input
      putStrLn result
      runREPLWith newS

-- sample definitions

stdDefinition = ["zero = \\f.\\x.x",
                 "succ = \\n.\\f.\\x.f (n f x)",
                 "plus = \\m.\\n.m succ n",
                 "mult = \\m.\\n.\\f.m (n f)",
                 "pow = \\b.\\e.e b",
                 "pred = \\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)",
                 "sub = \\m.\\n.n pred m",

                 "one = succ zero",
                 "two = succ one",
                 "three = succ two",
                 "four = succ three",

                 "true = \\x.\\y.x",
                 "false = \\x.\\y.y",
                 "and = \\p.\\q.p q p",
                 "or = \\p.\\q.p p q",
                 "not = \\p.\\a.\\b.p b a",
                 "if = \\p.\\a.\\b.p a b",
                 "iszero = \\n.n (\\x.false) true",
                 "leq = \\m.\\n.iszero (sub m n)",
                 "eq = \\m.\\n. and (leq m n) (leq n m)",

                 "Yv = \\h. (\\x.\\a.h (x x) a) (\\x.\\a.h (x x) a)",
                 "Y = \\g.(\\x.g (x x)) (\\x.g (x x))",
                 "fracG = \\r.\\n.if (iszero n) one (mult n (r (pred n)))"
                 ]

stdState :: LambdaState
stdState = helper $ runInterpM stdCalculusSettings Map.empty $ foldl1 (>>) $ map interpStr stdDefinition
  where helper (Right (_, x)) = x

runREPL = runREPLWith stdState
