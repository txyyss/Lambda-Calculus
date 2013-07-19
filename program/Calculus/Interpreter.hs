module Calculus.Interpreter where

import Calculus.Parser
import Calculus.PureLambda
import qualified Data.Map as Map
import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import System.IO

data CalculusSettings = CalculusSettings {
  maxSteps :: Int -> Int,
  traceEnabled :: Bool,
  simpleFormEnabled :: Bool}

stdCalculusSettings = CalculusSettings {maxSteps = (* 1000), traceEnabled = False, simpleFormEnabled = True}

-- evaluation code

type InterpM = StateT (LambdaState, CalculusSettings) (ErrorT String Identity)

runInterpM :: (LambdaState, CalculusSettings) -> InterpM a -> Either String (a, (LambdaState, CalculusSettings))
runInterpM gState x = runIdentity . runErrorT $ runStateT x gState

type Value = TermL

class InterpC t where
  interp :: t -> InterpM Value

instance (InterpC t1, InterpC t2) => InterpC (Either t1 t2) where
  interp (Left x)  = interp x
  interp (Right x) = interp x

instance InterpC TermL where
  interp input = do
    (state, settings) <- get
    let replacedInput = replaceFreeVars state input
    case limitedReduce (maxSteps settings) replacedInput of
      [] -> throwError (show input ++ " can't be reduced!")
      x -> return $ last x

instance InterpC TermA where
  interp (Asg v t) = do
    (state,settings) <- get
    if Map.member v state
      then throwError (v ++ " has been defined already!")
      else do
        let freeVs = freeVars t
        if v `elem` freeVs
          then throwError (v ++ " is recursively defined!")
          else if any (`Map.notMember` state) freeVs
               then throwError (show t ++ " contains free variables")
               else do
                 put $ (Map.insert v t state, settings)
                 return t

readExpr :: String -> InterpM LambdaCalculus
readExpr input = case parseCalculus input of
  Left err -> throwError err
  Right x -> return x

interpStr :: String -> InterpM Value
interpStr input = readExpr input >>= interp  

evalOnce :: String -> Either String (Value, (LambdaState, CalculusSettings))
evalOnce input = runInterpM (Map.empty, stdCalculusSettings) (interpStr input)

evalString :: (LambdaState, CalculusSettings) -> String ->  Either String (Value, (LambdaState, CalculusSettings))
evalString state input = runInterpM state (interpStr input)

-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

continueEval :: (LambdaState, CalculusSettings) -> String -> (String, (LambdaState, CalculusSettings))
continueEval state@(_, setting) input =
  case evalString state input of
    Left err -> (err, state)
    Right (v, newS) -> (if (simpleFormEnabled setting) then simpleForm v else fullForm v, newS)

runREPLWith :: (LambdaState, CalculusSettings) -> IO ()
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

stdState :: (LambdaState, CalculusSettings)
stdState = helper $ runInterpM (Map.empty,stdCalculusSettings) $ foldl1 (>>) $ map interpStr stdDefinition
  where helper (Right (_, x)) = x

runREPL = runREPLWith stdState
