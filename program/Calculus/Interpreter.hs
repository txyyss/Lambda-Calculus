module Calculus.Interpreter where

import Calculus.Parser
import Calculus.PureLambda
import Control.Monad (unless)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.List (intercalate)
import System.IO
import qualified Data.Map as Map

data CalculusSettings = CalculusSettings {
  maxSteps :: Int -> Int,
  traceEnabled :: Bool,
  simpleFormEnabled :: Bool}

stdCalculusSettings = CalculusSettings {maxSteps = (* 100), traceEnabled = False, simpleFormEnabled = False}

type CalculusState = (LambdaState, CalculusSettings)

-- evaluation code

type InterpM = StateT CalculusState (ErrorT String Identity)

runInterpM :: CalculusState -> InterpM a -> Either String (a, CalculusState)
runInterpM gState x = runIdentity . runErrorT $ runStateT x gState

type Value = Either String ()
-- type Value = TermL

class InterpC t where
  interp :: t -> InterpM Value

instance (InterpC t1, InterpC t2) => InterpC (Either t1 t2) where
  interp (Left x)  = interp x
  interp (Right x) = interp x

instance InterpC TermL where
  interp input = do
    (state, settings) <- get
    let replacedInput = replaceFreeVars state input
    let outputF = if simpleFormEnabled settings then simpleForm else fullForm
    case limitedReduce (maxSteps settings) replacedInput of
      [] -> throwError (show input ++ "seems can't be reduced!")
      x -> return $ Left (if traceEnabled settings
                          then intercalate "\n" $ map (\t -> "==> " ++ (outputF t)) x
                          else outputF $ last x)

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
                 return $ Right ()

readExpr :: String -> InterpM LambdaCalculus
readExpr input = case parseCalculus input of
  Left err -> throwError err
  Right x -> return x

interpStr :: String -> InterpM Value
interpStr input = readExpr input >>= interp  

evalOnce :: String -> Either String (Value, CalculusState)
evalOnce input = runInterpM (Map.empty, stdCalculusSettings) (interpStr input)

evalString :: CalculusState -> String ->  Either String (Value, CalculusState)
evalString state input = runInterpM state (interpStr input)

-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

runREPLWith :: CalculusState -> IO ()
runREPLWith state = do
  input <- readPrompt "Lambda> "
  unless (input == ":q") $
    do
      case evalString state input of
        Left err -> putStrLn err >> runREPLWith state
        Right (result, newS) ->
          case result of
            Left x -> putStrLn x >> runREPLWith newS
            Right () -> runREPLWith newS

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

stdState :: CalculusState
stdState = helper $ runInterpM (Map.empty,stdCalculusSettings) $ foldl1 (>>) $ map interpStr stdDefinition
  where helper (Right (_, x)) = x

runREPL = runREPLWith stdState
