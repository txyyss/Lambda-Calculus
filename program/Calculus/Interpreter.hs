module Calculus.Interpreter (evalLambda, runREPL) where

import Calculus.Parser
import Calculus.PureLambda
import Calculus.StringIO
import Control.Monad (unless)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.Char (isDigit, digitToInt)
import Data.List (intercalate, isPrefixOf)
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

data CalculusSettings = CalculusSettings {
  maxSteps          :: Int -> Int,
  traceEnabled      :: Bool,
  simpleFormEnabled :: Bool,
  hold              :: Bool
  }

stdCalculusSettings = CalculusSettings {maxSteps = (* 100), traceEnabled = False, simpleFormEnabled = True, hold = False}

type CalculusState = (LambdaState, CalculusSettings)

-- evaluation code

type InterpreterT m = StateT CalculusState (ErrorT String m)

runInterpreterT :: (Monad m) => CalculusState -> InterpreterT m a -> m (Either String (a, CalculusState))
runInterpreterT state x = runErrorT $ runStateT x state

type Value = Either String ()

class InterpC t where
  interp :: (Monad m) => t -> InterpreterT m Value

instance (InterpC t1, InterpC t2) => InterpC (Either t1 t2) where
  interp (Left x)  = interp x
  interp (Right x) = interp x

instance InterpC TermL where
  interp input = do
    (state, settings) <- get
    let replacedInput = replaceFreeVars state input
    let outputF       = if simpleFormEnabled settings then simpleForm else fullForm
    let evalF         = if hold settings then (\x y -> [y]) else limitedReduce
    case evalF (maxSteps settings) replacedInput of
      [] -> throwError (show input ++ " seems can't be reduced!")
      x  -> return $ Left (if traceEnabled settings
                           then intercalate "\n" $ map (\t -> "==> " ++ outputF t) x
                           else outputF $ last x)

instance InterpC TermA where
  interp (Asg v t) = do
    (state,settings) <- get
    if Map.member v state
      then throwError (v ++ " has been defined already!")
      else do
        let freeVs = freeVars t
        if v `Set.member` freeVs
          then throwError (v ++ " can't be recursively defined!")
          else if Set.member True $ Set.map (`Map.notMember` state) freeVs
               then throwError (show t ++ " contains free variables")
               else do
                 put (Map.insert v t state, settings)
                 return $ Right ()

readExpr :: (Monad m) => String -> InterpreterT m LambdaCalculus
readExpr input = case parseCalculus input of
  Left err -> throwError err
  Right x  -> return x

interpCommand :: (Monad m) => String -> InterpreterT m Value
interpCommand cmd = do
  (state, settings) <- get
  if isPrefixOf "set steps " cmd && all isDigit (drop 10 cmd)
    then do
           let newMaxSteps = foldl1 (\x y -> x * 10 + y) $ map digitToInt $ drop 10 cmd
           put (state, settings {maxSteps = (* newMaxSteps)})
           return $ Right ()
    else case cmd of
           "clear state"    -> put (Map.empty, settings)                         >> return (Right ())
           "reset settings" -> put (state, stdCalculusSettings)                  >> return (Right ())
           "reset state"    -> put (fst stdState, settings)                      >> return (Right ())
           "set +fullform"  -> put (state, settings {simpleFormEnabled = False}) >> return (Right ())
           "set +hold"      -> put (state, settings {hold = True})               >> return (Right ())
           "set +trace"     -> put (state, settings {traceEnabled = True})       >> return (Right ())
           "set -fullform"  -> put (state, settings {simpleFormEnabled = True})  >> return (Right ())
           "set -hold"      -> put (state, settings {hold = False})              >> return (Right ())
           "set -trace"     -> put (state, settings {traceEnabled = False})      >> return (Right ())
           _                -> throwError "Unrecognized command"

interpStr :: (Monad m) => String -> InterpreterT m Value
interpStr (':':cmd) = interpCommand cmd
interpStr input     = readExpr input >>= interp

-- REPL

runLambda :: (StringIO m) => InterpreterT m ()
runLambda = do
  input <- lift $ lift inputStr
  unless (input == quitCommand) $ (
    do
      value <- interpStr input
      case value of
        Left str -> lift $ lift (outputStr str)
        Right () -> return ()
    `catchError` (lift . lift . outputStr))
    >> runLambda

evalLambda :: [String] -> [String]
evalLambda x = execMockIO x $ runInterpreterT (Map.empty, stdCalculusSettings) runLambda

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

instance StringIO IO where
  inputStr = readPrompt "Lambda> "
  outputStr = putStrLn

-- sample definitions

stdDefinition = ["zero   = \\f.\\x.x",
                 "succ   = \\n.\\f.\\x.f (n f x)",
                 "plus   = \\m.\\n.m succ n",
                 "mult   = \\m.\\n.\\f.m (n f)",
                 "pow    = \\b.\\e.e b",
                 "pred   = \\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)",
                 "sub    = \\m.\\n.n pred m",

                 "one    = succ zero",
                 "two    = succ one",
                 "three  = succ two",
                 "four   = succ three",

                 "true   = \\x.\\y.x",
                 "false  = \\x.\\y.y",
                 "and    = \\p.\\q.p q p",
                 "or     = \\p.\\q.p p q",
                 "not    = \\p.\\a.\\b.p b a",
                 "if     = \\p.\\a.\\b.p a b",
                 "iszero = \\n.n (\\x.false) true",
                 "leq    = \\m.\\n.iszero (sub m n)",
                 "eq     = \\m.\\n. and (leq m n) (leq n m)",

                 "Yv     = \\h. (\\x.\\a.h (x x) a) (\\x.\\a.h (x x) a)",
                 "Y      = \\g.(\\x.g (x x)) (\\x.g (x x))",
                 "fracG  = \\r.\\n.if (iszero n) one (mult n (r (pred n)))"
                 ]

stdState :: CalculusState
stdState = helper . fst . runMockIO stdDefinition $ runInterpreterT (Map.empty, stdCalculusSettings) runLambda
  where helper (Right (_, x)) = x

runREPL :: IO ()
runREPL = putStrLn "Lambda Interpreter Version 1.0" >>
          putStrLn "Type \":q\" to quit." >>
          void (runInterpreterT stdState runLambda)
