module PureLambda where

import Text.Parsec
import qualified Text.Parsec.Token as T
import qualified Data.Map as Map
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Data.List (union, delete)
import Test.QuickCheck
import Data.Maybe (fromJust)
import Control.Monad (liftM, liftM2)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative((<*))
import System.IO

data CalculusSettings = CalculusSettings {maxSteps :: Int -> Int}

type Ide = String
data TermL = Var Ide | App TermL TermL | Abs Ide TermL deriving Eq -- lambda term
data TermA = Asg Ide TermL deriving Eq                             -- assignment term
type LambdaCalculus = Either TermL TermA
type LambdaState = Map.Map Ide TermL

stdCalculusSettings = CalculusSettings {maxSteps = (* 1000)}

absOpr = "."
absHead = "\\"
appOpr = " "

fullForm :: TermL -> String
fullForm (Var x) = x
fullForm (Abs x (Var y)) = absHead ++ x ++ absOpr ++ y
fullForm (Abs x y) = absHead ++ x ++ absOpr ++ "(" ++ fullForm y ++ ")"
fullForm (App x y) = helper x ++ appOpr ++ helper y
  where helper (Var a) = a
        helper a = "(" ++ fullForm a ++ ")"

simpleForm :: TermL -> String
simpleForm (Var x) = x
simpleForm (Abs x (Var y)) = absHead ++ x ++ absOpr ++ y
simpleForm (Abs x y) = absHead ++ x ++ absOpr ++ simpleForm y
simpleForm (App x y) = helper1 x ++ appOpr ++ helper2 y False
  where helper1 (Var a) = a
        helper1 (App x y) = helper1 x ++ appOpr ++ helper2 y True
        helper1 a = "(" ++ simpleForm a ++ ")"
        helper2 (Var a) _ = a
        helper2 a@(Abs _ _) False = simpleForm a
        helper2 a@(Abs _ _) True = "(" ++ simpleForm a ++ ")"
        helper2 a _ = "(" ++ simpleForm a ++ ")"

instance Show TermL where
  show = simpleForm

instance Show TermA where
  show (Asg id t) = id ++ " = " ++ show t

-- Grammar is:
-- var = letter, { letter | digit | "_" };
-- term = chain_term, {chain_term};
-- chain_term = var
--           | "(", term, ")"
--           | "\", var, ".", term;


-- Lexer
-- pureLambdaDef = emptyDef {T.reservedOpNames = ["=",".","\\"]}
  
lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
parens     = T.parens lexer
identifier = T.identifier lexer
symbol     = T.symbol lexer
-- reservedOp = T.reservedOp lexer

-- Parser
varParser :: Parser TermL
varParser = liftM Var identifier

termParser :: Parser TermL
termParser = do
  ls <- many1 chainTermLParser
  return $ foldl1 App ls

chainTermLParser :: Parser TermL
chainTermLParser = varParser <|> parens termParser <|>
                  do
                    symbol "\\"
                    m <- identifier
                    symbol "."
                    t <- termParser
                    return $ Abs m t

assignmentParser :: Parser TermA
assignmentParser = do
  m <- identifier
  symbol "="
  t <- termParser
  return $ Asg m t

lambdaCalculusParser :: Parser LambdaCalculus
lambdaCalculusParser = try (liftM Right assignmentParser) <|> liftM Left termParser

calculusParser :: Parser LambdaCalculus
calculusParser = whiteSpace >> lambdaCalculusParser <* eof

lambdaParser :: Parser TermL
lambdaParser = whiteSpace >> termParser <* eof

parseLambda' :: String -> TermL
parseLambda' = helper . runParser lambdaParser () ""
  where helper (Left err) = error $ show err
        helper (Right x) = x

parseLambda :: String -> Either String TermL
parseLambda = helper . runParser lambdaParser () ""
  where helper (Left err) = Left $ show err
        helper (Right x) = Right x

parseCalculus :: String -> Either String LambdaCalculus
parseCalculus = helper . runParser calculusParser () ""
  where helper (Left err) = Left $ show err
        helper (Right x) = Right x

parseCalculus' :: String -> LambdaCalculus
parseCalculus' = helper . runParser calculusParser () ""
  where helper (Left err) = error $ show err
        helper (Right x) = x


-- Theory part

freeVars :: TermL -> [Ide]
freeVars (Var x)     = [x]
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Abs x t)   = delete x $ freeVars t

isClosed :: TermL -> Bool
isClosed = null . freeVars

subst :: TermL -> Ide -> TermL -> TermL
subst n x m@(Var y)
  | x == y    = n
  | otherwise = m
subst n x (App p q) = App (subst n x p) (subst n x q)
subst n x m@(Abs y p)
  | x == y            = m
  | x `notElem` freeP = m
  | y `notElem` freeN = Abs y (subst n x p)
  | otherwise         = Abs z $ subst n x $ subst (Var z) y p
  where freeP = freeVars p
        freeN = freeVars n
        freeNP = freeP `union` freeN
        z = genNewIde freeNP

genNewIde :: [Ide] -> Ide
genNewIde ids = head $ filter (`notElem` ids) allWords

allWords :: [String]
allWords = concat $ iterate addPrefix initVars
  where addPrefix s = [a:b | a <- alphabet, b<-s]
        initVars = map (: []) alphabet
        alphabet = ['a'..'z']

alphaCongruent :: TermL -> TermL -> Bool
alphaCongruent (Var x) (Var y) = x == y
alphaCongruent (App x1 y1) (App x2 y2) = alphaCongruent x1 x2 && alphaCongruent y1 y2
alphaCongruent (Abs x tx) (Abs y ty)
  | x == y = alphaCongruent tx ty
  | otherwise = alphaCongruent (subst (Var z) x tx) (subst (Var z) y ty)
  where z = genNewIde $ freeVars tx `union` freeVars ty

-- Reduce code

loReduce (Var _) = Nothing -- Leftmost Outmost Reduce
loReduce (Abs x t'@(App t (Var y)))
  | x == y && (x `notElem` freeVars t) = Just t --eta conversion
  | otherwise =
    case loReduce t' of
      Just t'' -> Just $ Abs x t''
      Nothing -> Nothing
loReduce (App (Abs x t1) t2) = Just $ subst t2 x t1 --beta reduction
loReduce (App t1 t2) =
  case loReduce t1 of
    Just t1' -> Just $ App t1' t2
    Nothing ->
      case loReduce t2 of
        Just t2' -> Just $ App t1 t2'
        Nothing -> Nothing
loReduce (Abs x t) =
  case loReduce t of
    Just t' -> Just $ Abs x t'
    Nothing -> Nothing

lgh (Var _) = 1
lgh (App t1 t2) = lgh t1 + lgh t2
lgh (Abs _ t) = 1 + lgh t

limitedReduce :: (Int -> Int) -> TermL -> Maybe TermL
limitedReduce stepFunc x
  | length trace < steps = last trace
  | otherwise            = Nothing
  where steps = stepFunc $ lgh x
        trace = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x

exhaustedIterate :: Eq a => (a -> a) -> a -> a
exhaustedIterate f x = helper trace
  where trace = iterate f x
        helper (x:xs@(y:_))
          | x == y = x
          | otherwise = helper xs

replaceFreeVars :: LambdaState -> TermL -> TermL
replaceFreeVars state x = exhaustedIterate (helper []) x
  where helper scope v@(Var x)
          | x `elem` scope || Map.notMember x state = v
          | otherwise = state Map.! x
        helper scope (App f p) = App (helper scope f) (helper scope p)
        helper scope (Abs x t) = Abs x (helper (x:scope) t)

cSucc = parseLambda' "\\n.\\f.\\x.f (n f x)"
cPlus = parseLambda' "\\m.\\n.\\f.\\x.m f (n f x)"

churchNumeral :: Int -> TermL
churchNumeral n = Abs "f" (Abs "x" result)
  where result = helper (App (Var "f")) n (Var "x")
        helper f 0 x = x
        helper f m x = helper f (m - 1) (f x)

-- Test code

instance Arbitrary TermL where
  arbitrary = sized arbTermL
    where arbChar = elements ['a'..'z']
          arbTermL 0 = liftM (Var . (:[])) arbChar
          arbTermL 1 = arbTermL 0
          arbTermL n = oneof [liftM2 Abs (liftM (:[]) arbChar) (arbTermL (n-1)),
                     liftM2 App (arbTermL nd2) (arbTermL (n - nd2))]
            where nd2 = n `div` 2

propParse :: TermL -> Property
propParse t = classify (lgh t <= 5) "trivial"
                  (parseLambda' str == t &&
                   simpleForm (parseLambda' str) == str)
  where str = show t

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
readExpr input = case parse calculusParser "Lambda Calculus" input of
  Left err -> throwError $ show err
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
  if (input == ":q")
    then return ()
    else
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
