module PureLambda where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Data.List (union, delete)
import Calculus
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

type Ide = String
data Term = Var Ide | App Term Term | Abs Ide Term deriving Eq

absOpr = "."
absHead = "\\"
appOpr = " "

fullForm :: Term -> String
fullForm (Var x) = x
fullForm (Abs x (Var y)) = absHead ++ x ++ absOpr ++ y
fullForm (Abs x y) = absHead ++ x ++ absOpr ++ "(" ++ fullForm y ++ ")"
fullForm (App x y) = helper x ++ appOpr ++ helper y
  where helper (Var a) = a
        helper a = "(" ++ fullForm a ++ ")"

simpleForm :: Term -> String
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

instance Show Term where
  show = simpleForm

-- Grammar is:
-- var = letter, { letter | digit | "_" };
-- term = chain_term, {chain_term};
-- chain_term = var
--           | "(", term, ")"
--           | "\", var, ".", term;


-- Lexer
lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
symbol     = T.symbol lexer
parens     = T.parens lexer
identifier = T.identifier lexer

-- Parser
varParser :: Parser Term
varParser = do
  var <- identifier;
  return $ Var var

termParser :: Parser Term
termParser = do
  ls <- many1 chainTermParser
  return $ foldl1 App ls

chainTermParser :: Parser Term
chainTermParser = varParser <|> parens termParser <|>
                  do
                    symbol "\\"
                    m <- identifier
                    symbol "."
                    t <- termParser
                    return $ Abs m t

lambdaParser :: Parser Term
lambdaParser = whiteSpace >> termParser

parseLambda :: String -> Term
parseLambda = helper . runParser lambdaParser () ""
  where helper (Left err) = error $ show err
        helper (Right x) = x

-- Theory part

freeVars :: Term -> [Ide]
freeVars (Var x) = [x]
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Abs x t) = delete x $ freeVars t

isClosed :: Term -> Bool
isClosed = null . freeVars

subst :: Term -> Ide -> Term -> Term
subst n x m@(Var y)
  | x == y = n
  | otherwise = m
subst n x (App p q) = App (subst n x p) (subst n x q)
subst n x m@(Abs y p)
  | x == y = m
  | x `notElem` freeP = m
  | y `notElem` freeN = Abs y (subst n x p)
  | otherwise = Abs z $ subst n x $ subst (Var z) y p
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

alphaCongruent :: Term -> Term -> Bool
alphaCongruent (Var x) (Var y) = x == y
alphaCongruent (App x1 y1) (App x2 y2) = alphaCongruent x1 x2 && alphaCongruent y1 y2
alphaCongruent (Abs x tx) (Abs y ty)
  | x == y = alphaCongruent tx ty
  | otherwise = alphaCongruent (subst (Var z) x tx) (subst (Var z) y ty)
  where z = genNewIde $ freeVars tx `union` freeVars ty

-- Leftmost Outmost Reduce
instance Reducible Term where
  loReduce (Var _) = Nothing
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

cSucc = parseLambda "\\n.\\f.\\x.f (n f x)"
cPlus = parseLambda "\\m.\\n.\\f.\\x.m f (n f x)"

churchNumeral :: Int -> Term
churchNumeral n = Abs "f" (Abs "x" result)
  where result = helper (App (Var "f")) n (Var "x")
        helper f 0 x = x
        helper f m x = helper f (m - 1) (f x)

-- Test code

instance Arbitrary Term where
  arbitrary = sized arbTerm
    where arbChar = elements ['a'..'z']
          arbTerm 0 = liftM (Var . (:[])) arbChar
          arbTerm 1 = arbTerm 0
          arbTerm n = oneof [liftM2 Abs (liftM (:[]) arbChar) (arbTerm (n-1)),
                     liftM2 App (arbTerm nd2) (arbTerm (n - nd2))]
            where nd2 = n `div` 2

propParse :: Term -> Property
propParse t = classify (lgh t <= 5) "trivial"
                  (parseLambda str == t &&
                   simpleForm (parseLambda str) == str)
  where str = show t
