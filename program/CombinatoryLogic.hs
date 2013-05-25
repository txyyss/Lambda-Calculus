module CombinatoryLogic where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Data.List (union)
import Calculus
import qualified PureLambda as P

type Ide = String
data Term = Var Ide | Atom Ide | App Term Term deriving Eq

instance Show Term where
  show (Var x) = x
  show (Atom x) = x
  show (App x y) = show x ++ " " ++ helper y
    where helper x@(App _ _) = "(" ++ show x ++ ")"
          helper x = show x

-- Lexer
combinatoryLogicDef = emptyDef {T.reservedNames = ["S","K","I"]}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser combinatoryLogicDef

whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
symbol     = T.symbol lexer
parens     = T.parens lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer

-- Parser
varParser :: Parser Term
varParser = do
  var <- identifier
  return $ Var var

atomParser :: Ide -> Parser Term
atomParser x = do
  reserved x
  return $ Atom x

termParser :: Parser Term
termParser = do
  ls <- many1 chainTermParser
  return $ foldl1 App ls

chainTermParser :: Parser Term
chainTermParser = atomParser "S" <|>
                  atomParser "K" <|>
                  atomParser "I" <|>
                  varParser <|>
                  parens termParser

clParser :: Parser Term
clParser = whiteSpace >> termParser

parseCL :: String -> Term
parseCL = helper . runParser clParser () ""
  where helper (Left err) = error $ show err
        helper (Right x) = x

-- Theory

freeVars :: Term -> [Ide]
freeVars (Var x) = [x]
freeVars (Atom x) = []
freeVars (App t1 t2) = union (freeVars t1) (freeVars t2)

subst :: Term -> Ide -> Term -> Term
subst u x m@(Var y)
  | x == y = u
  | otherwise = m
subst u x (App p q) = App (subst u x p) (subst u x q)
subst u x m = m

-- Leftmost Outmost Reduce
instance Reducible Term where
  loReduce (Var _) = Nothing
  loReduce (Atom _) = Nothing
  loReduce (App (Atom "I") x) = Just x
  loReduce (App (App (Atom "K") x) y) = Just x
  loReduce (App (App (App (Atom "S") x) y) z) = Just $ App (App x z) (App y z)
  loReduce (App t1 t2) =
    case loReduce t1 of
      Just t1' -> Just $ App t1' t2
      Nothing ->
        case loReduce t2 of
          Just t2' -> Just $ App t1 t2'
          Nothing -> Nothing

abstraction :: Ide -> Term -> Term
abstraction x m
  | x `notElem` (freeVars m) = App (Atom "K") m
  | otherwise = helper m
  where helper (Var y)
          | x == y = Atom "I"
        helper (App u v@(Var y))
          | x == y && x `notElem` (freeVars u) = u
        helper (App u v) = absApp u v
        absApp u v = App (App (Atom "S") (abstraction x u)) (abstraction x v)

lambdaToCL :: P.Term -> Term
lambdaToCL (P.Var x) = Var x
lambdaToCL (P.App m n) = App (lambdaToCL m) (lambdaToCL n)
lambdaToCL (P.Abs x m) = abstraction x (lambdaToCL m)
