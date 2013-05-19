module PureLambda where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

type Ide = String
data Term = Var Ide | App Term Term | Abs Ide Term deriving (Eq, Show)

absOpr = "."
absHead = "\\"
appOpr = " "

-- Lexer

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
symbol     = T.symbol lexer
parens     = T.parens lexer
identifier = T.identifier lexer

composeTerms :: [Term] -> Term -> Term
composeTerms [] x = x
composeTerms ls x = App (foldl1 App ls) x

varParser :: Parser Term
varParser = do
  var <- identifier;
  return $ Var var

termParser :: Parser Term
termParser = try
             (do
                 heads <- many (lexeme chainTermParser)
                 lambdaExpr <- lexeme lambdaTermParser
                 return $ composeTerms heads lambdaExpr)
             <|>
             (do
                 heads <- many1 (lexeme chainTermParser)
                 -- tails <- lexeme lambdaTermParser
                 return $ composeTerms (init heads) (last heads))

chainTermParser :: Parser Term
chainTermParser = varParser <|> (parens termParser)

lambdaTermParser :: Parser Term
lambdaTermParser = 
  do
    lexeme $ symbol "\\"
    m <- lexeme identifier
    lexeme $ symbol "."
    t <- lexeme termParser
    return $ Abs m t

-- instance Show Term where
--   show (Var x) = x
--   show (Abs x (Var y)) = absHead ++ x ++ absOpr ++ y
--   show (Abs x y) = absHead ++ x ++ absOpr ++ show y
--   show (App x y) = helper1 x ++ appOpr ++ helper2 y
--     where helper1 (Var a) = a
--           helper1 a@(App _ _) = show a
--           helper1 a = "(" ++ show a ++ ")"
--           helper2 (Var a) = a
--           helper2 a = "(" ++ show a ++ ")"

lgh :: Term -> Int
lgh (Var _) = 1
lgh (App x y) = lgh x + lgh y
lgh (Abs _ y) = 1 + lgh y

occurs :: Term -> Term -> Bool
p `occurs` q
  | p == q = True
  | otherwise = helper p q
  where helper m (App x y) = (m `occurs` x) || (m `occurs` y)
        helper m (Abs x y) = (m == Var x) || (m `occurs` y)
        helper _ _ = False

