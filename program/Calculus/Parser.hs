module Calculus.Parser (parseCalculus, parseLambda') where

import Calculus.PureLambda (Ide, TermL(..), TermA(..), LambdaCalculus, lgh, simpleForm, fullForm)
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Control.Applicative((<*))

-- Grammar of lambda term is:
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
parens     = T.parens lexer
identifier = T.identifier lexer
symbol     = T.symbol lexer

-- Parser
varParser :: Parser TermL
varParser = liftM Var identifier

termParser :: Parser TermL
termParser = chainTermLParser `chainl1` return App

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
parseLambda' = helper . parse lambdaParser ""
  where helper (Left err) = error $ show err
        helper (Right x) = x

-- parseLambda :: String -> Either String TermL
-- parseLambda = helper . parse lambdaParser ""
--   where helper (Left err) = Left $ show err
--         helper (Right x) = Right x

parseCalculus :: String -> Either String LambdaCalculus
parseCalculus = helper . parse calculusParser ""
  where helper (Left err) = Left $ show err
        helper (Right x) = Right x

parseCalculus' :: String -> LambdaCalculus
parseCalculus' = helper . parse calculusParser ""
  where helper (Left err) = error $ show err
        helper (Right x) = x

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
