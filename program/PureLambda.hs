module PureLambda where

type Ide = String
data Term = Var Ide | App Term Term | Abs Ide Term deriving Eq

absOpr = " -> "
absHead = "\\"
appOpr = " "

instance Show Term where
  show (Var x) = x
  show (Abs x (Var y)) = absHead ++ x ++ absOpr ++ y
  show (Abs x y) = absHead ++ x ++ absOpr ++ show y
  show (App x y) = helper1 x ++ appOpr ++ helper2 y
    where helper1 (Var a) = a
          helper1 a@(App _ _) = show a
          helper1 a = "(" ++ show a ++ ")"
          helper2 (Var a) = a
          helper2 a = "(" ++ show a ++ ")"

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

