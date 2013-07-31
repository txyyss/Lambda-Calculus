module Calculus.PureLambda
       (Ide,
        LambdaCalculus,
        LambdaState,
        TermA(..),
        TermL(..),
        freeVars,
        fullForm,
        lgh,
        limitedReduce,
        replaceFreeVars,
        simpleForm) where

import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Ide            = String
data TermL          = Var Ide | App TermL TermL | Abs Ide TermL deriving Eq -- lambda term
data TermA          = Asg Ide TermL deriving Eq                             -- assignment term
type LambdaCalculus = Either TermL TermA
type LambdaState    = Map.Map Ide TermL

absOpr  = "."
absHead = "\\"
appOpr  = " "

fullForm :: TermL -> String
fullForm (Var x)         = x
fullForm (Abs x (Var y)) = absHead ++ x ++ absOpr ++ y
fullForm (Abs x y)       = absHead ++ x ++ absOpr ++ "(" ++ fullForm y ++ ")"
fullForm (App x y)       = helper x ++ appOpr ++ helper y
  where helper (Var a)   = a
        helper a         = "(" ++ fullForm a ++ ")"

simpleForm :: TermL -> String
simpleForm (Var x)                = x
simpleForm (Abs x (Var y))        = absHead ++ x ++ absOpr ++ y
simpleForm (Abs x y)              = absHead ++ x ++ absOpr ++ simpleForm y
simpleForm (App x y)              = helper1 x ++ appOpr ++ helper2 y False
  where helper1 (Var a)           = a
        helper1 (App x y)         = helper1 x ++ appOpr ++ helper2 y True
        helper1 a                 = "(" ++ simpleForm a ++ ")"
        helper2 (Var a) _         = a
        helper2 a@(Abs _ _) False = simpleForm a
        helper2 a@(Abs _ _) True  = "(" ++ simpleForm a ++ ")"
        helper2 a _               = "(" ++ simpleForm a ++ ")"

instance Show TermL where
  show = simpleForm

instance Show TermA where
  show (Asg id t) = id ++ " = " ++ show t

-- Theory part

freeVars :: TermL -> Set Ide
freeVars (Var x)     = Set.singleton x
freeVars (App t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (Abs x t)   = Set.delete x $ freeVars t

isClosed :: TermL -> Bool
isClosed = Set.null . freeVars

subst :: TermL -> Ide -> TermL -> TermL
subst n x m@(Var y)
  | x == y             = n
  | otherwise          = m
subst n x (App p q)    = App (subst n x p) (subst n x q)
subst n x m@(Abs y p)
  | x == y             = m
  | x `Set.notMember` freeP  = m
  | y `Set.notMember` freeN  = Abs y (subst n x p)
  | otherwise          = Abs z $ subst n x $ subst (Var z) y p
  where freeP          = freeVars p
        freeN          = freeVars n
        freeNP         = freeP `Set.union` freeN
        z              = genNewIde freeNP

genNewIde :: Set Ide -> Ide
genNewIde ids = head $ filter (`Set.notMember` ids) allWords

allWords :: [String]
allWords            = concat $ iterate addPrefix initVars
  where addPrefix s = [a:b | a <- alphabet, b<-s]
        initVars    = map (: []) alphabet
        alphabet    = ['a'..'z']

alphaCongruent :: TermL -> TermL -> Bool
alphaCongruent (Var x) (Var y)          = x == y
alphaCongruent (App x1 y1) (App x2 y2)  = alphaCongruent x1 x2 && alphaCongruent y1 y2
alphaCongruent (Abs x tx) (Abs y ty)
  | x == y                              = alphaCongruent tx ty
  | otherwise                           = alphaCongruent (subst (Var z) x tx) (subst (Var z) y ty)
  where z                               = genNewIde $ freeVars tx `Set.union` freeVars ty

-- Reduce code

-- Leftmost Outmost Reduce

loReduce :: TermL -> Maybe TermL
loReduce (Var _) = Nothing
loReduce (Abs x t'@(App t (Var y)))
  | x == y && (x `Set.notMember` freeVars t) = Just t --eta conversion
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

lgh :: TermL -> Int
lgh (Var _)     = 1
lgh (App t1 t2) = lgh t1 + lgh t2
lgh (Abs _ t)   = 1 + lgh t

limitedReduce :: (Int -> Int) -> TermL -> [TermL]
limitedReduce stepFunc x
  | length trace < steps = map fromJust trace
  | otherwise            = []
  where steps            = stepFunc $ lgh x
        trace            = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x

exhaustedIterate :: Eq a => (a -> a) -> a -> a
exhaustedIterate f x   = helper trace
  where trace          = iterate f x
        helper (x:xs@(y:_))
          | x == y     = x
          | otherwise  = helper xs

replaceFreeVars :: LambdaState -> TermL -> TermL
replaceFreeVars state x                             = exhaustedIterate (helper []) x
  where helper scope v@(Var x)
          | x `elem` scope || Map.notMember x state = v
          | otherwise                               = state Map.! x
        helper scope (App f p)                      = App (helper scope f) (helper scope p)
        helper scope (Abs x t)                      = Abs x (helper (x:scope) t)
