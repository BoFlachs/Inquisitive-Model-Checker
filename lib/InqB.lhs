
\section{InqB in Haskell}\label{sec:Placeholder2}
In this section we discuss the implementation of InqB in Haskell.

\begin{code}

module InqB where

import Data.List

-- Type declarations of the models
type World        = Int
type Universe     = [World]
type Individual   = String
type Domain       = [Individual]
type UnRelation   = [(World, [Individual])]
type BiRelation   = [(World, [(Individual, Individual)])]
type TertRelation = [(World, [(Individual, Individual, Individual)])]

data Model = Mo { universe :: Universe
                , dom :: Domain
                , unRel :: [UnRelation]
                , biRel :: [BiRelation]
                , tertRel :: [TertRelation] }
                deriving (Eq, Ord, Show)

myR :: UnRelation
myR = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myModel :: Model
myModel = Mo
    -- Universe 
    [1, 2,
     3, 4]
    -- Domain 
    ["a", "b"]
    -- Unary relations
    [myR]
    -- BiRelation
    []
    -- TertRelation
    []

-- Type declarations for Propositions
type Prop     = [[World]]
type InfState = [World]

{- This is needed if we want to introduce quantifiers.
type Var = String
type Vars = [Var]
-}

-- Type declarations for formulas
data Form = UnR UnRelation Individual
          | BinR BiRelation Individual Individual
          | TertR TertRelation Individual Individual Individual
          | Neg Form | Con Form Form | Dis Form Form
          | Impl Form Form
--        | Forall Var Form | Exists Var Form
          deriving (Eq, Ord, Show)

-- Functions working on formulas
powerset :: [a] -> [[a]]
powerset []  = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

nonInq :: Form -> Form
nonInq = Neg . Neg

nonInf :: Form -> Form
nonInf f = Dis f $ Neg f

toProp :: Model -> Form -> Prop
toProp m (UnR r i) = closeDownward [[x |(x, y) <- r, i `elem` y]]
toProp m (BinR r i1 i2) = closeDownward [[x |(x, y) <- r, (i1,i2) `elem` y]]
toProp m (TertR r i1 i2 i3) = closeDownward [[x | (x, y) <- r, (i1,i2,i3) `elem` y]]
toProp m (Neg f) = absPseudComp m (toProp m f)
toProp m (Con f1 f2) = toProp m f1 `intersect` toProp m f2
toProp m (Dis f1 f2) = toProp m f1 `union` toProp m f2
toProp m (Impl f1 f2) = relPseudComp m (toProp m f1) (toProp m f2)

absPseudComp :: Model -> Prop -> Prop
absPseudComp m p = powerset $ universe m \\ (nub . concat) p

closeDownward :: [[World]] -> Prop
closeDownward = nub . concatMap powerset

myProp1 :: Prop
myProp1 = closeDownward [[1,2]]

myProp2 :: Prop
myProp2 = closeDownward [[1,3]]

relPseudComp :: Model -> Prop -> Prop -> Prop
relPseudComp m p q = filter (all (\t -> t `notElem` p || t `elem` q) . powerset )
                                  $ powerset $ universe m

strictSubset :: InfState -> InfState -> Bool
strictSubset x y | null (x \\ y) && x /= y = True
                 | otherwise              = False

alt :: Model -> Form -> [InfState]
alt m f = [x | x <- p, not (any (strictSubset x) p)]
      where p = toProp m f

info :: Model -> Form -> InfState
info m f = nub . concat $ toProp m f

func :: Int -> Int
func n = (*5) n

\end{code}