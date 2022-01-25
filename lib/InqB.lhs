
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
nonInq = undefined

nonInf :: Form -> Form
nonInf = undefined

-- Functions working on models, formulas and propositions
toProp :: Form -> Prop
toProp = undefined

absPseudComp :: Prop -> Prop
absPseudComp = undefined

closeDownward :: [[World]] -> Prop
closeDownward = nub . concat . map powerset 

myProp1 :: Prop
myProp1 = closeDownward [[1,2]]

myProp2 :: Prop
myProp2 = closeDownward [[1,3]]

relPseudComp :: Model -> Prop -> Prop -> Prop
relPseudComp m p q = filter (\s -> 
                          all (\x -> x `notElem` p || x `elem` q) 
                              $ powerset s ) 
                                  $ powerset $ universe m

alt :: Form -> [InfState]
alt = undefined

info :: Form -> InfState
info = undefined


\end{code}

