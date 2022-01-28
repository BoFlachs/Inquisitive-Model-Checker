
\subsection{Models}\label{sec:Models}
In this subsection we discuss the implementation of models.

\begin{code}

module InqBModels where

import HelperFunctions
import Test.QuickCheck

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

-- Type declarations for Propositions
type Prop     = [[World]]
type InfState = [World]


myWorlds :: [World]
myWorlds = [1..4]

myIndividuals :: [Individual]
myIndividuals = ["a","b","c","d"]

myR' :: UnRelation
myR' = [(1,["a","b"]), (2,[""]), (3,["b"]), (4,[])]

instance Arbitrary Model where
  arbitrary = do
    u <- suchThat (sublistOf myWorlds) (not . null) 
    d <- suchThat (sublistOf myIndividuals) (not . null) 
    ur <- replicate 1 <$> (zip u <$> suchThat (sublistOf $ powerset d) (\x -> length x == length d))
    let br = pure []
    let tr = pure []
    return (Mo u d ur br tr)
  -- arbitrary = randomModel where
    -- randomModel :: Gen Model
    -- randomModel = Mo <$> u <*> d <*> ur <*> br <*> tr 
    --   where u = suchThat (sublistOf myWorlds) (not . null) 
    --         d = suchThat (sublistOf myIndividuals) (not . null) 
    --         ur = replicate 1 <$> (zip <$> u <*> (repeat <$> d))
    --         br = pure [] 
    --         tr = pure []


\end{code}