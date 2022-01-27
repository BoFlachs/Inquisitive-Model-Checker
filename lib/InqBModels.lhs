
\subsection{Models}\label{sec:Models}
In this subsection we discuss the implementation of models.

\begin{code}

module InqBModels where

-- import Test.QuickCheck

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

{-
myWorlds :: [World]
myWorlds = [1..4]

myIndiviuals :: [Individual]
myIndiviuals = ["a","b","c","d"]

instance Arbitrary Model where
  arbitrary = randomModel where
    randomModel :: Gen Model
    randomModel = Mo <$> u <*> d <*> ur <*> br <*> tr 
      where u = elements $ powerset myWorlds 
            -- might have a quickcheck functino for this non-empty stuff
            d = elements $ (filter (not . null) . powerset) myIndiviuals 
            ur = pure [myR]-- zip <$> d <*> d
            br = pure [] 
            tr = pure []
-}

\end{code}