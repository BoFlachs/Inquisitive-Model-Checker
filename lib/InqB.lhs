
\section{InqB in Haskell}\label{sec:Placeholder2}
In this section we discuss the implementation of InqB in Haskell.

\begin{code}
module InqB where

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

-- Type declarations for Proposotions
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

-- Functions working on formulas and propositions
nonInq :: Form -> Form
nonInq = undefined

nonInf :: Form -> Form
nonInf = undefined

toProp :: Form -> Prop
toProp = undefined

absPseudComp :: Prop -> Prop
absPseudComp = undefined

relPseudComp :: Prop -> Prop
relPseudComp = undefined

alt :: Form -> [InfState]
alt = undefined

info :: Form -> InfState
info = undefined


\end{code}

