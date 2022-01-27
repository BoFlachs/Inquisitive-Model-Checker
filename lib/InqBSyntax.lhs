
\subsection{Syntax}\label{sec:InqBSyntax}
In this subsection we discuss the implementation of the syntax of InqB in Haskell.

\begin{code}

module InqBSyntax where

import InqBModels
-- import Test.QuickCheck

-- Type declarations for variables
type Var          = String
type Vars         = [Var]

-- Call this terms
data Term       = Indv Individual | Var Var 
        deriving (Eq, Ord, Show)

-- Type declarations for formulas
data Form = UnR UnRelation Term
          | BinR BiRelation Term Term
          | TertR TertRelation Term Term Term
          | Neg Form | Con Form Form | Dis Form Form
          | Impl Form Form
          | Forall Var Form | Exists Var Form
          deriving (Eq, Ord, Show)


nonInq :: Form -> Form
nonInq = Neg . Neg

nonInf :: Form -> Form
nonInf f = Dis f $ Neg f

{--- Define data ModelWithForm =  MWF (Model, Form)
-- Create arbitrary instance for that

myR' :: UnRelation
myR' = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myModel' :: Model
myModel' = Mo
    -- Universe 
    [1, 2,
     3, 4]
    -- Domain 
    ["a", "b"]
    -- Unary relations
    [myR']
    -- BiRelation
    []
    -- TertRelation
    []

instance Arbitrary Form where
  arbitrary = sized randomForm where
    randomForm :: Int -> Gen Form
    randomForm 0 = UnR <$> elements (unRel myModel') 
                       <*> elements (map Var (dom myModel'))
    randomForm n = oneof 
      [ UnR   <$> elements (unRel myModel') 
              <*> elements (map Var (dom myModel'))
      , BinR  <$> elements (biRel myModel') 
              <*> elements (map Var (dom myModel')) 
              <*> elements (map Var (dom myModel'))
      , TertR <$> elements (tertRel myModel') 
              <*> elements (map Var (dom myModel')) 
              <*> elements (map Var (dom myModel'))
              <*> elements (map Var (dom myModel'))
      , Neg   <$> randomForm (n `div` 2)
      , Con   <$> randomForm (n `div` 2)
              <*> randomForm (n `div` 2)
      , Dis   <$> randomForm (n `div` 2)
              <*> randomForm (n `div` 2)
      , Impl  <$> randomForm (n `div` 2)
              <*> randomForm (n `div` 2) 
      ]
      -}

\end{code}