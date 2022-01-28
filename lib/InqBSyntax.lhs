
\subsection{Syntax}\label{sec:InqBSyntax}
In this subsection we discuss the implementation of the syntax of InqB in Haskell.

\begin{code}

module InqBSyntax where

import HelperFunctions
import InqBModels
import Test.QuickCheck

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

newtype ModelWithForm = MWF (Model, Form) deriving Show

instance Arbitrary ModelWithForm where
    arbitrary = do
      u <- suchThat (sublistOf myWorlds) (not . null) 
      d <- suchThat (sublistOf myIndividuals) (not . null) 
      ur <- replicate 1 <$> (zip u <$> suchThat (sublistOf $ powerset d) (\x -> length x == length d))
      let br = pure [(w,[])| w <- u]
      let tr = pure [(w,[])| w <- u] 
      let model = Mo u d ur br tr
      form <- sized (randomForm model) 
      return (MWF (model, form)) where 
        randomForm :: Model -> Int -> Gen Form
        randomForm m 0 = UnR <$> elements (unRel m) 
                       <*> elements (map Indv (dom m))
        randomForm m n = oneof 
            [ UnR   <$> elements (unRel m) 
                    <*> elements (map Indv (dom m))
            -- , BinR  <$> elements (biRel m) 
            --         <*> elements (map Indv (dom m)) 
            --         <*> elements (map Indv (dom m))
            -- , TertR <$> elements (tertRel m) 
            --         <*> elements (map Indv (dom m)) 
            --         <*> elements (map Indv (dom m))
            --         <*> elements (map Indv (dom m))
            , Neg   <$> randomForm m (n `div` 4)
            , Con   <$> randomForm m (n `div` 4)
                    <*> randomForm m (n `div` 4)
            , Dis   <$> randomForm m (n `div` 4)
                    <*> randomForm m (n `div` 4)
            , Impl  <$> randomForm m (n `div` 4)
                    <*> randomForm m (n `div` 4)
            -- Implement quantifier functions     
            ]
      
\end{code}