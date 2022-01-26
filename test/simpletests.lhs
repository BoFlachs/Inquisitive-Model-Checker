
\section{Simple Tests}
\label{sec:simpletests}
In this section we use QuickCheck to test some theorems from los bookos.

\begin{code}
module Main where

import InqB
-- import InqB()
import Test.QuickCheck

main :: IO()
main = do putStrLn "Hello"

myIndividuals :: Domain
myIndividuals = ["a","b","c"]

myRelation :: UnRelation
myRelation = [(1, ["a"])]

-- NOg niet correct
instance Arbitrary Form where
  arbitrary = sized randomForm where
    randomForm :: Int -> Gen Form
    randomForm 0 = pure $ UnR myRelation "a" --UnR <$> elements myIndividuals
    randomForm n = oneof 
      [ pure $ UnR myRelation "a" --Prp  <$> elements myAtoms
      , Neg  <$> randomForm (n `div` 2)
      , Con  <$> randomForm (n `div` 2)
             <*> randomForm (n `div` 2)
      , Dis  <$> randomForm (n `div` 2)
             <*> randomForm (n `div` 2)
      , Impl <$> randomForm (n `div` 2)
             <*> randomForm (n `div` 2) 
      ]

trivialTest :: Form -> Bool
trivialTest _ = True


\end{code}


