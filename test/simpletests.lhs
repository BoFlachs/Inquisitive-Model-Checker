\section{Simple Tests}
\label{sec:simpletests}
In this section we use QuickCheck to test some theorems from los bookos.

\begin{code}
-- {-# OPTIONS -Wno-orphans #-}

module Main where

import InqB
import Test.QuickCheck
import Examples

main :: IO()
main = do putStrLn "Hello"

testModel :: Model 
testModel = myModel2 

-- Define data ModelWithForm =  MWF (Model, Form)
-- Create arbitrary instance for that

instance Arbitrary Form where
  arbitrary = sized randomForm where
    randomForm :: Int -> Gen Form
    randomForm 0 = UnR <$> elements (unRel testModel) 
                       <*> elements (map Var (dom testModel))
    randomForm n = oneof 
      [ UnR   <$> elements (unRel testModel) 
              <*> elements (map Var (dom testModel))
      , BinR  <$> elements (biRel testModel) 
              <*> elements (map Var (dom testModel)) 
              <*> elements (map Var (dom testModel))
      , TertR <$> elements (tertRel testModel) 
              <*> elements (map Var (dom testModel)) 
              <*> elements (map Var (dom testModel))
              <*> elements (map Var (dom testModel))
      , Neg   <$> randomForm (n `div` 2)
      , Con   <$> randomForm (n `div` 2)
              <*> randomForm (n `div` 2)
      , Dis   <$> randomForm (n `div` 2)
              <*> randomForm (n `div` 2)
      , Impl  <$> randomForm (n `div` 2)
              <*> randomForm (n `div` 2) 
      ]

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


trivialTest :: Form -> Bool
trivialTest _ = True

trivialModelTest :: Model -> Bool 
trivialModelTest _ = True


\end{code}


