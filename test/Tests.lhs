\section{Simple Tests}
\label{sec:simpletests}
In this section we use QuickCheck to test some theorems from los bookos.

\begin{code}

module Main where

import InqBModels
import InqBSyntax
import InqBSemantics
import HelperFunctions
import Data.List
-- import Test.QuickCheck
-- import Examples

main :: IO()
main = do putStrLn "Hello"

isInquisitive :: Model -> Form -> Bool 
isInquisitive m f = sort (toProp m f) /= (sort . powerset) (info m f)

isInformative :: Model -> Form -> Bool 
isInformative m f = (sort . universe) m /= sort (info m f) 

isTautology :: Model -> Form -> Bool 
isTautology m f = (sort. powerset . universe) m == sort (toProp m f)  

entails :: Model -> Form -> Form -> Bool
entails m f1 f2 = all (`elem` p2) p1 where
             p1 = toProp m f1
             p2 = toProp m f2

isEquivalent :: Model -> Form -> Form -> Bool 
isEquivalent m f g = sort (toProp m f) == sort (toProp m g)

trivialTest :: Form -> Bool
trivialTest _ = True

trivialModelTest :: Model -> Bool 
trivialModelTest _ = True


\end{code}


