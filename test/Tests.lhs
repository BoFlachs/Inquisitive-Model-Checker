\section{Simple Tests}
\label{sec:simpletests}
In this section we use QuickCheck to test some theorems from los bookos.

\begin{code}

module Main where

import InqBModels
import InqBSyntax
import InqBSemantics
import ModelChecker
import Examples
import HelperFunctions ( powerset )
import Data.List
import Test.QuickCheck
import Test.Hspec

main :: IO()
main = hspec $ do
    describe "Basics" $ do
        it "Trivial model test" $
            property trivialModelTest

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

testProperty :: ModelWithForm -> Bool 
testProperty (MWF (m, f)) = isEquivalent m (nonInf f) (Dis f $ Neg f)

example :: Bool 
example = supportsForm myModel [1,2] (UnR myR (Indv "a"))

\end{code}


