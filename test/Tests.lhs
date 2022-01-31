\section{QuickCheck}
\label{sec:QuickCheck}
In this section we use QuickCheck to test some theorems from los bookos.

\begin{code}

module Main where

import InqBModels
import InqBSyntax
import InqBSemantics
import HelperFunctions ( powerset )
import Data.List
import Test.QuickCheck
import Test.Hspec

main :: IO()
main = hspec $ do
    describe "Fact 4.12" $ do
        it "!phi equiv neg neg phi" $
            property (\(MWF (m, f))-> isEquivalent m (nonInq f) (Neg (Neg f)) )
        it "?phi equiv phi or (neg phi)" $
            property (\(MWF (m, f))-> isEquivalent m (nonInf f) (Dis f $ Neg f) )
    describe "Fact 4.13" $ do
        it "phi equiv (!phi and ?phi)" $
            property (\(MWF (m, f))-> isEquivalent m f (Con (nonInq f) (nonInf f)) )
    describe "Fact 4" $ do
        it "2. (neg phi) is always non-inquisitive" $
            property (\(MWF (m, f))-> (not . isInquisitive m) (Neg f) )
        it "3. !phi is always non-inquisitive" $
            property (\(MWF (m, f))-> (not . isInquisitive m) (nonInq f) )
    describe "Fact 4.18" $ do
        it "1. ?phi is always non-informative" $
            property (\(MWF (m, f))-> (not . isInformative m) (nonInf f) )

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

\end{code}


