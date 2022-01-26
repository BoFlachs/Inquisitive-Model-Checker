
\section{Model Checker}
\label{sec: Placeholder}
In this section we discuss the model checker

\begin{code}
module Main where

import InqB
import Examples

main :: IO()
main = do putStrLn "Hello!"

-- Model checker
supportsProp :: InfState -> Prop -> Bool
supportsProp s p = s `elem` p 

supportsForm :: Model -> InfState -> Form -> Bool
supportsForm m s f = supportsProp s $ toProp m f

testExample :: Bool 
testExample = supportsForm myModel [1,2] (UnR myR "a")

isInquisitive :: Model -> Form -> Bool 
isInquisitive m f = length (alt m f) /= 1 

isInformative :: Model -> Form -> Bool 
isInformative m f = universe m /= info m f 

isTautology :: Model -> Form -> Bool 
isTautology m f = (powerset . universe) m == toProp m f  

entails :: Model -> Form -> Form -> Bool
entails m f1 f2 = all (`elem` p2) p1 where
             p1 = toProp m f1
             p2 = toProp m f2

isEquivalent :: Model -> Form -> Form -> Bool 
isEquivalent m f g = toProp m f == toProp m g

makesTrue :: Model -> World -> Form -> Bool 
makesTrue = undefined 

\end{code}

