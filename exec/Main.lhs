
\section{Model Checker}
\label{sec: Placeholder}
In this section we discuss the model checker

\begin{code}
module Main where

import InqB
import Examples

main :: IO()
main = do putStrLn "Hello!"

-- Functions that should be moved to Main.lhs
makesPropTrue :: InfState -> Prop -> Bool
makesPropTrue i p = i `elem` p 

makesFormTrue :: Model -> InfState -> Form -> Bool
makesFormTrue m i f = i `elem` toProp m f

test :: UnRelation  
test = myR

\end{code}

