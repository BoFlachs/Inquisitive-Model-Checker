
\subsection{Helper functions}\label{sec:helper}
In this subsection we discuss some helper functions that we implemented

\begin{code}

module HelperFunctions where

import InqBModels
import InqBSyntax
import Data.List

powerset :: [a] -> [[a]]
powerset []  = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

getString :: Term -> String 
getString (Indv i) = i
getString (Var v)  = v

strictSubset :: Eq a => [a] -> [a] -> Bool
strictSubset x y | null (x \\ y) && x /= y = True
                 | otherwise              = False

closeDownward :: [[World]] -> Prop
closeDownward = nub . concatMap powerset

\end{code}