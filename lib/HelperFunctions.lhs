
\subsection{Helper functions}\label{sec:helper}
In this subsection we discuss some helper functions that we implemented

\begin{code}

module HelperFunctions where

import Data.List

powerset :: [a] -> [[a]]
powerset []  = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


strictSubset :: Eq a => [a] -> [a] -> Bool
strictSubset x y | null (x \\ y) && x /= y = True
                 | otherwise              = False

closeDownward :: Eq a => [[a]] -> [[a]]
closeDownward = nub . concatMap powerset

\end{code}