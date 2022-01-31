
\subsection{Helper functions}\label{sec:helper}
We conclude this section with three helper functions that we 
used in the implementations above. The functions below can be 
used to generate the power set of a list, check whether something
is a strict subset and to give the downward closure of a set of sets, 
respectively. These functions are used in the code treated in sections 
3.2 and 3.3. Finally, note that the \verb|closeDownward| function 
can be used to transform the alternatives of a proposition, $\alt(P)$, 
into the proposition $P$. 
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