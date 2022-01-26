\section{Example models}
\label{sec: Placeholder}
In this section we create example models


\begin{code}
module Examples where

import InqB

myR :: UnRelation
myR = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myModel :: Model
myModel = Mo
    -- Universe 
    [1, 2,
     3, 4]
    -- Domain 
    ["a", "b"]
    -- Unary relations
    [myR]
    -- BiRelation
    []
    -- TertRelation
    []

\end{code}