\section{Example models}
\label{sec: Examples} 
In this section we create example models


\begin{code}
module Examples where

import InqBModels
import InqBSyntax

myR :: UnRelation
myR = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myR2 :: UnRelation
myR2 = [(1,["a","b"]), (2,["a,b"]), (3,[]), (4,[])]

myBiR :: BiRelation 
myBiR = [(1,[("a","a"),("b","b")]), 
        (2,[("a","a"),("b","b")]), 
        (3,[("c","c"),("b","b")]),
        (4,[("a","a"),("d","d")])]

myTertR :: TertRelation  
myTertR = [(1,[("a","a","b"),("b","b","c")]), 
        (2,[("a","a","d"),("b","b","c")]), 
        (3,[("c","a","c"),("d","b","b")]),
        (4,[("b","a","a"),("a","d","d")])]

myModel2 :: Model
myModel2 = Mo [1, 2, 3, 4] ["a", "b"] [myR, myR2] [myBiR] [myTertR]
    
form6 :: Form
form6 = nonInq (Dis (UnR myR (Indv "a")) (UnR myR (Indv "b")))


\end{code}