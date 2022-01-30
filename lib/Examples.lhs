\section{Example models}
\label{sec: Examples} 
In this section we create example models


\begin{code}
module Examples where

import InqBModels
import InqBSyntax

myR :: UnRelation
myR = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myModel :: Model
myModel = Mo [1, 2, 3, 4] ["a", "b"] [myR] [] []

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

-- myVars2 :: Vars 
-- myVars2 = ["x", "y", "z"]

myModel2 :: Model
myModel2 = Mo
    -- Universe 
    [1, 2,
     3, 4]
    -- Domain 
    ["a", "b"]
    -- Unary relations
    [myR, myR2]
    -- BiRelation
    [myBiR]
    -- TertRelation
    [myTertR]
    

form1 :: Form
form1 = UnR myR (Indv "a")

form2 :: Form
form2 = UnR myR (Indv "b")

form3 :: Form
form3 = Dis (UnR myR (Indv "a")) (UnR myR (Indv "b"))

form4 :: Form
form4 = Neg (UnR myR (Indv "a"))

form5 :: Form
form5 = Neg (Dis (UnR myR (Indv "a")) (UnR myR (Indv "b")))

form6 :: Form
form6 = nonInq (Dis (UnR myR (Indv "a")) (UnR myR (Indv "b")))

form7 :: Form
form7 = nonInf (UnR myR (Indv "a"))

form8 :: Form
form8 = nonInf (UnR myR (Indv "b"))

form9 :: Form
form9 = nonInf (Dis (UnR myR (Indv "a")) (UnR myR (Indv "b")))

form10 :: Form
form10 = nonInf (nonInq (Dis (UnR myR (Indv "a")) (UnR myR (Indv "b"))))

form11 :: Form
form11 = Con (UnR myR (Indv "a")) (UnR myR (Indv "b"))

form12 :: Form
form12 = Con (nonInf (UnR myR (Indv "a"))) (nonInf (UnR myR (Indv "b")))

form13 :: Form
form13 = Impl (UnR myR (Indv "a")) (UnR myR (Indv "b"))

form14 :: Form
form14 = Impl (UnR myR (Indv "a")) (nonInf (UnR myR (Indv "b")))

form15 :: Form
form15 = Forall "x" (nonInf (UnR myR (Var "x")))

form16 :: Form
form16 = Exists "x" (nonInf (UnR myR (Var "x")))


\end{code}