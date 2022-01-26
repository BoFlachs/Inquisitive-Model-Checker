\section{Example models}
\label{sec: Examples} 
In this section we create example models


\begin{code}
module Examples where

import InqB

myR :: UnRelation
myR = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myVars :: Vars 
myVars = ["x", "y", "z"]

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

\end{code}