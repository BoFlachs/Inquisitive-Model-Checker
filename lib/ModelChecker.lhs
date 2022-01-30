
\subsection{Model Checker}\label{sec:Modelchecker}
In this subsection we discuss the implementation of the syntax of model checker in Haskell.

\begin{code}
module ModelChecker where

import InqBModels
import InqBSyntax
import InqBSemantics

-- Model checker
supportsProp :: InfState -> Prop -> Bool
supportsProp s p = s `elem` p 

supportsForm :: Model -> InfState -> Form -> Bool
supportsForm m s f = supportsProp s $ toProp m f

makesTrue :: Model -> World -> Form -> Bool 
makesTrue m w f = [w] `elem` toProp m f
 
\end{code}