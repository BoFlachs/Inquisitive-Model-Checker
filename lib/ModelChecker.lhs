
\subsection{Model Checker}\label{sec:Modelchecker}
Now that we have implemented the models, syntax and 
semantics of \textsf{InqB}, our model checker can be 
implemented. As a result of our algebraic characterization
of \textsf{InqB}'s semantics, the support of a proposition
in an information state comes down to set inclusion.
The function \verb|supportsProp| takes an information
state and a proposition and checks whether that information 
state is included in the proposition. If so, the information 
state supports the propositions, otherwise it does not.

\begin{code}
module ModelChecker where

import InqBModels
import InqBSyntax
import InqBSemantics

supportsProp :: InfState -> Prop -> Bool
supportsProp s p = s `elem` p
\end{code}

Likewise, the function \verb|supportsForm| checks whether an 
information state supports a certain formula. However, this 
can only be checked by transforming the formula into a 
proposition relative to a certain model. Hence \verb|supportsForm| 
also takes a \verb|Model| as an argument.

\begin{code}
supportsForm :: Model -> InfState -> Form -> Bool
supportsForm m s f = supportsProp s $ toProp m f
\end{code}

Lastly, the function \verb|makesTrue| checks whether a formula 
is satisfied in a certain world of a particular model. This comes 
down to checking whether the singleton containing that particular 
world is an element of the proposition corresponding to the 
specified formula. Note that this is just checking whether that formula is 
supported by the information state containing only that world. 

\begin{code}
makesTrue :: Model -> World -> Form -> Bool 
makesTrue m w = supportsForm m [w]
\end{code}