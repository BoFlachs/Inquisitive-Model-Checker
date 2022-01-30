
\subsection{Syntax}\label{sec:InqBSyntax}
We now discuss the implementation of the syntax of \textit{InqB}.
We say that a variable is of the type \verb|String|, and a term is 
either an individual or a variable.

\begin{code}
module InqBSyntax where

import HelperFunctions
import InqBModels
import Test.QuickCheck

type Var  = String
data Term = Indv Individual | Var Var 
        deriving (Eq, Ord, Show)
\end{code}

We can then define formulas in a way that is analogous to the Backus-Naur form
for first-order formulas. Note that we do not add a relation symbol in the 
atomic sentences, but the actual relation. As discussed in Section \ref{sec:Models},
this may seem like an unnatural way to define formulas. However, as we 
we will see shortly, it is still intuitive to define one's own formulas in this
way. Furthermore, it allows for a straightforward implementation of arbitrary 
models and formulas.

\begin{code}
data Form = UnR UnRelation Term
          | BinR BiRelation Term Term
          | TertR TertRelation Term Term Term
          | Neg Form | Con Form Form | Dis Form Form
          | Impl Form Form
          | Forall Var Form | Exists Var Form
          deriving (Eq, Ord, Show)
\end{code}
The projection operators ! (\verb|nonInq|) and ? (\verb|nonInf|) can be seen as abbreviations. Therefore we
have implemented them as functions of the type \verb|Form| $\rightarrow$ \verb|Form|.
\begin{code}
nonInq :: Form -> Form
nonInq = Neg . Neg

nonInf :: Form -> Form
nonInf f = Dis f $ Neg f
\end{code}

We can then define an example formula using the example 
relation \verb|myR| from Section \ref{sec:Models}. This formula corresponds
to the \textit{InqB} formula $! (Ra \lor Rb) $.

\begin{code}
myForm :: Form
myForm = nonInq (Dis (UnR myR (Indv "a")) (UnR myR (Indv "b")))
\end{code}

\begin{code}
myWorlds :: [World]
myWorlds = [1..4]

myIndividuals :: [Individual]
myIndividuals = ["a","b","c","d"]

newtype ModelWithForm = MWF (Model, Form) deriving Show

instance Arbitrary ModelWithForm where
    arbitrary = do
      u  <- suchThat (sublistOf myWorlds) (not . null) 
      d  <- suchThat (sublistOf myIndividuals) (not . null) 
      ur <- replicate 1 <$> (zip u <$> 
                  (sublistOf ((concat . replicate (length u) . powerset) d) 
                    >>= shuffle ))
      br <- replicate 1 <$> (zip u <$> 
                  sublistOf ((concat . replicate (length u) . powerset) 
                    [(x,y)| x<-d,y<-d]))
      tr <- replicate 1 <$> (zip u <$> 
                  sublistOf ((concat . replicate (length u) . powerset) 
                    [(x,y,z)| x<-d, y<-d, z<-d]))
      let model = Mo u d ur br tr
      form <- sized (randomForm model) 
      return (MWF (model, form)) where 
        randomForm :: Model -> Int -> Gen Form
        randomForm m 0 = UnR <$> elements (unRel m) 
                       <*> elements (map Indv (dom m))
        randomForm m n = oneof 
            [ UnR   <$> elements (unRel m) <*> elements (map Indv (dom m))
            , BinR  <$> elements (biRel m) 
                    <*> elements (map Indv (dom m)) 
                    <*> elements (map Indv (dom m))
            , TertR <$> elements (tertRel m) 
                    <*> elements (map Indv (dom m)) 
                    <*> elements (map Indv (dom m))
                    <*> elements (map Indv (dom m))
            , Neg   <$> randomForm m (n `div` 4)
            , Con   <$> randomForm m (n `div` 4) <*> randomForm m (n `div` 4)
            , Dis   <$> randomForm m (n `div` 4) <*> randomForm m (n `div` 4)
            , Impl  <$> randomForm m (n `div` 4) <*> randomForm m (n `div` 4)
            ]
      
\end{code}