
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

Now that we have defined what an \textit{InqB} model and an \textit{InqB}
formula looks like in Haskell, we can create arbitrary instances of them.
We do this by creating a new type that is a tuple of a model and a formula.
Thereby we can create a single instance of the class \verb|Arbitrary|, which
we can use for the checking of several \textit{InqB} facts. These checks are
implemented using QuickCheck and are discussed in Section \ref{sec:QuickCheck}.

\begin{code}
newtype ModelWithForm = MWF (Model, Form) deriving Show
\end{code}

For an arbitrary \verb|ModelWithForm| we fix a set of world and a set of individuals.
The model then will contain an arbitrary subset of these. As the \verb|Arbitrary| instance
is quite long, we will go over the code line by line. 

First we let the universe, \verb|u|, and 
the domain, \verb|d|, be arbitrary non-empty subsets of \verb|myWorlds| and 
\verb|myIndividuals|, respectively. We then take an arbitrary and shuffled list of 
lists of individuals and zip this with the universe. Replicating this gives us something of 
the type \verb|UnRelation|. We have chosen to only include one relation of each 
arity in our arbitrary models, but this could be extended to an arbitrary number.
The code for binary, \verb|ur|, and tertiary, \verb|tr|, relations is analogous.\footnote{Note that these sets are not shuffled. This is because the shuffling of 
of these larger sets made the code very slow. A possible improvement would be a 
way of getting arbitrary relations with less overhead.} Putting these all together
we have an arbitrary model.


\begin{code}
myWorlds :: [World]
myWorlds = [1..4]

myIndividuals :: [Individual]
myIndividuals = ["a","b","c","d"]

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
\end{code}
Using the function \verb|sized| we can then create formulas of arbitrary length 
using the individuals and relations that were created for this arbitrary model.
We have not implemented arbitrary formulas containing quantifiers, as this poses 
a rather difficult extra challenge. To correctly implement this one could first 
create a formula, and then substitute some of the individuals for the variable 
that will be quantified over. However, for our current purposes and time constraints 
we have chosen to work with these restrictions.  
\begin{code}
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
            , Impl  <$> randomForm m (n `div` 4) <*> randomForm m (n `div` 4)]
\end{code}
Now that we have defined what models and formulas are, we can implement the 
semantics of \textit{InqB}.