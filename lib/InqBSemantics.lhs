
\subsection{Semantics}\label{sec:semantics}
We can implement the semantics of \textsf{InqB} using the models and syntax 
discussed in previous sections. Firstly, we need to implement the algebraic 
operators that we do not yet have in Haskell: the relative and absolute 
pseudo-complement. We follow Definition \ref{defpseudo}.

\begin{code}
module InqBSemantics where
      
import Data.List
import InqBModels
import InqBSyntax
import HelperFunctions

absPseudComp :: Model -> Prop -> Prop
absPseudComp m p = powerset $ universe m \\ (nub . concat) p

relPseudComp :: Model -> Prop -> Prop -> Prop
relPseudComp m p q = filter 
                      (all (\t -> t `notElem` p || t `elem` q) . powerset )
                         $ powerset $ universe m
\end{code}

In order to work with quantifiers and variables, we need to be able to 
substitute elements from our domain. The following function, \verb|substitute|, 
substitutes an individual in place of a variable in a formula, yielding a new formula.

\begin{code}
substitute :: Individual -> Var -> Form -> Form
substitute d x (UnR r i)          
                    | Var x == i  = UnR r (Indv d)
                    | otherwise   = UnR r i
substitute d x (BinR r i1 i2)     = BinR r (head terms) (terms !! 1)
                      where terms = map (\i -> if Var x == i 
                                      then Indv d else i) [i1, i2]
substitute d x (TertR r i1 i2 i3) = TertR r 
                                      (head terms) 
                                        (terms !! 1) 
                                          (terms !! 2)
                      where terms = map (\i -> if Var x == i 
                                      then Indv d else i) [i1, i2, i3]
substitute d x (Neg f)            = Neg $ substitute d x f
substitute d x (Con f1 f2)        = Con 
                                      (substitute d x f1) 
                                        (substitute d x f2)  
substitute d x (Dis f1 f2)        = Dis 
                                      (substitute d x f1) 
                                        (substitute d x f2) 
substitute d x (Impl f1 f2)       = Impl 
                                      (substitute d x f1) 
                                        (substitute d x f2) 
substitute d x (Forall y f)         
                    | x == y      = Forall y f
                    | otherwise   = Forall y $ substitute d x f 
substitute d x (Exists y f) 
                    | x == y      = Exists y f
                    | otherwise   = Exists y $ substitute d x f 
\end{code}

The next step consists of writing a function that enables us to turn a 
formula into a proposition relative to some model. We will see that in 
order to do this we need to be able to convert objects of type \verb|Term| 
into objects of type \verb|String|. The function \verb|getString| does this. 
We have chosen to not put this function in the module with helper functions, 
to avoid cyclic importations. 

\begin{code}
getString :: Term -> String 
getString (Indv i) = i
getString (Var v)  = v
\end{code}

Now, the function \verb|toProp| turns formulas into propositions relative 
to a model. The function is defined recursively and mirrors Definition 
\ref{defsemantics}. Observe that the cases for atomic formulas do not use 
the model. Instead, the relations are used in these clauses, because the 
required information for constructing a proposition is already present in 
the relation \verb|r|, which is part of a model. This is a consequence of 
not using relation symbols in formulas but the relations themselves. This 
is a shortcoming that we already discussed in Section \ref{sec:Models}. The 
clauses for non-atomic formulas are straightforward implementations of 
Definition \ref{defsemantics}.

\begin{code}
toProp :: Model -> Form -> Prop
toProp _ (UnR r i )         = closeDownward 
                                [[x |(x, y) <- r, getString i `elem` y]]
toProp _ (BinR r i1 i2)     = closeDownward 
                                [[x |(x, y) <- r, 
                                    (getString i1, getString i2) 
                                        `elem` y]]
toProp _ (TertR r i1 i2 i3) = closeDownward 
                                [[x |(x, y) <- r, 
                                    (getString i1, getString i2, getString i3) 
                                        `elem` y]]
toProp m (Neg f)            = absPseudComp m (toProp m f)
toProp m (Con f1 f2)        = toProp m f1 `intersect` toProp m f2
toProp m (Dis f1 f2)        = toProp m f1 `union` toProp m f2
toProp m (Impl f1 f2)       = relPseudComp m (toProp m f1) (toProp m f2)
toProp m (Forall x f)       = foldl1 intersect 
                                  [ p | d <- dom m, 
                                        let p = toProp m $ substitute d x f ]
toProp m (Exists x f)       = (nub . concat) 
                                  [ p | d <- dom m, 
                                        let p = toProp m $ substitute d x f ]
\end{code}

There are two additional functions part of our semantics. Firstly, we have 
the function \verb|alt|, turning a formula into the set of alternatives of its 
corresponding formula, relative to a model. This is done using \verb|toProp| 
and then taking the maximal elements of the resulting set of information states, 
in accordance with Definition \ref{defalt}. We sort the resulting set of information 
states to avoid having equivalent propositions that are not recognized as such by Haskell.

\begin{code}
alt :: Model -> Form -> [InfState]
alt m f = sort [x | x <- p, not (any (strictSubset x) p)]
          where p = toProp m f
\end{code}

Secondly, we have the function \verb|info|, giving the informative content 
of a formula relative to a model. This function takes the union of all information 
states in a proposition, in accordance with Definition \ref{definfo}. The resulting 
information state is sorted for the same reason as in the function above.

\begin{code}
info :: Model -> Form -> InfState
info m f = sort . nub . concat $ toProp m f
\end{code}