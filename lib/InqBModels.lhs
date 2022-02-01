
\subsection{Models}\label{sec:Models}
We now discuss the implementation of the \textsf{InqB} models 
as defined in Definition \ref{def: InqBModel}.
We make possible worlds of the type \verb|Int| and individuals of the type \verb|String|.

\begin{code}
module InqBModels where

type World        = Int
type Universe     = [World]
type Individual   = String
type Domain       = [Individual]
\end{code}

Inquisitive semantics is designed so that relations can be $n$-ary for any $n \in \mathbb{N}$.
However, in natural language we rarely encountered relations of an arity higher than three.
We have therefore chosen to only implement unary, binary and tertiary relations. For example,
the unary relation is represented as the characteristic set of a function from worlds to sets of individuals.
\footnote{Note that we represent sets as lists in Haskell.}

\begin{code}
type UnRelation   = [(World, [Individual])]
type BiRelation   = [(World, [(Individual, Individual)])]
type TertRelation = [(World, [(Individual, Individual, Individual)])]
\end{code}

Our models then consists of a universe, a domain, and lists of unary, binary and 
tertiary relations. Note that we diverge from Definition \ref{def: InqBModel} in this respect.
We omit the interpretation function $I$ and replace this in two ways. 

First, as the domain should be constant in all worlds, we work with the domain 
of the model rather than with a domain relative to a world.

Second, we do not work with relation symbols that are interpreted in a model. Instead we
add the relations directly to the model. As we shall see shortly, this allows for 
a very straightforward way of defining models. The downside is that we do not have a fixed
language with relation symbols that are interpreted differently in different models. This
means that a formula is always defined relative to a model, as we will see in 
Section \ref{sec:InqBSyntax}. We have chosen to put this restriction on our models so
that the implementation of arbitrary models can be simpler. And although this might be 
mathematically less complete, it allows for an intuitive way of defining one's one models.

\begin{code}
data Model = Mo { universe :: Universe
                , dom :: Domain
                , unRel :: [UnRelation]
                , biRel :: [BiRelation]
                , tertRel :: [TertRelation] }
        deriving (Eq, Ord, Show)
\end{code}

\noindent An example of an \textsf{InqB} model in this framework would then be as follows.
\begin{code}
myUnR :: UnRelation
myUnR = [(1,["a","b"]), (2,["a"]), (3,["b"]), (4,[])]

myUnR2 :: UnRelation
myUnR2 = [(1,["a","b"]), (2,["a,b"]), (3,[]), (4,[])]

myBiR :: BiRelation 
myBiR = [(1,[("a","a"),("b","b")]), (2,[("a","a")]), 
        (3,[("c","c"),("b","b")]),(4,[])]

myTertR :: TertRelation  
myTertR = [(1,[("a","a","b")]), (2,[("a","a","d"),("b","b","c")]), 
        (3,[]), (4,[("b","a","a"),("a","d","d")])]

myModel :: Model
myModel = Mo [1, 2, 3, 4] ["a", "b"] [myUnR, myUnR2] [myBiR] [myTertR]
\end{code}

\noindent Lastly, we define information states and propositions as sets of worlds
and sets of sets of worlds respectively.
\begin{code}
type Prop     = [[World]]
type InfState = [World]
\end{code}

\noindent Given these implementations of an \textsf{InqB} model we can now implement
the syntax of inquisitive semantics.