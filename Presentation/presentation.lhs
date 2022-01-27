% filename: presentation.lhs
\documentclass{beamer}
\mode<presentation>
{
  \usetheme{Madrid}     
  \setbeamertemplate{navigation symbols}{}
  \setbeamertemplate{caption}[numbered]
} 

\usepackage{minted}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

\newenvironment{slide}[1]
  {\begin{frame}[fragile,environment=slide]{#1}}
  {\end{frame}}
\long\def\ignore#1{}

\title{Is the Curry boiling?}
\subtitle{A model checker for Inquisitive Semantics}
\author{Bo Flachs \& Wessel Kroon}
\date{\today}

\begin{document}

\begin{slide}
  \titlepage
\end{slide}

\begin{slide}{Introduction}
    \begin{itemize}
        \item Inquisitive semantics is a framework for analysing information exchange.
        \item We will make a model checker for the most basic version of this framework (InqB).
    \end{itemize}
\end{slide}

\begin{slide}{Inquisitive Semantics}
    \begin{itemize}
        \item Classical formal semantics are quite suitable for analysing declarative sentences.
        \item However, they are not equipped to analyse questions such as:
        \begin{itemize}
          \item \textit{``Is the curry boiling?''}
        \end{itemize}
        \item Inquisitive semantics was developed at the ILLC to overcome these issues.
    \end{itemize}
\end{slide}

\begin{slide}{Models}
    \begin{itemize}
      \item A first order inquisitive model $M = \langle W, D, I\rangle$ where
        \begin{itemize}
          \item $W$ is a set of possible worlds;
          \item $D$ is a non-empty set of individuals;
          \item $I$ is an interpretation function.
        \end{itemize}
    \end{itemize}

    \begin{code}
      type World      = Int
      type Universe   = [World]
      type Individual = String

      type Domain     = [Individual]
      type UnRelation = [(World, [Individual])]
      type BiRelation = [(World, [(Individual, Individual)])]

    \end{code}
\end{slide}

\begin{slide}{Models in Haskell}
    \begin{code}
      data Model = Mo { universe :: Universe
                      , dom :: Domain
                      , unRel :: [UnRelation]
                      , biRel :: [BiRelation]
                      , tertRel :: [TertRelation] }
              deriving (Eq, Ord, Show)
    \end{code}
\end{slide}

\begin{slide}{Information States}
  \begin{itemize}
    \item Sets of worlds are taken as primitive rather than worlds.
    \item We call sets of worlds information states.
  \end{itemize}
  \begin{code}
    type InfState = [World]
  \end{code}
\end{slide}

\begin{slide}{Propositions}
  \begin{itemize}
    \item Propositions are non-empty, downward-closed sets of sets of worlds
    \item Intuition: A proposition consists of the information states that resolve the question.
    \item We represent a propositions by its maximal elements, called alternatives.
    \item A proposition has informative content and inquisitive content
  \end{itemize}
\end{slide}

\begin{slide}{Propositions in Haskell}
  \begin{code}
    type Prop     = [[World]]

    alt :: Model -> Form -> [InfState]
    alt m f = sort [x | x <- p, not (any (strictSubset x) p)]
          where p = toProp m f

    info :: Model -> Form -> InfState
    info m f = sort . nub . concat $ toProp m f
  \end{code}
\end{slide}

\begin{slide}{Support of Propositions}
  \begin{itemize}
    \item We say that an information state supports a propositions if its an element of it.
    \item In other words, $s$ supports $P$ iff $s \in P$.
  \end{itemize}
  \begin{code}
    supportsProp :: InfState -> Prop -> Bool
    supportsProp s p = s `elem` p 
  \end{code}
\end{slide}

\begin{slide}{Formulas}
  \begin{itemize}
    \item The language of InqB is that of first order logic:
  \end{itemize}
  \begin{code}
    type Var          = String
    type Vars         = [Var]

    data Term       = Indv Individual | Var Var 
            deriving (Eq, Ord, Show)

    data Form = UnR UnRelation Term
              | BinR BiRelation Term Term
              | TertR TertRelation Term Term Term
              | Neg Form | Con Form Form | Dis Form Form
              | Impl Form Form
              | Forall Var Form | Exists Var Form
              deriving (Eq, Ord, Show)
  \end{code}
\end{slide}

\begin{slide}{Special Operators}
  \begin{itemize}
    \item Furthermore, there are two special operators: ! and ?.
    \item As they are abbreviations, we implemented them as functions:
  \end{itemize}
  \begin{code}
    nonInq :: Form -> Form
    nonInq = Neg . Neg

    nonInf :: Form -> Form
    nonInf f = Dis f $ Neg f
  \end{code}
\end{slide}

\begin{slide}{Semantics}
  \begin{itemize}
    \item Formulas can be interpreted in a model as propositions.
  \end{itemize}
  \begin{code}
    toProp :: Model -> Form -> Prop
    toProp _ (UnR r i )  = closeDownward 
            [[x |(x, y) <- r, getString i `elem` y]]
    toProp m (Con f1 f2) 
            = toProp m f1 `intersect` toProp m f2
  \end{code}
  \begin{itemize}
    \item In the other clauses of this function we use more complicated helper functions. 
          We will not go into those clauses.
  \end{itemize}
\end{slide}

\begin{slide}{Model checker}
  \begin{itemize}
    \item An information state supports a formula if it is an element of the corresponding proposition.
  \end{itemize}
  \begin{code}
    supportsForm :: Model -> InfState -> Form -> Bool
    supportsForm m s f = supportsProp s $ toProp m f
  \end{code}
  \begin{itemize}
    \item Given a model, an information state and a formula, 
          our model checker should check if that information state
          supports the formula.
  \end{itemize}
\end{slide}

\begin{slide}{Useful functions}
  \begin{code}
    isInquisitive :: Model -> Form -> Bool 
    isInquisitive m f 
          = sort (toProp m f) /= 
                (sort . powerset) (info m f)

    isInformative :: Model -> Form -> Bool 
    isInformative m f 
          = (sort . universe) m /= sort (info m f) 

    entails :: Model -> Form -> Form -> Bool
    entails m f1 f2 = all (`elem` p2) p1 where
                p1 = toProp m f1
                p2 = toProp m f2
  \end{code}
\end{slide}

\begin{slide}{To do}
  \begin{itemize}
    \item Implement QuickCheck;
    \item Check several well-known theorems of InqB using QuickCheck.
  \end{itemize}
\end{slide}
\end{document}