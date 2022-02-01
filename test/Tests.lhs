\section{QuickCheck}
\label{sec:QuickCheck}
Now that we have defined \textsf{InqB} models, formulas, propositions and 
a model checker, we can use QuickCheck to check several facts about \textsf{InqB}.\footnote{We 
say facts rather than propositions or theorems, because this 
is the terminology that is used in the original source.}
These facts are from \cite{inquisitive19}, and we use their numbering. 
We will first list these facts, after which we will discuss the QuickCheck implementation.
\begin{itemize}
    \setlength\itemsep{ 0em}
    \item \textbf{Fact 4.12}
        \begin{itemize}
        \setlength\itemsep{ 0em}
            \item $! \varphi \equiv \neg \neg \varphi$
            \item $? \varphi \equiv \varphi \lor \neg \varphi$
        \end{itemize}
    \item \textbf{Fact 4.13}: $\varphi \equiv (!\varphi \land ?\varphi)$
    \newpage
    \item \textbf{Fact 4.17}
        \begin{itemize}
        \setlength\itemsep{ 0em}
            \item \textit{2}: $\neg\varphi$ is always non-inquisitive.
            \item \textit{3}: $!\varphi$ is always non-inquisitive.
        \end{itemize}
    \item \textbf{Fact 4.18}
        \begin{itemize}
        \setlength\itemsep{ 0em}
            \item \textit{1}: $?\varphi$ is always non-informative.
        \end{itemize}
\end{itemize}

\noindent For the implementation of the tests of these facts, we import all modules that were 
discussed up until now. 
\begin{code}
module Main where

import InqBModels
import InqBSyntax
import InqBSemantics
import HelperFunctions
import Data.List
import Test.QuickCheck
import Test.Hspec
\end{code}
The \verb|main| function implements all of the 
facts listed above as properties. We use Hspec, QuickCheck and the \verb|Arbitrary| 
instance of \verb|ModelWithForm| to test these facts. 
\begin{code}
main :: IO()
main = hspec $ do
  describe "Fact 4.12" $ do
    it "!phi equiv neg neg phi" $
      property (\(MWF (m, f))-> isEquivalent m (nonInq f) (Neg (Neg f)))
    it "?phi equiv phi or (neg phi)" $
      property (\(MWF (m, f))-> isEquivalent m (nonInf f) (Dis f $ Neg f))
  describe "Fact 4.13" $ do
    it "phi equiv (!phi and ?phi)" $
      property (\(MWF (m, f))-> isEquivalent m f (Con (nonInq f) (nonInf f)))
  describe "Fact 4.17" $ do
    it "2. (neg phi) is always non-inquisitive" $
      property (\(MWF (m, f))-> (not . isInquisitive m) (Neg f))
    it "3. !phi is always non-inquisitive" $
      property (\(MWF (m, f))-> (not . isInquisitive m) (nonInq f))
  describe "Fact 4.18" $ do
    it "1. ?phi is always non-informative" $
      property (\(MWF (m, f))-> (not . isInformative m) (nonInf f))
\end{code}
For these properties we have used three functions that implement when formulas are
inquisitive, informative, and equivalent. These correspond to the following three
definitions:
\begin{itemize}
  \setlength\itemsep{ 0em}
  \item A formula is informative iff \texttt{info}$(\varphi) \neq W$;
  \item A formula is inquisitive iff \texttt{info}$(\varphi) \notin [\varphi]$;
  \item $\varphi \equiv \psi$ just in case that $[\varphi] = [\psi]$.
\end{itemize}
\begin{code}
isInformative :: Model -> Form -> Bool 
isInformative m f = (sort . universe) m /= sort (info m f) 

isInquisitive :: Model -> Form -> Bool 
isInquisitive m f = sort (toProp m f) /= (sort . powerset) (info m f)

isEquivalent :: Model -> Form -> Form -> Bool 
isEquivalent m f g = sort (toProp m f) == sort (toProp m g)
\end{code}
These tests can be run by using the command \verb|stack test|. This will give the 
following output:
\begin{showCode}
imc> test (suite: tests)
                 
Fact 4.12
  !phi equiv neg neg phi
    +++ OK, passed 100 tests.
  ?phi equiv phi or (neg phi)
    +++ OK, passed 100 tests.
Fact 4.13
  phi equiv (!phi and ?phi)
    +++ OK, passed 100 tests.
Fact 4.17
  2. (neg phi) is always non-inquisitive
    +++ OK, passed 100 tests.
  3. !phi is always non-inquisitive
    +++ OK, passed 100 tests.
Fact 4.18
  1. ?phi is always non-informative
    +++ OK, passed 100 tests.

Finished in 0.0088 seconds
6 examples, 0 failures

imc> Test suite tests passed
\end{showCode}
We can therefore conclude that our implementation of \textsf{InqB} in Haskell 
works correctly. Furthermore, we have implemented a tool that can be used to check more 
involved facts about the framework \textsf{InqB}.