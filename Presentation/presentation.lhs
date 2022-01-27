% filename: presentation.lhs
\documentclass{beamer}

\usepackage{minted}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

\newenvironment{slide}[1]
  {\begin{frame}[fragile,environment=slide]{#1}}
  {\end{frame}}
\long\def\ignore#1{}

\title{Is the curry boiling?}
\subtitle{A model checker for Inquisitive Semantics}
\author{Bo Flachs \& Wessel Kroon}
\date{\today}

\begin{document}

\begin{frame}
  \titlepage
\end{frame}

\begin{frame}{Example}

\begin{code}
foo :: Int -> Int
foo _ = 5
\end{code}

\end{frame}

\end{document}