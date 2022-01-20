
\section{Derde sectie}
\label{sec:Derde}

We will now use the library form Section \ref{sec:Tweede} in a program.

\begin{code}
module Main where

import Basics

main :: IO ()
main = do
  putStrLn "Hello, world"
  print somenumbers
  print (map funnyfunction somenumbers)
  myrandomnumbers <- randomnumbers
  print myrandomnumbers
  print (map funnyfunction myrandomnumbers)
  putStrLn "GoodBye"
\end{code}

The output of the program is something like this:

\begin{showCode}
Hello!
[1,2,3,4,5,6,7,8,9,10]
[100,100,300,300,500,500,700,700,900,900]
[1,3,0,1,1,2,8,0,6,4]
[100,300,42,100,100,100,700,42,500,300]
GoodBye
\end{showCode}

