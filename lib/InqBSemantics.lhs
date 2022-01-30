
\subsection{Semantics}\label{sec:semantics}
In this subsection we discuss the implementation of the semantics in Haskell.

\begin{code}

module InqBSemantics where
      
import Data.List
import InqBModels
import InqBSyntax
import HelperFunctions


absPseudComp :: Model -> Prop -> Prop
absPseudComp m p = powerset $ universe m \\ (nub . concat) p

relPseudComp :: Model -> Prop -> Prop -> Prop
relPseudComp m p q = filter (all (\t -> t `notElem` p || t `elem` q) . powerset )
                                  $ powerset $ universe m

substitute :: Individual -> Var -> Form -> Form
substitute d x (UnR r i)          
                      | Var x == i  = UnR r (Indv d)
                      | otherwise   = UnR r i
substitute d x (BinR r i1 i2)       = BinR r (head terms) (terms !! 1)
                      where terms = map (\i -> if Var x == i then Indv d else i) [i1, i2]
substitute d x (TertR r i1 i2 i3)   = TertR r (head terms) (terms !! 1) (terms !! 2)
                      where terms = map (\i -> if Var x == i then Indv d else i) [i1, i2, i3]
substitute d x (Neg f)              = Neg $ substitute d x f
substitute d x (Con f1 f2)          = Con (substitute d x f1) (substitute d x f2)  
substitute d x (Dis f1 f2)          = Dis (substitute d x f1) (substitute d x f2) 
substitute d x (Impl f1 f2)         = Impl (substitute d x f1) (substitute d x f2) 
substitute d x (Forall y f)         
                    | x == y        = Forall y f
                    | otherwise     = Forall y $ substitute d x f 
substitute d x (Exists y f) 
                    | x == y        = Exists y f
                    | otherwise     = Exists y $ substitute d x f 

getString :: Term -> String 
getString (Indv i) = i
getString (Var v)  = v

toProp :: Model -> Form -> Prop
toProp _ (UnR r i )         = closeDownward [[x |(x, y) <- r, getString i `elem` y]]
toProp _ (BinR r i1 i2)     = closeDownward [[x |(x, y) <- r, (getString i1, getString i2) `elem` y]]
toProp _ (TertR r i1 i2 i3) = closeDownward [[x |(x, y) <- r, (getString i1, getString i2, getString i3) `elem` y]]
toProp m (Neg f)            = absPseudComp m (toProp m f)
toProp m (Con f1 f2)        = toProp m f1 `intersect` toProp m f2
toProp m (Dis f1 f2)        = toProp m f1 `union` toProp m f2
toProp m (Impl f1 f2)       = relPseudComp m (toProp m f1) (toProp m f2)
toProp m (Forall x f)       = foldl1 intersect [ p | d <- dom m, let p = toProp m $ substitute d x f ]
toProp m (Exists x f)       = (nub . concat) [ p | d <- dom m, let p = toProp m $ substitute d x f ]

alt :: Model -> Form -> [InfState]
alt m f = sort [x | x <- p, not (any (strictSubset x) p)]
      where p = toProp m f

info :: Model -> Form -> InfState
info m f = sort . nub . concat $ toProp m f

\end{code}