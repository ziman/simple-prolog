module Unify (mgu) where

import Term
import Data.Monoid
import Control.Monad
import Control.Applicative

mgu :: MonadPlus m => LTerm -> LTerm -> m Subst
mgu s t = unify epsilon [(s,t)]

unify :: MonadPlus m => Subst -> [(LTerm, LTerm)] -> m Subst
unify theta [] = return theta
unify theta ((Fun ln lts,Fun rn rts):xs) = guard (ln == rn) >> unify theta (zip lts rts ++ xs)
unify theta ((x@(Var xn),t):xs)
    | t == x          = unify theta xs
    | x `occursIn` t = mzero
    | otherwise      = unify (theta >=> gamma) (map (both (>>= gamma)) xs)
  where
    gamma = elemSubst xn t
unify theta ((t,Var x):xs) = unify theta ((Var x,t):xs) -- t is not a variable: guaranteed by the previous equation

occursIn :: (Ord v) => Term v -> Term v -> Bool
t `occursIn` ft@(Fun f subterm) = (t == ft) || ((t `occursIn`) `any` subterm)
t `occursIn` s = t == s

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)
