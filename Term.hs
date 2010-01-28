{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Term
    ( Name, Term (..), LTerm
    , Subst, epsilon, elemSubst
    , Clause (..), Query, Program
    , vars
    )
    where

import qualified Data.Set as S
import Data.Monoid
import Data.List
import Control.Monad
import Control.Applicative

type Name = String
type LTerm = Term Name
type Subst = Name -> LTerm
data Term v = Fun Name [Term v] | Var v deriving (Eq, Ord)
data Clause = Clause LTerm [LTerm] deriving Show
type Program = [Clause]
type Query = [LTerm]

instance Show LTerm where
    show (Var x) = x
    show (Fun n []) = n
    show (Fun n ts) = concat [n, "(", intercalate "," $ map show ts, ")"]

instance Show Subst where
    show theta = concat [ "{", intercalate ", " (map format vs) , "}" ]
      where
        format v = v ++ " = " ++ show (theta v)
        vs = filter (\x -> theta x /= Var x) (map return ['A'..'Z'])
    
instance Functor Term where
    fmap f (Fun n ts) = Fun n (map (fmap f) ts)
    fmap f (Var x)    = Var (f x)    

instance Applicative Term where
    pure  = Var
    f <*> x = do { f' <- f; x' <- x; return $ f' x' }

instance Monad Term where
    return = Var
    x >>= f = joinT (f `fmap` x)

instance Monad m => Monoid (a -> m a) where
    mempty  = return
    mappend = (>=>)

class Variables a v | a -> v where
    vars :: a -> S.Set v

instance Ord v => Variables (Term v) v where
    vars (Fun _ ts) = vars ts
    vars (Var x)    = S.singleton x

instance Variables Clause Name where
    vars (Clause c cs) = vars c `S.union` vars cs

instance (Ord b, Variables a b) => Variables [a] b where
    vars xs = S.unions $ map vars xs

joinT :: Term (Term v) -> Term v
joinT (Fun n ts) = Fun n (map join ts)
joinT (Var x)    = x

-- The empty substitution
epsilon :: Subst
epsilon = return

-- Elementary substition containing one assignment
elemSubst :: Name -> LTerm -> Subst
elemSubst n t x
    | x == n     = t
    | otherwise = return x
