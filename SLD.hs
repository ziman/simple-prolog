module SLD (sld) where

import Data.List
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

import Term
import Unify
import Parse
import Text.Parsec

testProg =
    [ Clause (Fun "conn" [Fun "berlin" [],Fun "moscow" []]) []
    , Clause (Fun "conn" [Fun "berlin" [],Fun "bonn" []]) []
    , Clause (Fun "conn" [Fun "moscow" [],Fun "paris" []]) []
    , Clause (Fun "path" [Var "X",Var "X"]) []
    , Clause (Fun "path" [Var "X",Var "Y"]) [Fun "conn" [Var "X",Var "Z"],Fun "path" [Var "Z",Var "Y"]]
    ]

sld :: Program -> Query -> [Subst]
sld cs qs = prove cs epsilon freeVars qs
  where
    freeVars = map (\n -> '_' : show n) [1..]

prove :: Program -> Subst -> [Name] -> Query -> [Subst]
prove cs theta ns []     = return theta
prove cs theta ns (q:qs) = do
    (gamma, ns', ts) <- usable q (map (uniqueVariant ns) cs)
    prove cs (theta >=> gamma) ns' (map (>>= gamma) (ts ++ qs))

uniqueVariant :: [Name] -> Clause -> ([Name], Clause)
uniqueVariant ns c@(Clause h bs) = (ns', Clause (h >>= theta) (map (>>= theta) bs))
  where
    usedVars    = S.toList $ vars c
    (trans,ns') = splitAt (length usedVars) ns
    theta           = foldl' (>=>) epsilon $ zipWith elemSubst usedVars (map Var ns) 

usable :: LTerm -> [([Name], Clause)] -> [(Subst, [Name], Query)]
usable t = foldr f []
  where
    f (ns, c@(Clause h bs)) xs = ((\gamma -> (gamma,ns,bs)) <$> mgu t h) ++ xs
