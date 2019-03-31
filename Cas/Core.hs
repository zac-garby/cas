module Cas.Core where

import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Term
    = Hole String
    | Number Double
    | Variable String
    | Op String Term Term
    | Prefix String Term
    | Call String [Term]
    deriving (Show, Read, Eq)

type Pattern = Term
type Rule = (Term, Term)

data Match
    = Match { getMatch :: Map String Term }
    | NoMatch
    deriving (Show, Eq)

instance Semigroup Match where
    -- It can be assumed, when joining two matches, that each individual
    -- map is already consistent with itself. This should be the case when
    -- a match is generated from the `match` function.
    Match a <> Match b
        | consistent = Match (a <> b)
        | otherwise = NoMatch
        where consistent = all checkConsistency (M.toList b)
              checkConsistency (k, v) = case M.lookup k a of
                  Just prev -> v == prev
                  Nothing   -> True

    _ <> _ = NoMatch

-- Shorthand for some common operators
(@+) = Op "+"
(@-) = Op "-"
(@*) = Op "*"
(@/) = Op "/"

trigExpansionRules :: [Rule]
trigExpansionRules =
    [ ((Call "sin" [Hole "a" @+ Hole "b"]), ((Call "sin" [Hole "a"]) @* (Call "cos" [Hole "b"])) @+ ((Call "cos" [Hole "a"]) @* (Call "sin" [Hole "b"])))
    , ((Call "cos" [Hole "a" @+ Hole "b"]), ((Call "cos" [Hole "a"]) @* (Call "cos" [Hole "b"])) @- ((Call "sin" [Hole "a"]) @* (Call "sin" [Hole "b"])))
    , ((Call "tan" [Hole "a"]), (Call "sin" [Hole "a"] @/ Call "cos" [Hole "a"])) ]

isInteger :: Double -> Bool
isInteger n = n == (fromIntegral $ round n)

showTerm :: Term -> String
showTerm (Hole x) = "?" ++ x
showTerm (Number x) = if isInteger x then show $ round x else show x
showTerm (Variable x) = x
showTerm (Op op l r) = "(" ++ showTerm l ++ " " ++ op ++ " " ++ showTerm r ++ ")"
showTerm (Prefix op x) = op ++ showTerm x
showTerm (Call fn args) = fn ++ "(" ++ intercalate ", " (map showTerm args) ++ ")"

match :: Pattern -> Term -> Match
match (Hole x) term = Match $ M.fromList [(x, term)]
match (Op patternOp patternL patternR) (Op termOp termL termR)
    | patternOp /= termOp = NoMatch
    | otherwise = (<>) (match patternL termL) (match patternR termR)
match (Prefix patternOp patternX) (Prefix termOp termX)
    | patternOp /= termOp = NoMatch
    | otherwise = match patternX termX
match (Call patternFn patternArgs) (Call termFn termArgs)
    | patternFn /= termFn = NoMatch
    | length patternArgs /= length termArgs = NoMatch
    | otherwise = foldl (<>) (Match M.empty) (zipWith match patternArgs termArgs)
match a b = if a == b then Match M.empty else NoMatch

substitute :: Map String Term -> Term -> Term
substitute vars term@(Hole x) = fromMaybe term (M.lookup x vars)
substitute vars term = applyInside (substitute vars) term

applyRule :: Rule -> Term -> Term
applyRule rule@(pattern, replacement) term = case match pattern term of
    NoMatch -> applyInside (applyRule rule) term
    Match vars -> substitute vars replacement

applyRules :: [Rule] -> Term -> [Term]
applyRules rules term = nub $ map (\r -> applyRule r term) rules

applyInside :: (Term -> Term) -> Term -> Term
applyInside f (Op op left right) = Op op (f left) (f right)
applyInside f (Prefix op x) = Prefix op (f x)
applyInside f (Call fn args) = Call fn $ map f args
applyInside _ term = term

expansionIterations :: [Rule] -> Term -> [[Term]]
expansionIterations rules term = map nub $ iterate (>>= applyRules rules) [term]

firstIdempotentIteration :: (Eq a) => [a] -> a
firstIdempotentIteration (x:y:ys)
    | x == y = x
    | otherwise = firstIdempotentIteration (y:ys)

expansions :: [Rule] -> Term -> [Term]
expansions rules = firstIdempotentIteration . expansionIterations rules

expand :: [Rule] -> Term -> Term
expand rules = last . expansions rules