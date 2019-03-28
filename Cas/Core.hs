module Cas.Core where

import Data.Maybe
import Data.List

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
type Match = [(String, Term)]

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

match' :: Pattern -> Term -> Maybe Match
match' (Hole x) term = Just [(x, term)]
match' (Op patternOp patternL patternR) (Op termOp termL termR)
    | patternOp /= termOp = Nothing
    | otherwise = (++) <$> (match' patternL termL) <*> (match' patternR termR)
match' (Prefix patternOp patternX) (Prefix termOp termX)
    | patternOp /= termOp = Nothing
    | otherwise = match' patternX termX
match' (Call patternFn patternArgs) (Call termFn termArgs)
    | patternFn /= termFn = Nothing
    | length patternArgs /= length termArgs = Nothing
    | otherwise = concat <$> sequence (zipWith match' patternArgs termArgs)
match' a b = if a == b then Just [] else Nothing

match :: Pattern -> Term -> Maybe Match
match pattern term = case match' pattern term of
    Nothing -> Nothing
    Just vars -> if consistent vars then Just vars else Nothing

consistent :: Match -> Bool
consistent ms = all check ms
    where check (name, val) = (fromJust $ lookup name ms) == val

substitute :: Match -> Term -> Term
substitute vars term@(Hole x) = fromMaybe term (lookup x vars)
substitute vars term = applyInside (substitute vars) term

applyRule :: Rule -> Term -> Term
applyRule rule@(pattern, replacement) term = case match pattern term of
    Nothing -> applyInside (applyRule rule) term
    Just vars -> substitute vars replacement

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