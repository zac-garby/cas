module Main where
    
import Cas.Core

main = putStrLn $ showTerm $ expand trigExpansionRules $ Call "tan" [Variable "X" @+ Variable "Y"]
