-- Term Rewriting System experiments

import Data.String.Utils
import Data.String
import Data.List.Utils

type Term = String
data Rule = Rule Term Term deriving (Show, Eq, Read)
type Program = [Rule]
--type Input = Term
--type Output = Term

eval :: Program -> Term -> Term
eval prog input = 
    let
        next = evalStep prog input
        stopped = (next == input)
    in
        if stopped
        then next
        else eval prog next

evalStep [] input = input
evalStep ((Rule lhs rhs):rs) input = evalStep rs $ replace lhs rhs input

parse :: String -> Program
parse s = 
    let
        ls = lines s
        lineToRule l = listToRule $ split " " l
        listToRule [lhs,rhs] = Rule lhs rhs
    in
        map lineToRule ls

sample1 = [
    Rule "1" "0",
    Rule "0" "1"
    ]
--sample1 = parse "1 0"++
--    "0 1"

sample2 = reverse sample1

flipBits = [
    Rule "_0" "1_",
    Rule "_1" "0_"
    ]


metaSample2_alt = "24_061_84_160_8A"++"_0101010101"
metaSample2 =  metaSample2_alt

--metaGoldStandard :: String -> Term
metaGoldStandard s =
    let
        s' = tail $ filter (/='2') $ filter (/='8') s
        [programString, input] = split "A" s'
        ruleStrings =  split "4" programString
        rule_lists = [  split "6" r | r <- ruleStrings ]
        rules = [Rule lhs rhs | [lhs,rhs] <- rule_lists]
    in
        eval rules input

meta = [

       ]

