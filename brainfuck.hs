-- Brainfuck interpreter in Haskell

import Data.Char (ord, chr)
import System.IO (hSetEcho, stdin)

-- A brainfuck program would be represented as an Expr (usually a Sequence),
-- and a Loop is allowed to contain a sequence too
data Expr = Sequence [Expr] | Plus | Minus | Back | Forward | Loop Expr | Input | Output deriving (Eq, Show)

type Cell = Int
type Memory = Zipper Cell

data Zipper a = Zipper [a] [a] deriving (Show)

fromList :: [a] -> Zipper a
fromList xs = Zipper [] xs

current :: Zipper a -> a
current (Zipper before (x:after)) = x

forward :: Zipper a -> Zipper a
forward (Zipper before (x:after)) = Zipper (x:before) after

back :: Zipper a -> Zipper a
back (Zipper (x:before) (after)) = Zipper before (x:after)

update :: (a->a) -> Zipper a -> Zipper a
update f (Zipper before (x:after)) = Zipper before (new:after)
    where new = f x

initialMemory = fromList $ repeat 0

eval :: Expr -> Memory -> Memory

eval Plus m = update (\x -> x+1) m
eval Minus m = update (\x -> x-1) m
eval Back m = back m
eval Forward m = forward m

-- helper function for evaluating sequences
evalSeq :: [Expr] -> Memory -> IO Memory
evalSeq (i:is) m =
    do 
       m' <- evalIO i m
       evalSeq is m'
evalSeq [] m = do
    return m

-- main eval function which can handle any instruction and delegates some to evalSe and eval
evalIO :: Expr -> Memory -> IO Memory
evalIO (Input) m =
    do
        char <- getChar
        let value = ord char
        let m' = update (\x -> value) m
        return m'
evalIO (Output) m =
    do
        let value = current m
        let char = chr value
        putChar (if ord char /= 0 then char else '0')
        return m
evalIO (Sequence is) m = evalSeq is m
evalIO l@(Loop s) m = evalLoop s m
evalIO i m = do
    let m' = eval i m
    return m'

evalLoop :: Expr -> Memory -> IO Memory
evalLoop s m =
    if current m == 0 then
        return m
    else
        do
            m2 <- evalIO s m
            evalLoop s m2

-- Convert Brainfuck programs from text to abstract syntax tree and back again
-- Assume that every loop contains a Sequence even if it's one instruction. And the
-- root node of the tree is always Sequence.
-- Sequence [Expr] | Plus | Minus | Back | Forward | Loop Sequence | Input | Output
brainfuckCharacters = "+-<>,.[]"

parse :: String -> Expr
parse s = parseSequence $ filter (\c -> elem c brainfuckCharacters) s

parseSequence s = Sequence $ parse' s where
    parse' "" = []
    parse' s = 
        let (e, rest) = parseExpr s in
            (e : parse' rest)

parseExpr :: String -> (Expr, String)
parseExpr ('[':cs) = (Loop $ parseSequence contents, rest)  where
    -- split the contents of the loop from the rest of the program (excluding the closing bracket)
    contents = takeWhile (/=']') cs
    rest = tail $ dropWhile (/=']') cs
parseExpr (c:cs) = (parseChar c, cs)

parseChar c =
   case c of
    '+' -> Plus
    '-' -> Minus
    '<' -> Back
    '>' -> Forward
    ',' -> Input
    '.' -> Output


-- +[.-]
sampleProgram :: String
sampleProgram = "+[.-]"

sampleProgramAST :: Expr
sampleProgramAST = Sequence [
    Plus,
    Loop (Sequence [
        Output,
        Minus
        ])
    ]

-- input/echo a specific number of characters
-- ++++++++++++++++++++++++[>,.<-]

evalString :: String -> IO ()
evalString s = do
    finalMemory <- evalIO (parse s) initialMemory
    return ()

main :: IO ()
main = do
    hSetEcho stdin False
    finalMemory <- evalIO sampleProgramAST initialMemory
    return ()

