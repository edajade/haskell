-- Brainfuck interpreter in Haskell
-- The eval function can handle all of the instructions except IO.
-- The easiest solution would be to handle sequences inside the IO monad too
-- Also you need a simple parser

import Data.Char (ord, chr)

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
eval l@(Loop s) m = 
    if current m == 0 then
        m
    else
        let
            m2 = eval s m
            m3 = eval l m2
        in
            m3

eval (Sequence is) m = evalSeq is m
eval Plus m = update (\x -> x+1) m
eval Minus m = update (\x -> x-1) m
eval Back m = back m
eval Forward m = forward m

evalSeq :: [Expr] -> Memory -> Memory
evalSeq (i:is) m =
    let
        m' = eval i m
    in
        evalSeq is m'
evalSeq [] m = m


evalIO :: Expr -> Memory -> IO Memory
evalIO e m
    | e == Input =
        do
            char <- getChar
            let value = ord char
            let m' = update (\x -> value) m
            return m'
    | e == Output =
        do
            let value = current m
            let char = chr value
            putChar $ char
            return m
    | otherwise = undefined

-- +[-]
sampleProgram :: Expr
sampleProgram = Sequence [
    Plus,
    Loop Minus]

main :: IO ()
main = do
    let finalMemory = eval sampleProgram initialMemory
    return ()

