collatz :: (Integral a) => a -> a
collatz n
    | (even n)     =  n `quot` 2
    | otherwise    = n*3+1

collatz_sequence 1 = [1]
collatz_sequence n = n : (collatz_sequence $ collatz n)

collatz_sequences = map collatz_sequence [1..]

enumerate xs = zip [1..] xs

pattern = [length xs `quot` i | (i,xs) <- enumerate collatz_sequences]
