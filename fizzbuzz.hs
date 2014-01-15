main = do
    fizzbuzz [1..100]

fizzbuzz [] = return ()
fizzbuzz (x:xs) = do
    putStrLn message
    fizzbuzz xs
    where
        mult3 = x `mod` 3 == 0
        mult5 = x `mod` 5 == 0
        message | mult3 && mult5 = "FizzBuzz"
                | mult3 = "Fizz"
                | mult5 = "Buzz"
                | otherwise = show x

