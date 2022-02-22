dobrapos :: [Int] -> [Int]
dobrapos (x:y)
    |y == [] && x > 0 = [x*2] ++[]
    |y == [] = []
    |x <= 0 = (dobrapos y)
    |otherwise = [x*2] ++ (dobrapos y)

fatores :: Int -> [Int]
fatores n = [x| x <- [1..n],n `mod` x == 0]

primo :: Int -> Bool
primo n =length (fatores n) == 2