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

dobra :: [Int]
dobra = [x|x <- [0,3..15]]

multiplos :: [Int]
multiplos = [x| x <- [0..20],x `mod` 2 == 0 && x `mod` 3 == 0]

printa_separados :: [[Int]]
printa_separados = [[x]| x <- [1..5]]

lista_de_um :: [[Int]]
lista_de_um = [| x <- [1,11,111,1111,11111]]

tuplas_dois :: [(Int,Int)]
tuplas_dois = [(x,y)| x <- [1,2,3],y <- [3,2,1]]

