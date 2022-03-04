fatorial :: Int -> Int
fatorial 1 = 1
fatorial x
    |otherwise = (fatorial (x-1)) * x

reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:y)
    |otherwise = (reverso y) ++ [x]

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:y)
    |otherwise = (tamanho y) + 1