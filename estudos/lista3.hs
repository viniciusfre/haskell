--1 - Especifique uma função que some os números pares entre zero e 1000.
soma_pares :: Int -> Int
soma_pares x
    |x < 0 = 1002
    |otherwise = soma_pares (x-2) + x-2
--Chamar a função com x = 1000
--2 - Especifique uma função que obtenha a soma dos n primeiros termos da série:
soma_n :: Float -> Float
soma_n x
    |x == 0 = 0
    |otherwise = soma_n (x-1) + 1/(x*2)
--3 - Seja a função definida pela seguinte serie de Taylor:
--Implemente o calculo recursivo da soma da série para n = 100 termos e teste para vários
--valores de x. Compare os resultados obtidos com o valor dado pela função exp x do preludiopadrão.
fat :: Int -> Int
fat n
    |n == 0 = 1
    |otherwise = fat (n-1) * n

taylor :: Float -> Int -> Float
taylor x n
    |n == 0 = 1
    |otherwise = ((x ^ n) / fromIntegral (fat n)) + taylor x (n-1)
--4 - Escreva uma função recursiva soma_digitos que recebe um número inteiro n e retorna a
--soma de seus dígitos. Exemplo: se n = 132, soma_digitos n retorna 6.
soma_digitos :: Int -> Int
soma_digitos n
    |n == 0 = 0
    |otherwise = soma_digitos(n `div` 10) + n `mod` 10

--soma_digitos 253
--soma_digitos (253 div 10) + 253 mod 10
--soma_digitos (253 div 10) + 3
--soma_digitos (253 div 10) + 3
--soma_digitos (25) + 3
--soma_digitos (25 div 10) + 25 mod 10 + 3
--soma_digitos (2) + 5 + 3
--soma_digitos (0) + 5 + 3 + 2 
--10
