{-
Nome: Vinícius Freitas Venunes de Souza
Matrícula: 12111BSI210
-}
{-UFU/FACOM/BCC
Disciplina: Programação Funcional
Ref: Lista de Exercícios
-}
{-1. Assistir as aulas de 9 a 14 disponíveis em https://www.youtube.com/playlist?
list=PLYlU1pEvulHqVQ5ocB_SptHB4yIJ0WyfZ
-}
{-2. Implemente a função “menor” que obtém o menor elemento de uma lista de inteiros. Mostre a
execução passo a passo dessa função para o exemplo dado. \Na sua definição faça uso de outra
função “menor_de_2” que obtém o menor de dois números inteiros.
Exemplo de uso:
Main> menor [2, 3,-1,4]
-1
-}
menor_de_2 :: Int -> Int -> Int
menor_de_2 nro1 nro2
    |nro1 <= nro2 = nro1
    |otherwise = nro2

menor :: [Int] -> Int
menor [] = error"Lista sem elementos suficientes"
menor [x] = error"Lista sem elementos suficientes"
menor (x:y)
    |tamanho (x:y) == 2 = (menor_de_2 x (head y))
    |x < (menor y) = x
    |otherwise = (menor y)

--menor [2,3,-1,4]
{-
menor [2,3,-1,4]
menor [3,-1,4]/entrou no otherwise
menor [-1,4]/entrou no otherwise
(menor_de_2 -1 (4))/entrou na primeira condição de tamanho == 2
-1/entrou na primeira condição do menor_de_2
-}

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:y)
    |otherwise = (tamanho y) + 1
{-3. Implemente uma função que tem como entrada um número inteiro e que retorna uma lista com
cada dígito do numero separadamente.
Exemplo:
> digitos 1234
[1, 2, 3, 4]
-}
digitos :: Int -> [Int]
digitos 0 = [0]
digitos x
    |otherwise = digitos' x
--Filtra se o usuário digitou "digitos 0", para que não ocorra erro com o caso base da digitos'.
digitos' :: Int -> [Int]
digitos' x
    |x == 0 = []
    |otherwise = digitos' (x `div` 10) ++ [x `mod` 10]
{-4. A representação binária de um número consiste em realizar sucessivas divisões deste número por
2 e imprimir do último para o primeiro, todos os restos das divisões.
Por exemplo:
6 / 2 = 3 (resto 0) ! 3 / 2 = 1 (resto 1) ! 1 / 2 = 0 (resto 1)
6 em binário = 110
Implemente a função converte :: Int -> [Int], ex:
> converte 23
[1 ,0 ,1 ,1 ,1]
-}
converte :: Int -> [Int]
converte x
    |x  == 0 = []
    |otherwise = converte (x`div`2) ++ [(x `mod` 2)]
