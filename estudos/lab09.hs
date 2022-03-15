cab :: [a] -> a
cab (x:_) = x

par :: Integral a => a -> Bool
par x = x `mod` 2 == 0

divisao :: Fractional a => a -> a -> a
divisao x y = x / y

antes :: Ord t => (t,t) -> Bool
antes (x,y) = x < y

iguais :: Eq t => (t,t) -> Bool
iguais (x,y) = x == y

{-
UFU/FACOM/BSI
Disciplina: PF
Ref: Exercícios
Tópico: Classes e Polimorfismo
Dica: Resolva todos os exercícios sem utilizar o computador. Uma vez que os exercícios estejam
prontos, utilize o GHCi para conferir suas respostas.
1. Quais os tipos das seguintes funções? Inclua as devidas restrições de classe, se necessário.
segundo xs = head ( tail xs)
-}
segundo :: [a] -> a
segundo xs = head(tail xs)
{-
trocar (x, y) = (y, x)
-}
trocar :: (a,b) -> (b,a)
trocar (x,y) = (y,x)
{-
parear x y = (x, y)
-}
parear :: a -> b -> (a,b)
parear x y = (x,y)
{-
dobro x = x * 2
-}
dobro :: Num a => a -> a
dobro x = x*2
{-
palindromo xs = reverse xs == xs
-}
palindromo :: Eq a => [a] -> Bool
palindromo (xs) = reverse xs == xs
{-
2. Escreva uma função que forneça o terceiro elemento de uma tupla-3. Declare o tipo mais geral
para essa função.
-}
third_element :: (a,b,c) -> c
third_element (x,y,z) = z
{-
3. Fornecidos três valores, a, b e c, implemente uma função que retorne quantos desses três são
iguais. A reposta deve ser 3, se todos sao iguais; 2, se dois sao iguais e um é distinto dos demais ou
0, se todos sao distintos entre si. Declare o tipo mais geral para essa função.
-}
equals ::Eq a => a -> a -> a -> Int
equals x y z
    |x == y && y == z = 3
    |x /= y && x /= z && y /= z = 0
    |otherwise = 2
{-
4. Escreva uma função que receba uma tupla-3 e retorne uma tupla-2 com o maior e o menor
elemento dentre os três. Declare o tipo mais geral para essa função.
-}
bigtwo ::Ord a => (a,a,a) -> (a,a)
bigtwo (x,y,z)
    |x >= y && x >= z = (x,(bigtwo' (y,z)))
    |y >= z && y >= x = (y,(bigtwo' (x,z)))
    |z >= x && z >= y = (z,(bigtwo' (y,x)))

bigtwo' :: Ord a => (a,a) -> a
bigtwo' (x,y)
    |x < y = x
    |otherwise = y
{-
5. Implemente as seguintes funções recursivas em Haskell definindo o tipo mais geral de cada
função.
(a) Determinar o comprimento de uma lista. Exemplo de uso:
> comprimento [3 ,14 ,1 ,5 ,9]
5
-}
comprimento :: [a] -> Int
comprimento [] = 0
comprimento (x:y) = (comprimento y) + 1
{-
(b) Determinar o somatorio dos elementos de uma lista. Exemplo de uso:
> somatorio [3 ,14 ,1 ,5 ,9]
32
-}
somatorio :: Num a => [a] -> a
somatorio [x] = x
somatorio (x:y) = (somatorio y) + x
{-
(c) Determinar o somatorio dos elementos ímpares de uma lista. Exemplo de uso:
> somatorio_impares [3 ,14 ,1 ,5 ,9]
18
-}
somatorio_impares :: Integral a => [a] -> a
somatorio_impares (x:y)
    |y == [] && x `mod` 2 /= 0 = x
    |y == [] && x `mod` 2 == 0 = 0
    |x `mod` 2 /= 0 = x + somatorio_impares y
    |otherwise = somatorio_impares y
{-
(d) Determinar a soma dos elementos de uma lista que sao múltiplos de 3. Exemplo de uso:
> soma_mult_3 [3 ,14 ,1 ,5 ,9]
12
-}
soma_mult_3 :: Integral a => [a] -> a
soma_mult_3 (x:y)
    |y == [] && x `mod` 3 == 0 = x
    |y == [] && x `mod` 3 /= 0 = 0
    |x `mod` 3 == 0 = x + soma_mult_3 y
    |otherwise = soma_mult_3 y
{-
(e) Determinar o produtorio dos elementos de uma lista. Exemplo de uso:
> produtorio [3 ,14 ,1 ,5 ,9]
1890
-}
produtorio :: Num a => [a] -> a
produtorio [x] = x
produtorio (x:y) = produtorio y * x
{-
(f) Determinar o n-esimo elemento de uma lista. Exemplo de uso:
> n_esimo 3 [3 ,14 ,1 ,5 ,9]
5
-}
n_esimo ::Integral b =>  b -> [a] -> a
n_esimo 0 (x:y) = x
n_esimo w (x:y)
    |otherwise = (n_esimo (w-1) y)

{-
Obs.: Considere que o primeiro elemento da lista esta na posição 1.
(g) Determinar o ultimo elemento de uma lista,. Exemplo de uso:
> ultimo [3 ,14 ,1 ,5 ,9]
9
-}
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:y)
    |otherwise = ultimo  y
{-
(h) Substituir todas as ocorrencias de um elemento x em uma lista por outro elemento y. Exemplo
de uso:
> substituir_todos 1 2 [3 ,14 ,1 ,5 ,1]
[3 ,14 ,2 ,5 ,2]
-}
substituir_todos ::Eq a => a -> a -> [a] -> [a]
substituir_todos w x [] = []
substituir_todos w x (y:z)
    |w == y = x : (substituir_todos w x z)
    |otherwise = y : (substituir_todos w x z)
{-
(i) Determinar o maior elemento de uma lista. Exemplo de uso:
> maior [3 ,14 ,1 ,5 ,9]
14
-}
maior :: Ord a => [a] -> a
maior [x] = x
maior (x:y)
    |x > maior y = x
    |otherwise = (maior y)

{-
(j) Escreva uma função que retorna verdadeiro se todos os elementos de uma lista de inteiros forem
ímpares, ou falso, caso contrario. Exemplo de uso:
> impares [3 ,1 ,5 ,9]
True
-}
impares ::Integral a => [a] -> Bool
impares [] = True
impares (x:y)
    |x `mod` 2 == 0 = False
    |otherwise = (impares y)
{-
(k) Inserir um elemento ordenadamente em uma lista já ordenada. Exemplo de uso:
> insere 4 [1 ,3 ,5]
[1 ,3 ,4 ,5]
Se o elemento ja existir, pode inseri-lo depois de sua ocorrência.
-}
insere :: Ord a => a -> [a] -> [a]
insere x [] = []
insere x (y:z)
    |x <= y = [x] ++ [y] ++ z
    |otherwise = [y] ++ (insere x z)
{-
(l) Verificar se um elemente pertece a lista. Exemplo de uso:
> pertence 5 [3 ,14 ,1 ,5 ,9]
True
-}
pertence ::Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (y:z)
    |x == y = True
    |otherwise = (pertence x z)
{-
(m) Dada uma lista de tuplas-2, criar uma nova lista com apenas os primeiros elementos de cada
tupla. Exemplo de uso:
> primeiros [(3 ,14) ,(1 ,5) ,(9 ,1)]
[3 ,1 ,9]
-}
primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros (x:y)
    |otherwise = (fst x) : (primeiros y)
{-
(n) Dada uma lista de listas, concatenar todas as sub-listas em uma unica lista. Exemplo de uso:
> concatenar [[3 ,14 ,1 ,5] ,[9 ,1 ,2]]
[3 ,14 ,1 ,5 ,9 ,1 ,2]
&&&&&&&&&&&&&&&&&&&&&&&&&
-}
concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:y) = x ++ (concatenar y)
