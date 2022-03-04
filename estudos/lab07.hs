import Data.Char

cab :: [Int] -> Int
cab (x:_) = x

cau :: [Int] -> [Int]
cau (_:xs) = xs

vazia :: [Int] -> Bool
vazia [] = True
vazia (x:xs) = False

--1. Mostre os resultados das seguintes execuções, ou explique porque elas não podem ser
--executadas.
-- - primeira versão
separa :: [ Int ] -> [ Int ]
separa (p:s:r) = (s:r)
-- separa [1 ,2 ,3 ,4 ,5]
-- separa [1 ,2 ,3]
--separa [1 ,2]
--separa [1]
--A definição da função está correta porém o uso de 3 variaveis é desnecessario. O certo deveriam ser duas, cabeça e cauda,o uso do termo p é desnecessário.
--segunda versão
--separab :: [ Int ] -> [ Int ]
--separab (p:s:r) = (r:s:p)
--separab [1 ,2 ,3 ,4 ,5]
--separab [1 ,2 ,3]
--separab [1 ,2]
--separab [1]
--A definição da função está errada pois o termo à esquerda deve ser uma lista e nesse caso se trata de um termo sozinho, oq gera erro de compilação.
--terceira versão
separac :: [ Int ] -> [ Int ]
separac (p:r) = r
--separac [1 ,2 ,3 ,4 ,5]
--separac [1 ,2 ,3]
--separac [1 ,2]
--separac [1]
--separac []
--Definição correta

--2. Implemente as seguintes funções em Haskell. Mostre a execução passo a passo dessas funções
--para os exemplos fornecidos.
--(a) Determinar o comprimento de uma lista, ex:
-- comprimento [3 ,14 ,1 ,5 ,9]
--5
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (_:y) = (comprimento y) + 1
--(b) Determinar o somatorio dos elementos de uma lista, ex:
-- somatorio [3 ,14 ,1 ,5 ,9]
--32
somatorio :: [Int] -> Int
somatorio [] = error "Lista vazia"
somatorio [x] = x
somatorio (x:y) = (somatorio y) + x
--soma[1,2,3]
--soma [2,3] + 1
--soma [3] + 1 + 2
--soma [] + 1 + 2 + 3
--(c) Determinar o somatorio dos elementos ímpares de uma lista, ex:
-- somatorio_impares [3 ,14 ,1 ,5 ,9]
--18
--Obs.: Nao utilize a função odd
somatorio_impares :: [Int] -> Int
somatorio_impares [] = error"Lista vazia"
somatorio_impares [x] = x
somatorio_impares (x:y)
    |x `mod` 2 /= 0 = (somatorio_impares y) + x 
    |otherwise = somatorio_impares y
--(d) Determinar a soma dos quadrados dos elementos de uma lista, ex:
--soma_quadrados [3 ,14 ,1 ,5 ,9]
--312
soma_quadrados :: [Int] -> Int
soma_quadrados [] = error"Lista vazia"
soma_quadrados [x] = x*x
soma_quadrados (x:y) = x*x + soma_quadrados y
--(e) Determinar a soma dos elementos de uma lista que sao múltiplos de 3, ex:
-- soma_mult_3 [3 ,14 ,1 ,5 ,9]
--12
soma_mult_3 :: [Int] -> Int
soma_mult_3 [] = 0
soma_mult_3 (x:y)
    |x `mod` 3 == 0 = soma_mult_3 (y) + x
    |otherwise = soma_mult_3 y 
--soma_mult_3 [3,14,1,5,9]
--soma_mult_3 [14,1,5,9] + 3
--soma_mult_3 [1,5,9] + 3
--(f) Determinar o produtorio dos elementos de uma lista, ex: ´
--produtorio [3 ,14 ,1 ,5 ,9]
--1890
produtorio :: [Int] -> Int
produtorio [] = error "Lista vazia"
produtorio [x] = x
produtorio (x:y) = (produtorio y) * x
{-(g) Determinar o n-esimo elemento de uma lista, ex:
> n_esimo 3 [3 ,14 ,1 ,5 ,9]
5
Obs.: Considere que o primeiro elemento da lista esta na posição
-}
n_esimo :: Int -> [Int] -> Int
n_esimo x (y:z)
    |x == 0 = y
    |otherwise = (n_esimo (x-1) z)
{-(h) Determinar o ultimo elemento de uma lista, ex:
> ultimo [3 ,14 ,1 ,5 ,9]
9
Obs.: Nao utilize a função last.
-}
ultimo :: [Int] -> Int
ultimo (x:y)
    |y == [] = x
    |otherwise = (ultimo y)
{-(i) “Duplicar” os elementos de uma lista, ex:
> duplica [3 ,14 ,1 ,5 ,9]
[3 ,3 ,14 ,14 ,1 ,1 ,5 ,5 ,9 ,9]
-}
duplica :: [Int] -> [Int]
duplica [] = error"Lista vazia"
duplica [x] = [x] ++ [x]
duplica (x:y) = [x] ++ [x] ++ (duplica y)  
--(duplica [14,1,5,9]) ++ [3] ++ [3]

{-(j) Reverter uma lista, ex:
> reverso [3 ,14 ,1 ,5 ,9]
[9 ,5 ,1 ,14 ,3]
-}
reverso :: [Int] -> [Int]
reverso [] = error"Lista vazia"
reverso [x] = [x] 
reverso (x:y) =(reverso y) ++ [x] 

{-(k) Substituir todas as ocorrencias de um elemento x em uma lista por outro elemento
y, ex:
> substituir_todos 1 2 [3 ,14 ,1 ,5 ,1]
[3 ,14 ,2 ,5 ,2]
-}
substituir_todos :: Int -> Int -> [Int] -> [Int]
substituir_todos x y (w:z)
    |z == [] && w == x = [y]
    |z == [] = [w]
    |w == x = y : (substituir_todos x y (z))
    |otherwise = [w] ++ (substituir_todos x y z)
--substituir_todos 1 2 [3 ,14 ,1 ,5 ,1]
--
--substituir_todos 1 2 [1 ,5 ,1]

{-(l) Substituir a primeira ocorrencia de um elemento x em uma lista por outro elemento
y, ex:
> substituir_primeiro 1 2 [3 ,14 ,1 ,5 ,1]
[3 ,14 ,2 ,5 ,1]
-}
substituir_primeiro :: Int -> Int -> [Int] -> [Int]
substituir_primeiro x y (w:z)
   |z == [] = [w]
   |w == x = y : (substituir_primeiro x y (z))
   |otherwise = [w] ++ (substituir_primeiro x y z)

{-(m) Determinar o produto interno de dois vetores representados por listas, ex:
> produto_interno [3 ,14 ,1] [5 ,9 ,26]
167
-}
produto_interno :: [Int] -> [Int] -> Int
produto_interno (x:y) (w:z)
    |y == [] = x*w
    |otherwise = x * w + (produto_interno y z)
{-(n) Determinar o maior elemento de uma lista, ex:
> maior [3 ,14 ,1 ,5 ,9]
14
-}
maior :: [Int] -> Int
maior (x:y)
    |y == [] = x
    |x > (maior y) = x
    |otherwise = (maior y)
{-(o) “Desduplicar” os elementos de uma lista, ex:
> desduplicar [3 ,3 ,14 ,14 ,1 ,1 ,5 ,5 ,9 ,9]
[3 ,14 ,1 ,5 ,9]
-}
desduplicar :: [Int] -> [Int]
desduplicar (x:y)
    |y == [] = [x]
    |x == (head y) = (desduplicar y)
    |otherwise = x : (desduplicar y) 
{-(p) Escreva uma func¸ao que retorna verdadeiro se todos os elementos de uma lista de inteiros
forem ímpares, ou falso, caso contrario, ex:
> impares [3 ,1 ,5 ,9]
True
-}
impares :: [Int] -> Bool
impares (x:y)
    |x `mod` 2 /= 0 && y == [] = True
    |x `mod` 2 /= 0 = (impares y)
    |otherwise = False
{-(q) Inserir um elemento ordenadamente em uma lista ja ordenada, ex:
> insere 4 [1 ,3 ,5]
[1 ,3 ,4 ,5]
Se o elemento ja existir, pode inseri-lo depois de sua ocorrência.
-}
insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:z)
    |x == y = y : x : z
    |x < y = x : (y:z)
    |otherwise = y : insere x z
{-(r) Calcular o quadrado de cada elemento da lista, ex:
> quadrado [3 ,14 ,1 ,5 ,9]
[9 ,196 ,1 ,25 ,81]
-}
quadrado :: [Int] -> [Int]
quadrado (x:y)
    |y == [] = [x*x]
    |otherwise = [x*x] ++ (quadrado y)
{-(s) Verificar se um elemento pertece a lista, ex: `
> pertence 5 [3 ,14 ,1 ,5 ,9]
True
-}
pertence :: Int -> [Int] -> Bool
pertence w (x:y)
    |y == [] && w /= x = False
    |w == x = True
    |otherwise = (pertence w y)
{-(t) Remover todos os elementos k de uma lista, ex:
> remover_todos 1 [3 ,14 ,1 ,5 ,9 ,1]
[3 ,14 ,5 ,9]
-}
remover_todos :: Int -> [Int] -> [Int]
remover_todos w (x:y)
    |y == [] && w /= x = [x]
    |y == [] = []
    |w == x = (remover_todos w y)
    |otherwise = x : (remover_todos w y)
{-(u) Dada uma lista de tuplas-2, criar uma nova lista com apenas os primeiros elementos
de cada tupla, ex:
> primeiros [(3 ,14) ,(1 ,5) ,(9 ,1)]
[3 ,1 ,9]
-}
primeiros :: [(Int,Int)] -> [Int]
primeiros [] = []
primeiros (x:y)
    |otherwise = [fst x] ++ (primeiros y)
{-(v) Dada uma lista de listas, concatenar todas as sub-listas em uma unica lista, ex:
> concatenar [[3 ,14 ,1 ,5] ,[9 ,1 ,2]]
[3 ,14 ,1 ,5 ,9 ,1 ,2]
-}
concatenar :: [[Int]] -> [Int]
concatenar [] = []
concatenar (x:y)
    |otherwise = x ++ (concatenar y)
{-(w) Dadas duas listas de mesmo tamanho, obter uma terceira lista, representando a diferença
absoluta entre as listas dadas, ex:
> diferenca [1, 3, 2, 8] [2, 5, 6, 8]
[1, 2, 4, 0]
-}
diferenca :: [Int] -> [Int] -> [Int]
diferenca (w:x) (y:z)
    |x == [] && (w-y) > 0 = (w - y) : []
    |x == [] && (w-y) < 0 = ((w-y) * (-1)): []
    |(w - y) < 0 = ((w-y) * (-1)) : (diferenca x z)
    |otherwise = (w - y) : (diferenca x z)

{-(x) Dadas duas listas, verificar se ambas sao exatamente iguais, ou seja, possuem o mesmo tamanho
e os mesmos elementos, ex:
> iguais [1, 3, 2, 8] [1, 3, 2, 8]
True
-}
iguais :: [Int] -> [Int] -> Bool
iguais [] [] = True
iguais (w:x) (y:z)
    |w /= y = False
    |(tamanho x) /= (tamanho z) = False
    |otherwise = (iguais x z)
{-(y) Dada uma lista de numeros reais, calcular a média aritmética dos elementos, ex:
> media [3.0 , 1.4 , 1.0]
1.8
-}
soma_media :: [Double] -> Double
soma_media [] = 0
soma_media (x:y)
    |otherwise = x + (soma_media y)

comp_media :: [Double] -> Double
comp_media [] = 0
comp_media (x:y)
    |otherwise = 1 + (comp_media y)

media :: [Double] -> Double
media [] = 0
media z
    |otherwise = soma_media z / comp_media z

--media [3.0 , 1.4 , 1.0]
--3 / ((media [1.4,1.0])+1)
--3 + 1.4 / ((media [1.0])+1+1)
--3 + 1.4 + 1.0 / ((media [])1+1+1)
--3 + 1.4 + 1.0 / 0+1+1+1)
{-(z) Dado um inteiro n, devolva a lista de todos os numeros inteiros ímpares entre 1 e n,
ex:
> lista_impares 9
[1 ,3 ,5 ,7 ,9]
-}
lista_impares :: Int -> [Int]
lista_impares x
    |x == 1 = [1]
    |x `mod` 2 /= 0 = (lista_impares (x-1))  ++ [x]
    |otherwise =(lista_impares (x-1))
{-3. Implemente, usando recursão, as seguintes operações sobre conjuntos, lembrando que um
conjunto e uma sequência ordenada de elementos não-repetidos:
(a) pertence: define se um dado elemento pertence a um conjunto, ex:
> pertence 2 [1 ,3 ,5]
False
-}
pertence_linha :: Int -> [Int] -> Bool
pertence_linha w (x:y)
    |y == [] && w /= x = False
    |w == x = True
    |otherwise = (pertence_linha w y)
{-(b) uniao: dados dois conjuntos, fornece a uniao deles, ex:
> uniao [1 ,3 ,5] [2 ,3 ,4]
[1 ,2 ,3 ,4,5]
-}
uniao :: [Int] -> [Int] -> [Int]
uniao (w:x) (y:z)
    |x == [] = [w] ++ [y] ++ []
    |w == y = [w] ++ (uniao x z) 
    |otherwise = [w] ++ [y] ++ (uniao x z)
{-
(c) inter: dados dois conjuntos, fornece a intersecção deles, ex:
> inter [1 ,3,5] [1,2,3,4]
[1 ,3]
-}
inter :: [Int] -> [Int] -> [Int]
inter (x:y) z
    |y == [] && pertence x z = [x]
    |y == [] = []
    |pertence x z = x : (inter z y)
    |otherwise = (inter y z)
--Precisa comparar o primeiro termo da primeira com todos os elementos da segunda, se não encontrar nenhum termo igual, tirar fora o primeiro termo e não inclui-lo se encontrar, inclui-lo.
{-
(d) diff: dados dois conjuntos, fornece a diferenc¸a deles, ex:
> diff [1,3,5] [1 ,2 ,3 ,4]
[2 ,4 ,5]
-}
diff :: [Int] -> [Int] -> [Int]
diff x z = (diff' x z) ++ (diff' z x)
     
diff' :: [Int] -> [Int] -> [Int]
diff' (x:y) z
    |y == [] && pertence x z = []
    |y == [] = [x]
    |pertence x z = (diff' y z)
    |otherwise = x:(diff' y z)
{-
(e) subc: dados dois conjuntos, diz se o primeiro e subconjunto do segundo, ex:
> sub_conjunto [2 ,4 ,6] [1 ,2 ,3 ,4 ,5 ,6 ,7 ,8]
True
-}
sub_conjunto :: [Int] -> [Int] -> Bool
sub_conjunto (x:y) z
    |y == [] && pertence x z = True
    |y == [] = False
    |pertence x z = (sub_conjunto y z)
    |otherwise = False

{-
(f) interc: intercalar dois conjuntos de mesmo tamanho em um terceiro conjunto, ex:
> intercala [1 ,5 ,7 ,9 ,10] [2 ,3 ,4 ,5 ,8]
[1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10]
Nao se esqueçaa de declarar corretamento os tipos de cada função.
-}
insere' :: Int -> [Int] -> [Int]
insere' n [] = [n] 
insere' n (x:xs)
    |n < x = n:x:xs
    |n == x = x:xs
    |otherwise = x:(insere' n xs)

ordena :: [Int] -> [Int]
ordena [] = []
ordena (x:y) = insere' x (ordena y)   

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala x y = ordena (x++y)

{-
4. No modulo Char encontramos a função toUpper que converte uma letra minuscula na sua
correspondente maiuscula.
(a) Crie uma função recursiva maius que converte todas as letras de uma palavra em maiusculas;
-}
maius :: [Char] -> [Char]
maius [] = []
maius (x:y)
    |letra x = (toUpper x) : (maius y)
    |otherwise = (maius y)

letra :: Char -> Bool
letra x
    |x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' = True
    |otherwise = False
{-
(b) Usando a função isAlpha, tambem do módulo Char, refaça a função maius para
descartar símbolos e numeros;
-}
maius' :: [Char] -> [Char]
maius' [] = []
maius' (x:y)
    |isAlpha x = (toUpper x) : (maius' y)
    |otherwise = (maius' y)
{-
(c) Fac¸a uma nova função que recebe uma palavra e devolve em tupla a palavra original e a sua
correspondente escrita em maiuscula.
OBS: Para importar o modulo Char usa-se import Data.Char, no seu arquivo fonte 
-}
fac :: [Char] -> ([Char],[Char])
fac [] = ([],[])
fac (x:y) = ((x:y),maius(x:y))

{-
5. Considere uma lista de numeros inteiros como entrada de uma função. A função retorna
verdadeiro se a lista e alternante ou falso, caso contrário. Uma lista e alternante se seus dígitos se
alternam entre par e ımpar.
Exemplos:
[1,2,3,4,1,2] é alternante
[2,1,4,1,6,3,8,1] é alternante
[1,3,2,1] nao é alternante
[2,1,4,3,5,4] nao é alternante
-}
alternante :: [Int] -> Bool
alternante [] = False
alternante (x:y)
    |y == [] = True
    |x `mod` 2 == 0 && (head y) `mod` 2 == 0 = False
    |x `mod` 2 /= 0 && (head y) `mod` 2 /= 0 = False
    |otherwise = (alternante y)
{-
6. A representação binária de um número consiste em realizar sucessivas divisoes deste numero por
2 e imprimir do ultimo para o primeiro, todos os restos das divisões.
Por exemplo:
6 / 2 = 3 (resto 0) ! 3 / 2 = 1 (resto 1) ! 1 / 2 = 0 (resto 1)
6 em binario = 110
Implemente a função onverte :: Int -> [Int], ex:
> converte 23
[1 ,0 ,1 ,1 ,1]
-}
converte :: Int -> [Int]
converte x
    |x  == 0 = []
    |otherwise = converte (x`div`2) ++ [(x `mod` 2)]
{-
7. Escreva uma função que dada uma lista com 0s e 1s, representando um número binário, calcule
seu correspondente na forma decimal.
> converte [1 ,0 ,1 ,1 ,1]
23
-}
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:y)
    |otherwise = (tamanho y) + 1

converte_reverso :: [Int] -> Int
converte_reverso [] = 0
converte_reverso (x:y)
    |x == 0 = (converte_reverso y)
    |otherwise = (converte_reverso y) + (2 ^ (tamanho y))

{-8. Implemente uma função que tem como entrada um número inteiro e que retorna uma lista com
cada dígito do numero separadamente. Dica: parte inteira e resto da divisão por 10.
Exemplo:
> digitos 1234
[1,2,3,4]
-}
digitos :: Int -> [Int]
digitos x
    |x == 0 = []
    |otherwise = digitos (x `div` 10) ++ [x `mod` 10]
