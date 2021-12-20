--1. Usando as funções head e tail, defina a função terceiro que devolve o terceiro elemento de uma lista de inteiros.
--terceiro :: [Int] -> [Int]
--terceiro x = tail x 
terceiro :: [Int] -> Int
terceiro x = head(tail(tail x))

--2. Considere a função reverse do preludio-padrão:
-- > reverse [1 ,2 ,3]
--[3 ,2 ,1]
--Utilizando essa função (além das funções head e tail, crie as seguintes funções:
--(a) Função ultimo, que devolve o ultimo elemento de uma string. Exemplo:
-- > ultimo " haskell "
-- ’l’
ultimo :: String -> Char
ultimo x = head(reverse x)
--(b) Função inicio, que devolve todos os elementos da string, exceto o ultimo. Exemplo:
-- > inicio " haskell "
-- " haskel "
inicio :: String -> String
inicio x = reverse(tail(reverse x)) --lleksah

--3. Implemente uma função que receba o primeiro e o último nome de alguém e retorne suas iniciais
--em uma tupla.
--Por exemplo:
-- > iniciais " Haskell " " Curry "
--('H','C')
nome :: String -> String -> (Char,Char) 
nome x y = (head x , head y)


