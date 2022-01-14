{-
1. Forneça uma temperatura em graus Fahrenheit a partir de uma temperatura em
graus Celsius.
O grau Fahrenheit (símbolo: °F) é uma escala de temperatura proposta por Daniel
Gabriel Fahrenheit em 1724. Nesta escala o ponto de fusão da água é de 32 °F e o
ponto de ebulição de 212 °F. Uma diferença de 1,8 grau Fahrenheit equivale à de 1 °C.
-}

--Converte temperatura em celsius para fahrenheit
conversortemperatura :: Float -> Float
conversortemperatura celsius = celsius * 1.8 + 32 
{-
2. Uma empresa decidiu dar a seus funcionários um abono de salario, baseando-se nos
pontos obtidos durante o mês, de acordo com a tabela:
-}
abono :: Int -> Int
abono x
    | x >= 1 && x <= 10 = 100
    | x >= 11 && x <= 20 = 200
    | x >= 21 && x <= 30 = 300
    | x >= 31 && x<= 40 = 400
    | x >= 41 = 500

{-3. Considere que o preço de uma passagem de avião em um trecho pode variar
dependendo da idade do passageiro. Pessoas com 60 anos ou mais pagam apenas
60% do preço total. Crianças até 10 anos pagam 50% e bebês (abaixo de 2 anos)
pagam apenas 10%. Faça uma função que tenha como entrada o valor total da
passagem e a idade do passageiro e produz o valor a ser pago.-}
valor :: Float -> Int -> Float
valor passagem idade
    | idade < 2 = passagem/10
    | idade > 2 && idade <= 10 = passagem/2
    | idade >= 60 = (passagem / 2) + (passagem / 10)
    | otherwise = passagem

--4. Faça uma função que recebe um numero e retorna verdadeiro se o numero for par.
par :: Int -> Bool
par n
    | (mod n 2) == 0 = True
    | otherwise = False

--5. Faça uma função que recebe dois valores e retorna o menor
menor :: Float -> Float -> Float
menor x y
    | x < y = x
    | x > y = y
    | x == y = x

--6. Faça uma função que recebe três valores e retorna o menor
menortres :: Float -> Float -> Float -> Float
menortres x y z
    | x < y && x < z = x
    | y < x && y < z = y
    | z < x && z < y = z

--7. Escreva uma função recursiva para calcular o fatorial de um numero natural.

--8. Especifique as seguintes funções para a manipulação de listas:
--a) nro-elementos: recebe uma lista qualquer e retorna o número de elementos na
--lista.
nroelementos :: [Int] -> Int
nroelementos n = length n

--b) maior: recebe uma lista de números e retorna o maior . 


--c) conta-ocorrencias: recebe um elemento e uma lista qualquer e retorna o número
--de ocorrências do elemento na lista.

--d) unica-ocorrencia: recebe um elemento e uma lista e verifica se existe uma
--única ocorrência do elemento na lista .

--e) maiores-que: recebe um número e uma lista de números e retorna uma lista com
--os números que são maiores do que o valor informado

--f) concatena: recebe duas listas quaisquer e retorna uma terceira lista com os
--elementos da primeira no início e os elementos da segunda no fim.
concatena :: [Int] -> [Int] -> [Int]
concatena a b = a ++ b

--g) duplica: recebe uma lista e retorna uma nova lista contendo a duplicação dos
--elementos da lista original.



