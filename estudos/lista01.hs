--1. Dadas as seguintes funcões:
--Incrementa +1 no valor x
inc :: Int -> Int
inc x = x + 1

--Calcula o quadrado do x
quadrado :: Int -> Int
quadrado x = x * x


--Faz a média dos valores a e b
media :: Float -> Float -> Float
media a b = (a + b) / 2.0 

--Mostre qual será o valor das seguintes execuções
--(a) inc (quadrado 5) = 26; inc (5 * 5); inc (25); 25+1; 26
--(b) quadrado (inc 5) = 36; quadrado(5+1); quadrado(6); 6 * 6 = 36
--(c) media (inc 3)(inc 5) = erro/Porque o tipo da função inc e da função media são diferentes
--Justifique sua resposta.

--2. Faça uma função que determine a quarta potência de um número n (n4 ), usando a função que determina o quadrado de um número
--Resposta:

--Determina a quarta potência de um número, usando a função que determina o quadrado de um número.
quartaPotencia :: Int -> Int
quartaPotencia nro = quadrado(quadrado nro)

--3. Faça uma função que, dado um total de segundos, calcule o total de horas.
--Resposta:

--Dado um total de segundos, calcula o total de horas.
calcula_horas :: Float -> Float
calcula_horas sec = sec/3600

--4. Faça uma função que, dado um total de segundos, calcule o total de minutos usando a função definida em 3.
--Resposta:

--Dado um total de segundos, calcula o total de minutos.
calcula_minutos :: Float -> Float
calcula_minutos sec = calcula_horas(sec)*60

--5. Dados dois valores lógicos, faça uma função que implemente a fórmula: (p ∨ q) ∧ ¬(p ∧ q). OBS: ∨ equivale ao “ou” lógico, ∧ representa o “e” lógico e ¬ à negação lógica.
--Resposta:

--Dados dois valores lógicos, implementa a fórmula:(p ∨ q) ∧ ¬(p ∧ q).OBS: ∨ equivale ao “ou” lógico e ∧ representa o “e” lógico.
formula1 :: Bool -> Bool -> Bool
formula1 p q = (p || q) && not(p && q)











