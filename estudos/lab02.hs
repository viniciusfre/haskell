--Incrementa +1 no valor x
inc :: Int -> Int
inc x = x+1

--Calcula o quadrado do x
quadrado :: Int -> Int
quadrado x = x * x

--Faz a média dos valores a e b
media :: Float -> Float -> Float
media a b = (a + b) / 2.0

--Calcula área do retângulo
-- c comprimento/ l largura
area_retangulo :: Int -> Int -> Int
area_retangulo c l = c * l

--Calcula área do quadrado
--l1 e l2 são os lados 
area_quadrado :: Int -> Int -> Int
area_quadrado l1 l2 = l1 * l2

--Calcula área do triângulo
--b comprimento da base/ h altura
area_triangulo :: Float -> Float -> Float
area_triangulo b h = (b * h) / 2

--Calcula área do trapézio
-- b1 base maior/b2 base menor/h altura
area_trapezio :: Float -> Float -> Float -> Float
area_trapezio b1 b2 h = ((b1 + b2) * h) /2

--Calcula área do circulo
--r é o raio
area_circulo :: Float -> Float
area_circulo r = pi*r^2

--Calcula área da coroa circular/ r1 é o raio da coroa maior e r2 da coroa menor
area_coroa :: Float -> Float -> Float
area_coroa r1 r2 = pi * (r1^2 - r2^2)

--Calcula volume do cubo
-- c = comprimento/ h= altura/ l= largura
volume_cubo :: Int -> Int -> Int -> Int
volume_cubo c h l = c * h * l

--Calcula volume de um paralelepipedo
--c comprimento/ h altura/ l largura
volume_paralelepipedo :: Int -> Int -> Int -> Int
volume_paralelepipedo c h l = c * h * l

--Calcula volume de uma piramide regular
--l1b lado1 da base/ lado2 da base/ h altura
volume_piramide :: Float -> Float -> Float -> Float
volume_piramide l1b l2b h = (l1b * l2b * h) / 3

--Calcula volume de uma esfera
--r é raio
volume_esfera :: Float -> Float
volume_esfera r = 4/3 * pi * r^3

--Calcula hipotenusa de um triângulo retângulo
--co= cateto oposto/ca= cateto adjacente
hipotenusa_triangulo :: Float -> Float -> Float 
hipotenusa_triangulo co ca = sqrt(co^2+ca^2)

--Dado um ponto (x, y),    determina a distância desse ponto à origem (0, 0)
calcula_distancia :: Float -> Float -> Float 
calcula_distancia x1 y1 = sqrt((x1)^2 +(y1)^2)

--Dados dois pontos (xa, ya) e (xb, yb), determina a distância entre esses pontos.
distancia_pontos :: Float -> Float -> Float -> Float -> Float
distancia_pontos xa ya xb yb = sqrt(((xa-xb) ^ 2 + (ya-yb) ^ 2))

--Determina o cubo de um número.
cubo :: Int -> Int
cubo nro = nro^3

--Determina a quarta potência de um número, usando a função que determina o quadrado de um número.
quarta_potencia :: Int -> Int
quarta_potencia nro = quadrado(quadrado nro)

--Dado um total de segundos, calcula o total de horas.
calcula_horas :: Float -> Float
calcula_horas sec = sec/3600

--Dado um total de segundos, calcula o total de minutos.
calcula_minutos :: Float -> Float
calcula_minutos sec = sec/60

--Dada uma temperatura em graus Fahrenheit, converte em graus Celsius
converte_f_c :: Float -> Float
converte_f_c f = (5 * f - 160) / 9

--Dada uma temperatura em graus Kelvin, converte em graus Celsius.
converte_k_c :: Float -> Float
converte_k_c k = k - 273.15

--Dada uma temperatura em graus Fahrenheit, converte em graus Kelvin.
converte_f_k :: Float -> Float
converte_f_k f = (f + 459.67) * 5/9

--Dada uma velocidade em quilômetros por hora, converte em metros por segundo
converte_km_ms :: Float -> Float
converte_km_ms km = km/3.6

--Dados dois valores lógicos, implementa a fórmula:(p ∨ q) ∧ ¬(p ∧ q).OBS: ∨ equivale ao “ou” lógico e ∧ representa o “e” lógico.
formula1 :: Bool -> Bool -> Bool
formula1 p q = (p || q) && not(p && q)

--Dados três valores lógicos, implementa a fórmula:(p ∨ q) ∧ r.
formula2 :: Bool -> Bool -> Bool -> Bool
formula2 p q r = (p || q) && r

--Dados três valores lógicos, implementa a fórmula:(p ∧ q) ∨ ¬(p ∧ r)
formula3 :: Bool -> Bool -> Bool -> Bool
formula3 p q r = (p && q) || not(p && r)

--Dados quatro valores lógicos, implementa a fórmula:p ∨ (q ∧ r) ∨¬s
formula4 :: Bool -> Bool -> Bool -> Bool -> Bool
formula4 p q r s = p || (q && r) || not s

--Dados quatro valores lógicos, implementa a fórmula:¬(p ∨ q) ∧ (r ∨s) ∧ ¬r
formula5 :: Bool -> Bool -> Bool -> Bool -> Bool
formula5 p q r s = not (p || q) && (r || s) && not r
























