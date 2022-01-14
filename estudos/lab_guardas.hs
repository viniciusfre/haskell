{-1. Fornecidos tres valores, a, b e c em uma tupla, implemente uma função que retorne quantos
desses tres são iguais. A reposta deve ser 3, se todos sao iguais; 2, se dois sao iguais e um e distinto
dos demais ou 0, se todos sao distintos entre si.-}
qntd_iguais :: (Float,Float,Float) -> Int
qntd_iguais (a,b,c) 
    | a == b && a == c = 3
    | a == b && a /= c = 2
    | a == c && a /= b = 2
    | b == c && b /= a = 2
    | otherwise = 0

{-3. Fac¸a uma função que recebe três valores reais em uma tupla e devolve o menor.-}
menortres :: (Float,Float,Float) -> Float
menortres (a,b,c)
    | a <= b && a <= c = a
    | b <= c = b
    | otherwise = c

{-4. Escreva uma função que receba uma tupla-3 de três inteiros e retorne uma tupla-2 com o maior e
o menor elemento dentre os três.-}
tupla_de_dois :: (Int,Int,Int) -> (Int,Int)
tupla_de_dois (a,b,c)
    |a > b && a > c && b > c = (a,b)
    |a > b && a > c && c > b = (a,c)
    |b > a && b > c && a > c = (b,a)
    |b > a && b > c && c > a = (b,c)
    |c > a && c > b && a > b = (c,a)
    |otherwise = (c,b)

{-5. Seja a seguinte equação do segundo grau: ax2
 + bx + c = 0 sendo que a, b e c sao numeros reais e
a≠0. Essa equação tem:
• duas raízes reais, se b2
 > 4ac;
• uma raiz real, se b2
 = 4ac; e
• nenhuma raiz real, se b2 < 4ac.
Faça uma função que, dados três coeficientes a, b, e c, informe quantas raízes a equação possui-}
raizes :: Float -> Float -> Float -> String
raizes a b c 
    | b2 > delta = "Duas raizes reais"
    | b2 == delta = "Uma raiz real"
    | otherwise = "Nenhuma raiz real"
    where
        b2 = b*b
        delta = 4*a*c

    

