{-1. Implemente o cálculo do mínimo múltiplo comum (MMC) de dois números:
Main > mmc (2, 3)
6
Utilize, para isso, a função mdc vista em aula. Sabe-se que:
mmc (a,b)=
a .b
mdc(a,b)-}
mdc :: (Int,Int) -> Int
mdc (m,n)
    |n == 0 = m
    |n > 0 = mdc(n, m `mod` n)
    
--mdc(6 , 2)
--mdc(2, 6 `mod` 2)
--mdc(2, 0)
--mdc 2



mmc :: (Int,Int) -> Int
mmc (a,b)= div (a*b) (mdc(a,b))
{-2. Pesquise e implemente um metodo recursivo para calcular o MDC de três números. Utilize a
função mdc vista em aula. Sabe-se que:
mdc(a,b, c)=mdc(mdc(a,b), c )
-}

mdc_tres :: (Int,Int,Int) -> Int
mdc_tres (a,b,c) = mdc(mdc(a,b),c)

{-3. Implemente uma função recursiva soma n :: Int ­> Int que computa a soma dos
números de 1 a n.
-}

soma :: Int -> Int
soma n
    |n == 1 = 1
    |n == 0 = error "O n deve ser maior que zero!"
    |n > 0 = n + soma(n-1)

{-4. Implemente uma função recursiva para calcular a soma entre dois números n1 e n2 incluindo os
limites. Em seguida reimplemente essa função para excluir os limites.-}

soma_dois_inclui :: Int -> Int -> Int
soma_dois_inclui n1 n2
    |n2 == n1 = error "Intervalo invalido!"
    |n2 - n1 == 1 = n2 + n1
    |otherwise = soma_dois_inclui n1 (n2-1) + n2

soma_dois_nao_inclui :: Int -> Int -> Int
soma_dois_nao_inclui n1 n2
    |n2 == n1 = error "Intervalo invalido"
    |n2 - n1 == 1 = 0
    |n2 - n1 == 2 = n2-1
    |otherwise = soma_dois_nao_inclui n1 (n2-1) + n2-1

--5 9/6+7+8
--5 8/6+7

{-5. Implemente uma função recursiva que, dados dois números n1 e n2, encontra os multiplos de
um terceiro numero n3 que se encontram nesse intervalo.-}

multiplos :: [Int]-> [Int]
multiplos [n1,n2,n3]
    |n2 - n1 < 0 = error"Numeros invalidos"
    |n2 == n1 && n2 `mod` n3 == 0 = [n1]
    |n2 == n1 && n2 `mod` n3 /= 0 = []
    |n2 - n1 == 1 && n2 `mod` n3 == 0 = [n1+1]
    |n2 - n1 == 1 && n1 `mod` n3 == 0 = [n1]
    |n2 `mod` n3 == 0 = multiplos [n1,n2-1,n3] ++ [n2] 
    |otherwise = multiplos [n1,n2-1,n3]
--n1=6 n2=12 n3=4/
--multiplos [6,11,4] ++ [12] 
--n1=6 n2=11 n3=4
--multiplos [6,10,4] ++ [12] 
--n1=6 n2=10 n3=4
--multiplos [6,9,4] ++ [12]
--n1=6 n2=9 n3=4
--multiplos [6,8,4] ++ [12]
--n1=6 n2=8 n3=4
--multiplos [6,7,4] ++ [12] ++[8]
--n1=6 n2=7 n3=4
--[6] ++ [12] ++[8]

{-6. Implemente uma função recursiva que calcule o número de grupos distintos com k pessoas que
podem ser formados a partir de um conjunto de n pessoas (ou seja, a combinação de n pessoas em
grupos de k. A definição abaixo da função comb(n, k) define as regras:
comb(n , k )=n se k=1
comb(n, k )=1 se k=n
comb(n , k )=comb(n−1, k−1)+comb(n−1, k ) se 1<k<
-}
comb :: (Int,Int) -> Int
comb (n,k)
    |k == 1 = n
    |k == n = 1
    |n > k && k > 1 = comb(n-1,k-1)+comb(n-1,k)


{-7. Seja a função ex
 definida pela seguinte serie de Taylor:
e
x=1+
x
1
1!
+
x
2
2!
+
x
3
3!
+…+
x
n
n!
Implemente o calculo recursivo da soma da série para n = 10 termos e teste para varios valores de x.
Compare os resultados obtidos com o valor dado pela função exp x do preludio-padrão.-}
fat :: Int -> Int
fat n
    |n == 0 = 1
    |otherwise = fat (n-1) * n

taylor :: Float -> Int -> Float
taylor x n
    |n == 0 = 1
    |otherwise = ((x ^ n) / fromIntegral (fat n)) + taylor x (n-1)
{-8. Escreva uma função recursiva conta_digitos que recebe um numero inteiro n e retorna sua
quantidade de dígitos. Exemplo: se n = 132, conta_digitos n retorna 3.-}

conta_digitos :: Int -> Int
conta_digitos n 
    |n == 0 = 1
    |otherwise = conta_digitos' n

conta_digitos' :: Int -> Int
conta_digitos' n
    |n == 0 = 0
    |otherwise = conta_digitos' (n `div` 10) + 1

{-9. Escreva uma função recursiva soma_digitos que recebe um numero inteiro n e retorna a
soma de seus dígitos. Exemplo: se n = 132, soma_digitos n retorna 6.-}
soma_digitos :: Int -> Int
soma_digitos n
    |n == 0 = 0
    |otherwise = soma_digitos(n `div` 10) + n `mod` 10

{-10. Implemente a função recursiva potencia (b, e) :: (Int, Int) ­> Int que eleva
a base b ao expoente e.-}
potencia :: (Int,Int) -> Int
potencia (b,e)
    |e == 0 = 1
    |otherwise = potencia (b,e-1) * b


-- 2 5 = 2 * 2 * 2 * 2 * 2
-- 2 4 = 2 * 2 * 2 * 2
-- 2 3 = 2 * 2 * 2
-- 2 2 = 2 * 2
-- 2 1 = 2
-- 2 0 = 1

{-11. Implemente a função de Ackermann, a qual é definida por:
A(m ,n)=n+1 se m=0
A(m ,n)=A(m−1,1) se m>0 e n=0
A(m,n)=A(m,n−1) se m>0 e n>0
Observação: Teste essa função com valores pequenos (em torno de 0 a 3).-}
levi :: (Int,Int) -> Int
levi (m,n)
    |m == 0 = n+1
    |m > 0 && n == 0 = levi (m-1,1)
    |m > 0 && n > 0 = levi (m,n-1)

{-12. Desconsidere o conhecimento da função sqrt na linguagem Haskell. Uma forma de se obter a
raiz quadrada de um numero qualquer x seria através de busca binária:
• Assuma que a raiz quadrada de x esta entre 1 (início) e x (fim), se x ≥ 1;
• Assuma que a raiz quadrada de x esta entre x (início) e 1 (fim), se x < 1;
• Se o número for negativo, retorne 0.
Para sabermos se um palpite y = (inicio+fim)/2 é a raiz quadrada de x, basta testar
se y   y ∗ é próximo o suficiente de x ou, em outras palavras, se o módulo da diferença entre x e y
∗ y está dentro de uma tolerância definida. Caso contrário, podemos restringir a busca entre início
e y ou entre y e fim. Escreva a função que implemente este algoritmo, considerando 10−6 como
tolerância para o cálculo do resultado.-}

{-raiz :: Int -> Int
raiz x
    |x < 0 == 0
    |x >= 1 = raiz x
    |x < 1 =
    |palpite * palpite == (x-y)*y = palpite 
    |otherwise = 
    where
        palpite == (1+x)/2
        -}

--somaDb 5 9/5 + 6 + 7 + 8 + 9
--
--
