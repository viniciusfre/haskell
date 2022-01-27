{-1.Implemente o OU lógico
(a) usando casamento de padrões
-}
ou_logico :: Bool -> Bool -> Bool
ou_logico False False = False
ou_logico _ _ = True
--(b) Usando guardas
ou_logico_guardas :: Bool -> Bool -> Bool
ou_logico_guardas x y
    |x == False && y == False = False
    |otherwise = True

{-2. Em Lógica Proposicional, o operador equivalente ( ↔) é definido pela seguinte tabela-verdade:
p  q  p ↔ q
T  T    T
T  F    F
F  T    F
F  F    T
Implemente a função equivalente :: Bool -> Bool -> Bool corrrespondente ao operador ↔.
-}
--(a) usando casamento de padrões
equivalente :: Bool -> Bool -> Bool
equivalente True True = True
equivalente False False = True
equivalente _ _ = False
--(b) Usando guardas
equivalente_guardas :: Bool -> Bool -> Bool
equivalente_guardas x y
    |x == y = True
    |otherwise = False
{-3. Em Logica Proposicional, o Conectivo de Scheffer e verdadeiro se, pelo menos, um dos
operandos for falso. Corresponde a palavra NAND do idioma inglês. Este conectivo é definido pela
seguinte tabela-verdade:
p q p # q
T T F
T F T
F T T
F F T
onde # corresponde à função scheffer. Forneça implementações para a função scheffer :: Bool ->
Bool -> Bool-}
--(a) (10 pontos) utilizando casamento de padrões
scheffer :: Bool -> Bool -> Bool
scheffer True True = False
scheffer False b = True
scheffer b False = True
--(b) (10 pontos) utilizando guardas
scheffer_guardas :: Bool -> Bool -> Bool
scheffer_guardas x y
    |x == True && y == True = False
    |otherwise = True
{-4. Escreva uma função de nome "calcula" que deverá ser chamada com três parâmetros de
entrada. Caso o primeiro parâmetro seja igual a um asterisco os outros dois parâmetros serão
multiplicados e devolvidos como resposta. Se o primeiro parâmetro for igual a uma barra, o
segundo parâmetro deverá ser dividido pelo terceiro. Caso o primeiro parâmetro seja diferente
de asterisco ou barra uma mensagem de erro deverá ser devolvida como resposta.
Exemplos de uso:
Main> calcula '*' 2 3
6
Main> calcula '/' 4 2
2
Main> calcula '+' 2 3
*** Exception : erro!
OBS: lembre-se que uma mensagem de erro é dada pela função error.-}
calcula :: Char -> Float -> Float -> Float
calcula c x y
    |c == '/' = x/y
    |c == '*' = x*y
    |otherwise = error "erro!"

{-lab06-}
fat :: Int -> Int
fat n
    |n == 0 = 1
    |otherwise = n * fat(n-1)

soma :: Int -> Int
soma n
    |n == 0 = 0
    |otherwise = n +(soma n-1)
    --3 + (soma 2)
    --3 + (2 + soma 1)
    --3 + (2 + 1 + 