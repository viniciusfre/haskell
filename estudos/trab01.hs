{-
Nome:Vinícius Freitas Venunes de Souza
Matrícula: 12111BSI210
-}
{-UFU/FACOM/BSI
Disciplina: PF
Ref: Trabalho de Programação
Ao resolver a tarefa, se esforce para criar não apenas um código que funcione, mas um código que
seja elegante e conciso. Tente escrever pequenas funções que executam apenas uma única tarefa e,
em seguida, combine essas partes menores para criar funções mais complexas.
Certifique-se de escrever funções exatamente com o nome especificado e tipo de assinatura para
cada exercício. Você pode criar funções auxiliares adicionais com quaisquer nomes e assinaturas de
tipo que desejar.
Título do trabalho: validando números de cartão de crédito
Você já se perguntou como os sites validam o número do seu cartão de crédito quando faz compras
online? Eles não verificam um enorme banco de dados de números, e eles não usam de magia. Na
verdade, a maioria dos provedores de crédito confia em uma fórmula de soma de verificação para
distinguir números válidos de coleções aleatórias de dígitos (ou erros de digitação).
Neste trabalho você implementará o algoritmo de validação de cartões de crédito. Ele segue estas
etapas:
-}

{-1. Dobre o valor de cada segundo dígito começando a partir da direita. Ou seja, o último dígito
permanece inalterado; o penúltimo dígito é duplicado; o terceiro ao último dígito permanece
inalterado; e assim por diante. Por exemplo, [1,3,8,6] torna-se [2,3,16,6].
-}
toDigits :: Integer -> [Integer]
toDigits x
    |x == 0 || x < 0 = []
    |otherwise = toDigits (x `div` 10) ++ [x `mod` 10]
--correto

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    |x == 0 || x < 0 = []
    |otherwise =reverso(toDigits (x))
--correto

reverso :: [Integer] -> [Integer]
reverso [] = []
reverso (x:y)
    |otherwise = (reverso y) ++[x]
--correto

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y)
    |par (x:y) == True = [x*2] ++ (doubleEveryOther y)
    |otherwise = [x] ++ (doubleEveryOther y)
--correto
tamanho :: [Integer] -> Integer
tamanho [] = 0
tamanho (x:y)
    |otherwise = (tamanho y) + 1
--complemento

par :: [Integer] -> Bool
par z
    |(tamanho z) `mod` 2 == 0 = True
    |otherwise = False
--complemento

--[4,6,7,1,3,8,6] torna-se [4,12,7,2,3,16,6].
--30
--[1,5,2,7,4,6,2,5] -> [2,5,4,7,8,6,4,5]
--18
--[1,4,5,3,6,8,3,1,2] -> [1,8,5,6,6,16,3,2,2]
--32
{-2. Adicione os dígitos dos valores duplicados. Por exemplo, [2,3,16,6] torna-se 2 + 3 + 1 +
6 + 6 = 18
-}
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:y)
    |x `div` 10 == 0 = x + (sumDigits y) 
    |otherwise = unidade + (dezena) + (sumDigits y)
    where
        unidade = x`mod` 10
        dezena = x `div` 10
--correto

--[2,3,16,6]
--2 + [3,16,6]
--2 + 3 [16,6]
--2 + 3 + 6 + 10
--2 + 3 + 1 + 6 + 6 []     
{-3. Calcule o resto da divisão do valor obtido anteriormente por 10. Para a acima exemplo, o
remanescente iria ser oito. Se o resultado é igual a 0, então o número é válido.
-}
validate :: Integer -> Bool
validate x
    |sumDigits(doubleEveryOther((toDigits x))) `mod` 10 == 0 = True
    |otherwise = False

{-
Tarefa 1 É preciso primeiramente encontrar os dígitos de um número. Defina as funções
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits deve converter números inteiros positivos em uma lista de dígitos. (Para entradas 0 ou
negativas, toDigits deve retornar a lista vazia.) ToDigitsRev deve fazer o mesmo, mas com os
dígitos invertidos.
Exemplo: toDigits 1234 ==> [1,2,3,4]
Exemplo: toDigitsRev 1234 ==> [4,3,2,1]
Exemplo: toDigits 0 ==> []
Exemplo: toDigits (-17) ==> []
Tarefa 2 Uma vez que nós temos os dígitos na devida ordem, é preciso duplicar o valor de cada
segundo dígito começando a partir da direita. Para isto defina a função especificada a seguir.
doubleEveryOther :: [Integer] -> [Integer]
Exemplo: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Exemplo: doubleEveryOther [1,2,3] ==> [1,4,3]
-}

{-Tarefa 3 A saída de doubleEveryOther tem uma mistura de números de um e dois dígitos . Defina a
função
sumDigits :: [Integer] -> Integer
para calcular a soma de todos os dígitos.
Exemplo: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 ==> 22
-}

{-Tarefa 4 Defina a função
validate :: Integer -> Bool
que indica se um Inteiro pode ser um número de cartão de crédito válido. Isso vai usar todas as
funções definidas anteriormente.
Exemplo: validate 4012888888881881 ==> True
Exemplo : validate 4012888888881882 ==> False
&&&&&&&&&&&&&&&&&&&
-}