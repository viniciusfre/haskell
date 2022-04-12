--Nome:Vinícius Freitas Venunes de Souza
--Matrícula: 12111BSI210
{-UFU/FACOM
Disciplina: Programação Funcional
Ref: Estudo Dirigido – Tipos Algébricos
Até agora foram apresentados vários tipos intrínsecos da linguagem, como valores booleanos,
caracteres e números. Porém existem certos problemas computacionais que são mais difíceis de
serem modelados com estes valores, como por exemplo os meses.
Exercício: defina um tipo sinônimo Mes, com base nos tipos anteriormente vistos, que possa ter
como valores os meses do ano.
A seguir será mostrado como criar um tipo de dados cujos valores correspondam aos meses do ano.
Pode-se definir um tipo Meses da seguinte maneira:
data Meses = Jan|Fev|Mar|Abr| Mai|Jun|Jul|Ago|Set|Out|Nov|Dez
Este tipo é um enumerated type. Ele é formado por doze valores que são chamados de construtores
(constructors) de tipo. Uma definição de tipo começa sempre com a palavra data, depois vem o
nome do tipo (Meses), que deve começar com letra maiúscula (note que todos os tipos em Haskell
começam com letra maiúscula), e seus construtores (que também começam com letra maiúscula).
Os construtores são todos os valores que um tipo pode assumir. Tipos definidos dessa forma são
denominados tipos algébricos de dados. Um exemplo de tipo algébrico já conhecido é o tipo Bool:
data Bool = True | False
Pode-se criar um tipo que possua vários componentes:
type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade
As funções que manipulam os tipos algébricos podem ser definidas por pattern matching
(casamento de padrões):
mostraPessoa :: Pessoas -> String
mostraPessoa (Pessoa nom idade) = “Nome: “ ++ nom ++ “ Idade: “ ++ show idade
Exemplo:
Main > mostraPessoa (Pessoa “Ana Silva” 22)
Nome: Ana Silva Idade: 22
Um tipo pode trabalhar com valores bem diferentes. Supondo que se queira trabalhar com figuras
geométricas. O tipo pode assumir o valor de um círculo ou de um retângulo. Então:
data Forma = Circulo Float 
O valor para o círculo poderia ser o raio, e para o retângulo poderia ser base e altura. Para se definir
uma função que calcula a área de um objeto do tipo Forma pode-se trabalhar novamente com
pattern matching:
area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a
Outro exemplo do mesmo princípio seria para se trabalhar com endereços. Algumas ruas têm nomes
e outras são representadas por números. Observe o exemplo a seguir.
data Rua = Numero Int Residencia | Nome String Residencia
data Residencia = Casa Int | Apartamento Int Int
Agora se pode definir operações que formatam o endereço usando pattern matching em cima dos
construtores. Quando um tipo é definido, algumas classes podem se instanciadas diretamente
através da palavra reservada deriving:
data Meses = Jan|Fev|Mar|Abr|Mai|Jun|Jul|Ago|Set|Out|Nov|Dez
deriving (Eq, Show, Enum)
No código descrito acima tem-se que Eq, Show e Enum representam nomes de classes. Classes
descrevem coleções de tipos para os quais determinadas operações (ou funções) devem estar
presentes. Para tais classes tem-se que tais operações dizem respeito a operações de igualdade,
conversão de valores do tipo em string e operações relacionais, respectivamente.
Desta maneira, pode-se fazer coisas do tipo:
Main > Jan
Jan
Main > Mar == Mar
True
Main > [Jan .. Set]
[Jan,Fev,Mar,Abr,Mai,Jun,Jul,Ago,Set]
Tarefa: Assista ao vídeo em: https://youtu.be/tQfpwH-TG7Y
Exercício: sua tarefa agora é definir o tipo algébrico Produto. Um produto é composto por seu
código de barra, nome e preço, conforme apresentado no primeiro estudo dirigido.
Exercício: Com base no tipo Produto, crie uma base de dados (no formato de uma lista) de
produtos que poderiam estar presentes num supermercado, conforme apresentado no primeiro
estudo dirigido.
Exercício: Dado um determinado, especificado pelo seu código de barra, obtenha o seu preço.
&&&&&&&&&&&&&
-}
type Preço = Float
type Nome = String
type Codbar = Int
data Produto = Product [(Codbar,Nome,Preço)]

listaDeProdutos :: Produto
listaDeProdutos =Product[(1234, "Oleo DoBom, 1lt" ,1.95),(4756,"Chocolate Cazzeiro, 250g", 1.80),(3216, "Arroz DoBom, 5Kg",2.13),(5823, "Balas Pedregulho, 1Kg",3.79),(4719,"Queijo Mineirim, 1Kg",4.49),(6832,"Iogurte Maravilha, 1Kg",4.99),( 1112,"Rapadura QuebraDente,1Kg",8.0),(1111, "Sal Donorte, 1Kg",2.21),( 1113, "Cafe DoBom, 1Kg",2.85),(1115, "Biscoito Bibi, 1Kg", 8.0),(3814, "Sorvete QGelo, 1l",6.95)]

acha :: Produto -> Codbar -> Preço
acha (Product []) (z) = 0
acha (Product (x:y)) z
    |z == primeiro = terceiro 
    |otherwise = (acha (Product y) z)
    where
        primeiro = first_element (x)
        segundo = second_element (x)
        terceiro = third_element (x)

first_element :: (Codbar,Nome,Preço)-> Codbar
first_element (x,y,z) = x

second_element :: (Codbar,Nome,Preço) -> Nome
second_element (x,y,z) = y

third_element :: (Codbar,Nome,Preço) -> Preço
third_element (x,y,z) = z

achaItem :: Codbar -> Preço
achaItem x = (acha listaDeProdutos x)





