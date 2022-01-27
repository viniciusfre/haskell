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



mmc :: (Int,Int) -> Int
mmc (a,b)= div (a*b) (mdc(a,b))

mdc_tres :: (Int,Int,Int) -> Int
mdc_tres (a,b,c) = mdc(mdc(a,b),c)

soma :: Int -> Int
soma n
    |n == 1 = 1
    |n == 0 = error "O n deve ser maior que zero!"
    |n > 0 = n + soma(n-1)

soma_dois :: Int -> Int -> Int
soma_dois n1 n2
    |n2 == n1 = 0
    |n2 - n1 == 1 = 0
    |n2 > 0 = (soma_dois n1 n2-1) + n2-1




