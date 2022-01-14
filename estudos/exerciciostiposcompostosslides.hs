--Retorna o quociente na primeira e o resto na segunda
modiv :: Int -> Int -> (Int,Int)
modiv arg1 arg2 = ((div arg1 arg2),(mod arg1 arg2))

--Converte as horas, minutos, segundos, para um total de segundos
segundos :: (Int,Int,Int) -> Int
segundos (horas,minutos,segundos) = horas * 3600 + minutos * 60 + segundos
--216000
horario :: Int -> (Int,Int,Int)
horario x = ((div x 3600),(div(mod x 3600)60),(mod(mod x 3600)60))




