not_logico :: Bool -> Bool
not_logico True = False
not_logico x = True
    

e_logico :: Bool -> Bool -> Bool
e_logico True True = True
e_logico _ _ = False

ordena :: Char -> Char -> (Char,Char)
ordena a b 

    |a >= b = (b,a)

    |b >= a = (a,b)


