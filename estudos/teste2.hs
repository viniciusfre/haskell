calcula :: Char -> Float -> Float -> Float
calcula operador n1 n2
    |operador == '+' = n1 + n2
    |operador == '-' = n1 - n2
    |operador == '*' = n1 * n2
    |operador == '/' = n1 / n2
    |otherwise = error "erro!"

calcula_casamento :: Char -> Float -> Float -> Float
calcula_casamento '*' x y = x * y
calcula_casamento '+' x y = x + y
calcula_casamento '-' x y = x - y
calcula_casamento '/' x y = x / y
calcula_casamento _ x y= error "erro!"



