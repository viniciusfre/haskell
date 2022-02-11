somaDois :: Int -> Int -> Int
somaDois x y
    |x > y = 0
    |otherwise = x + somaDois (x+1) y

        
somaDB :: Int -> Int -> Int
somaDB x y      
    |x == y = x-1
    |x - y == 1 = x+1              
    |otherwise = x + somaDB (x+1) y