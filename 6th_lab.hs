-- ch(x), arcos(x)

arcos x = (pi / 2) - (foo x 0) - (foo x 1) - (foo x 2)- (foo x 3)- (foo x 4)- (foo x 5)- (foo x 6)

foo x n = fst * snd
    where fst =  ((fac (2 * n)) / ((power 2 (2 * n)) * (power (fac n) 2)))
          snd = ((power x ((2 * n) + 1)) / ((2 * n) + 1))

-- * ((power x  ((2 * n) + 1)) / (2 * n + 1))

-- power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

fac 0 = 1
fac n = n * fac (n - 1)

ch x = ((exp x) + (exp ((-1) * x))) / 2