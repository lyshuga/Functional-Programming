
-- Написати програму символьного диференціювання (по одній змінній) алгебраїчних виразів, 
-- представлених у формі правильних префіксних виразів, які містять усі арифметичні операції 
-- та вказані у варіанті математичні функції
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


data Table = Table {
      table :: [(String, Int)],
      sameKey :: String -> String -> Bool
    }

-- makeTable :: Int -> Counter

sameKeyFunc x y = x == y

makeTable sameKey = Table {
    table    = []
    , sameKey = sameKeyFunc
    }

lookUp :: Table -> String -> Maybe [Int]
lookUp (Table table sameKey) checkKey | length result > 0 = Just result
                                      | otherwise = Nothing
    where result = [val | (key, val) <- table, checkKey == key]

insert (Table table sameKey) (insertK, insertV) = Table {
    table    =  insertValue table insertK insertV sameKey
    , sameKey = sameKeyFunc
    }
   

customInsert (Table table sameKey) (insertK, insertV) = insertValue table insertK insertV sameKey

showTable (Table table sameKey) = [val | (key, val) <- table]

insertValue [] key value _ | key == "" = []
                           | otherwise = [(key, value)]
insertValue (x:xs) key value sameKey | sameKey (fst x) key = (key, value) : insertValue xs "" value sameKey
                           | otherwise = x : insertValue xs key value sameKey
