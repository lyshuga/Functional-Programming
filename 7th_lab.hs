-- Варіант 10. 
-- Задати многочлен. 
-- Утворити алгебраїчну суму, різницю, множення двох многочленів та зведення до цілої степені. 
-- Здійснити приведення подібних членів, тобто об'єднувати одночлени, що мають однакові набори змінних, 
-- з відповідною заміною коефіцієнтів. 
-- Для подання многочленів в пам’яті використовувати списки.

-- mult ([1,5,2], [1,2,3])
-- mmmult = ([3,2,1],[1,2,3])

-- ([1,5,2,0], [1,2,3,5]) ([3,2,1,8],[1,2,3,4])

-- *Main> summmeb ([1,5,2,1], [1,2,3,4]) ([3,2,1,8],[1,2,3,5])
-- [4,7,3,1,8]

summmeb mult1 mult2 | length el1 >= length el2 = summ el1 (filllist el2 el1)
                    | otherwise = summ el2 (filllist el1 el2)
    where multt1 = (convert (snd mult1) (fst mult1) [] [] 1)
          multt2 = (convert (snd mult2) (fst mult2) [] [] 1)
          el1 = fst multt1
          el2 = fst multt2


filllist ls xs | length xs > length ls = filllist (ls ++ [0]) xs
               | otherwise = ls


convert [] _ newone newones _ = (newones ,newone)
convert arf@(el:fstmultt) ars@(els:sndmultt) newone snewone prev | el - prev > 1 = convert arf ars (newone ++ [prev + 1]) (snewone ++ [0]) (prev + 1)
                                  | otherwise = convert fstmultt sndmultt (newone ++ [el]) (snewone ++ [els]) el
          
summ multt1 multt2 = [el1 + el2 | (el1, i) <- zip multt1 [0..], (el2, n) <- zip multt2 [0..], i == n]


difmeb mult1 mult2 | length el1 >= length el2 = difm el1 (filllist el2 el1)
                    | otherwise = difm (filllist el1 el2) el2 
    where multt1 = (convert (snd mult1) (fst mult1) [] [] 1)
          multt2 = (convert (snd mult2) (fst mult2) [] [] 1)
          el1 = fst multt1
          el2 = fst multt2

difm multt1 multt2 = [el1 - el2 | (el1, i) <- zip multt1 [0..], (el2, n) <- zip multt2 [0..], i == n]

