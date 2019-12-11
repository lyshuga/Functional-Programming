
func1 a b c = fst + snd
  where fst = (1 - b) / (1 + b)
        snd = abs ((c - 2 * a) / c ^ 2)

deleteFirst _ [] = [] 
deleteFirst a (b:bc) | a == b    = bc 
                      | otherwise = b : deleteFirst a bc

remove element list = deleteFirst element list

hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

func2 x y z m = length . filter (\x -> sum(x) == m) . removeDuplicates $ map sort (concat [[el] : findseq [el] (remove el ls) | el <- ls])
  where ls = [x,y,z]

findseq el [x] = [(x:el)]
findseq el xs = concat [(x : el) : findseq (x : el) (remove x xs) | x <- xs]

hasDuplicates' xs = not (hasDuplicates xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- make_lists xs n = removeDuplicates (filter hasDuplicates' (map sort (sequenceA $ replicate n xs)))

-- check_equals_sum :: [Int] -> Int -> Bool
-- check_equals_sum ls m = m == sum (ls)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = not $ any (== head xs) (tail xs)


-- Module 1
--Lists

a = [[1,2,3], [1,2,3,4]]

b = [1,2,3]

data Dog = Dog {
  dogName  :: String,
  dogAge   :: Int } deriving (Eq,Ord,Show,Read)

c = [(Dog "Small dog" 1), (Dog "Big dog" 3), (Dog "Old dog" 12)]

d = [[1,2,3], [2], [3]]

e = []

f = [[1,2,3], [4,5], [6,7,8,9]]