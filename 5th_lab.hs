-- Варіант 10. 
-- Задати текст. 
-- Вставити задане нове слово після кожного входження іншого заданого слова.


insertword text word wordspec = unwords $ map (\x -> insert word wordspec x) (words text)

insert :: String -> String -> String -> String
insert word specword x | x == specword = unwords [x,word]
                       | otherwise = x