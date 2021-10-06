is_even :: Integer -> Bool
is_even var = var `mod` 2 == 0

main = do 

   let list = [[1, 2], [4, 6], [6, 2], [3, 5], [7, 4], [2, 1], [7, 5]]
   
   putStrLn "Список елементів, де кожен елемент це раціональне число:"
   print list

   let filtered_list = filter (\x -> is_even (x !! 0)) list
   putStrLn "Список елементів, де кожен елемент це раціональне число з парним чисельником:"
   print filtered_list
   putStrLn "Кількість елементів в списку:"
   print $ length filtered_list