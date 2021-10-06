create_array fill_with 0 = []
create_array fill_with 1 = [fill_with]
create_array fill_with num = [fill_with] ++ (create_array fill_with (num - 1))

time_for_each_ship num_of_ships
   | (num_of_ships `mod` 2 == 1) = (create_array 1 (num_of_ships - 1)) ++ [0.5]
   | otherwise = create_array 1 num_of_ships

simulation queue index prev_ships result
   | ((length queue) - 1 == index) = result ++ (time_for_each_ship curr_ships)
   | (curr_ships >= 4) = simulation queue (index + 1) (curr_ships - 4) (result ++ [1, 1, 1, 1])
   | otherwise = simulation queue (index + 1) 0 (result ++ (time_for_each_ship curr_ships))
   where curr_ships = (length (queue !! index)) + prev_ships

sim ships = simulation ships 0 0 []

average list = (sum list) / (fromIntegral ((length list) :: Int))

main = do 
   let ships = [[1, 2, 3, 4, 5], [6, 7], [8], [9, 10, 11]]
   let result = sim ships

   putStrLn "Перелік кораблів, що надходили в порт:"
   print $ ships

   putStrLn "Кількість днів, яка була витрачена на кожен корабель:"
   print $ result
   putStrLn "Мінімальна кількість днів:"
   print $ minimum result
   putStrLn "Середня кількість днів:"
   print $ average result
   putStrLn "Максимальна кількість днів:"
   print $ maximum result
