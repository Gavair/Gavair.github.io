is_equal_triangle :: Double -> Double -> Double -> Bool
is_equal_triangle a b c = (a == b) && (b == c)

get_triangle_radius queue index
   | (index >= (length queue) - 2) = []
   | (is_equal == True) = [(queue !! index) * (sqrt 3) / 3] ++ (get_triangle_radius queue (index + 1))
   | otherwise = get_triangle_radius queue (index + 1)
   where is_equal = is_equal_triangle (queue !! index) (queue !! (index + 1)) (queue !! (index + 2))

main = do 
   let queue = [1, 2, 4, 2, 2, 2, 2, 4, 4, 4, 3, 5, 5, 5]
   putStrLn $ "Список сторін:"
   print queue

   putStrLn $ "Список радіусів трикутників, які можна утворити з переліку"
   print $ get_triangle_radius queue 0

