get_r complex_num = sqrt ((complex_num !! 0) ** 2 + (complex_num !! 1) ** 2)
get_fi complex_num = acos ((complex_num !! 0) / (get_r complex_num))

sum_list list = [sum (map (!! 0) list), sum (map (!! 1) list)]

main = do 

   let complex_number_list = [[1, 4], [4, 6], [2, 1], [6, 7], [1, 1]]
   putStrLn $ "Список з комплексними числами:"
   print complex_number_list

   let complex_sum = sum_list complex_number_list

   putStrLn $ "Сума всіх елементів списку в показниковій формі:"
   putStrLn $ "z = " ++ show (get_r complex_sum) ++ " * exp(" ++ show (get_fi complex_sum) ++ "i)"



