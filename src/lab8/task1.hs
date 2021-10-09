-- Функція яка знаходить інтеграл в текстовій формі
get_integral :: String -> String
get_integral x = "∫(" ++ x ++ ")dx"

-- Головна функція, з якої починається програма
main = do
   -- задання вхідних параметрів
   let a = "-1"
   let f = "x*exp(x)"

   putStrLn "Вхідні параметри:"
   putStrLn $ "a(x) = " ++ a
   putStrLn $ "f(x) = " ++ f

   -- знаходження функції u
   let u = "exp(" ++ (get_integral a) ++ ")"
   putStrLn "Інтегруючий множник:"
   putStrLn $ "u = " ++ u

   -- виведення загального рішення диференціального рівняння
   putStrLn "Загальне рішення диференціального рівняння:"
   putStrLn $ "y = ("  ++ get_integral (u ++ "*" ++ f) ++ " + C) / (" ++ u ++ ")"
