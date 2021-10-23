import Text.Read
import Text.Regex.Posix

-- Функція, яка перевіряє, чи є рядок числом
is_number :: String -> Bool
is_number str
   | (number == Nothing) = False
   | otherwise = True
   where number = readMaybe str :: Maybe Double

-- Функція яка вирішує прості інтеграли
solve_integral :: String -> String
solve_integral x
   | (x == "0") = "C"
   | (is_number x) = x ++ "*x + C"
   | (x == "sin(x)") = "-cos(x) + C"
   | (x =~ "\\`sin\\([0-9]*x\\)\\'" :: Bool) = "-cos(" ++ number ++ "x) / " ++ number ++ " + C"
   | (x == "cos(x)") = "sin(x) + C"
   | (x =~ "\\`cos\\([0-9]*x\\)\\'" :: Bool) = "sin(" ++ number ++ "x) / " ++ number ++ " + C"
   | (x == "exp(x)") = x ++ " + C"
   | (x =~ "\\`exp\\([0-9]*x\\)\\'" :: Bool) = x ++ " / " ++ number ++ " + C"
   | (x =~ "\\`x\\^[0-9]+\\'" :: Bool) = "( x^(" ++ (show $ (read $ number :: Integer) + 1) ++ ") ) / " ++ (show $ (read $ number :: Integer) + 1) ++ " + C"
   | (x =~ "\\`[0-9]+\\^x\\'" :: Bool) = x ++ " / ln(" ++ number ++ ") + C"
   | (x =~ "\\`[0-9]+/x\\'" :: Bool) = "ln|" ++ number ++ "| + C"
   | otherwise = "∫(" ++ x ++ ")dx"
   where number = x =~ "[0-9]+" :: String

-- Головна функція, з якої починається програма
main = do
   -- задання вхідних параметрів
   let a = "-1"
   let f = "x*exp(x)"

   -- putStrLn "Вхідні параметри:"
   putStrLn $ "a(x) = " ++ a
   putStrLn $ "f(x) = " ++ f

   -- знаходження функції u
   let u = "exp(" ++ (solve_integral a) ++ ")"
   putStrLn "Інтегруючий множник:"
   putStrLn $ "u(x) = " ++ u

   -- виведення загального рішення диференціального рівняння
   putStrLn "Загальне рішення диференціального рівняння:"
   putStrLn $ "y = ("  ++ solve_integral (u ++ "*" ++ f) ++ " + C) / (" ++ u ++ ")"
