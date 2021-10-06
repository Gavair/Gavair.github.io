factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

is_factorial :: Integer -> Integer -> Bool
is_factorial n prime_num
   | (n < 1) = False
   | ((factorial prime_num) == n) = True
   | ((factorial prime_num) > n) = False
   | otherwise = is_factorial n (prime_num + 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

is_fibonacci :: Integer -> Integer -> Bool
is_fibonacci n prime_num
   | (n < 0) = False
   | ((fib prime_num) == n) = True
   | ((fib prime_num) > n) = False
   | otherwise = is_fibonacci n (prime_num + 1)

is_sqrt :: Integer -> Integer -> Bool
is_sqrt n prime_num
   | (n < 0) = False
   | (prime_num ^ 2 == n) = True
   | (prime_num ^ 2 > n) = False
   | otherwise = is_sqrt n (prime_num + 1)

final_filter list = filter (\x -> (is_factorial x 1) || (is_fibonacci x 0) || (is_sqrt x 1)) list

main = do 
   let l = [1, 2, 10, -432, 6, 49, 345, 0, -5, 36, 24, 9, 0, 5, 8, 12, 100, 13]
   
   putStrLn "Початковий масив:"
   print l

   putStrLn "Елементи, які є числами фібоначі, факторіалами числа або квадратами числа:"
   print (final_filter l)