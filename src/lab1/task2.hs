-- is prime number function
is_prime_internal :: Integer -> Integer -> Bool
is_prime_internal num divider
   | divider == 1 = True
   | (num `mod` divider) == 0 = False 
   | otherwise = is_prime_internal num (divider - 1)

-- return true when value is prime
is_prime :: Integer -> Bool
is_prime num = is_prime_internal num (num - 1)

-- get number of prime dividers
num_divisor_of_prime :: Integer -> Integer -> Integer -> Integer
num_divisor_of_prime num divider num_of_divs
   | divider == 1 = num_of_divs
   | ((num `mod` divider) == 0) && (is_prime divider) = num_divisor_of_prime (num `div` divider) divider (num_of_divs + 1)
   | otherwise = (num_divisor_of_prime num (divider - 1) num_of_divs)

-- does number has 2 prime dividers
is_div_2 :: Integer -> Bool
is_div_2 num = (num_divisor_of_prime num (num - 1) 0) == 2

-- does number has 3 prime dividers
is_div_3 :: Integer -> Bool
is_div_3 num = (num_divisor_of_prime num (num - 1) 0) == 3

-- is number power of prime number to n
is_power_of_prime :: Integer -> Integer -> Integer -> Bool
is_power_of_prime num primary_num power
   | (primary_num == 1) = False
   | ((primary_num ^ power) == num) && (is_prime primary_num) = True
   | otherwise = is_power_of_prime num (primary_num - 1) power

-- is root of number is prime
is_pow_2 :: Integer -> Bool
is_pow_2 num = is_power_of_prime num num 2

-- is third root of number is prime
is_pow_3 :: Integer -> Bool
is_pow_3 num = is_power_of_prime num num 3

-- Main function
main = do 
   putStrLn "Please enter value:"
   inputjar <- getLine
   let value = read inputjar :: Integer

   putStrLn "Is product of two numbers?"
   print (is_div_2 value)

   putStrLn "Is product of three prime numbers?"
   print (is_div_3 value)

   putStrLn "Is square of any prime number?"
   print (is_pow_2 value)

   putStrLn "Is cube of any prime number?"
   print (is_pow_3 value)

   putStrLn "Tests:"

   putStrLn "Should be True"
   print (is_div_2 22)
   print (is_div_2 9)
   print (is_div_2 6)
   print (is_div_3 30)
   print (is_div_3 27)
   print (is_div_3 12)
   print (is_pow_2 4)
   print (is_pow_2 9)
   print (is_pow_2 25)
   print (is_pow_2 49)
   print (is_pow_2 121)
   print (is_pow_3 8)
   print (is_pow_3 27)
   print (is_pow_3 125)

   putStrLn "Should be False"
   print (is_div_2 2)
   print (is_div_2 5)
   print (is_div_2 7)
   print (is_div_2 8)
   print (is_div_2 24)
   print (is_div_2 27)
   print (is_div_3 32)
   print (is_div_3 23)
   print (is_div_3 29)
   print (is_div_3 13)
   print (is_pow_2 8)
   print (is_pow_2 27)
   print (is_pow_3 64)
   print (is_pow_2 125)
   print (is_pow_3 9)
   print (is_pow_3 28)
   print (is_pow_3 124)
