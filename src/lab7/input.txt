find_first_element f index list
   | (f (list !! index)) = index
   | otherwise = (find_first_element f (index + 1) list)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

delete_zeros list = filter (\x -> x /= 0) list

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

is_factorial :: Integer -> Integer -> Bool
is_factorial 1 _ = True
is_factorial n prime_num
   | (n <= 1) = False
   | ((factorial prime_num) == n) = True
   | ((factorial prime_num) > n) = False
   | otherwise = is_factorial n (prime_num + 1)
   

get_factorial_numbers list = filter (\x -> (is_factorial x 1)) list

main = do 
   let l = [1, 2, 10, -432, 6, 345, 0, -5, 24, 0]

   let first_positive_id = find_first_element (\x -> x > 0) 0 l
   let first_negative_id = find_first_element (\x -> x < 0) 0 l
   let first_zero_id = find_first_element (\x -> x == 0) 0 l

   putStrLn "Початковий масив:"
   print l
   let part1 = (slice (first_negative_id + 1) first_zero_id l)
   let part2 = (slice (first_positive_id + 1) first_zero_id l)
   let sublists = part1 ++ part2

   putStrLn "а) Підсписок з елементів між першим від’ємним та нульовим елементом та між першим додатним та нульовим:"
   print (sublists)
   putStrLn "б) Без нульових елементів в списку:"
   print (delete_zeros l)
   putStrLn "в) Елементи, які є факторіалами числа:"
   print (get_factorial_numbers l)
