solve_bee eps f a b
   | abs (a - b) <= eps = center
   | (f a) * (f center) < 0 = solve_bee eps f a center
   | (f b) * (f center) < 0 = solve_bee eps f center b
   | otherwise = center
   where center = (a + b) / 2

solve_newton eps f df x0 a b
   | (x0 < a) || (x0 > b) = -1 
   | (abs (x1 - x0)) >= eps = (solve_newton eps f df x1 a b)
   | otherwise = x1
   where x1 = x0 - (f x0) / (df x0)

main = do 
   let f = \x -> x ** 2 - 1 - cos (5 * x)
   let df = \ x -> 2 * x + 5 * sin (5 * x)

   print (solve_bee 0.000001 f 1 2)
   print (solve_newton 0.000001 f df 1.5 1 2)