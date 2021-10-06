simpson f x h = (h / 3) * ((f x) + 4 * (f (x + h)) + (f (x + 2 * h)))

sum_with_step f from to step
   | (from + step >= to) = f(to)
   | otherwise = (f from) + (sum_with_step f (from + step) to step)

trapezoid f a b n 
   = (b - a) / n * ((a + b) / 2 + sum_with_step f (a + step) (b - step) step)
   where step = ((b - a) / n)

main = do 
   let a = 0
   let b = pi / 2
   let h = (b - a) / 2
   
   let f = \ x -> exp (-(cos x)) * (cos (sin x))

   print $ trapezoid f a b 1000000
   print $ simpson f a h