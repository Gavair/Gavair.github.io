-- factorial function
fac :: Double -> Double
fac 0 = 1
fac n = n * fac (n - 1)

-- sh function
sh :: Double -> Double -> Double -> Double
sh x n precision
   | (x ** n) / (fac n) > precision = (x ** n) / (fac n) + (sh x (n + 2) precision)
   | otherwise = 0

-- y function
y :: Double -> Double
y x
   | (-2 <= x) && (x <= 0) = (sh x 1 0.0001) + (sh (x + 2) 1 0.0001)
   | (1 < x) && (x <= 2) = ((sh x 1 0.0001) ** 2) / (sh (x ** 3) 1 0.0001)
   | otherwise = -9999

-- Main function
main = do 
   print (y (-3))
   print (y (-2.5))
   print (y (-2))
   print (y (-1.5))
   print (y (-1))
   print (y (-0.5))
   print (y 0)
   print (y 0.5)
   print (y 1)
   print (y 1.5)
   print (y 2)
   print (y 2.5)
   print (y 3)
