-- Recursion function
func :: Double -> Double -> Double
func _ 0 = 0
func den n = 1 / (den + (func (den + 1) (n - 1)))

-- Main function
main = do 
   print (func 3 100)
