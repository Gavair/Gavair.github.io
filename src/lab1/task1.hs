-- Polinom function
func :: Double -> Double -> Double
func 0 x = 0
func 1 x = 2 * x
func n x = ((2 * n) / (n - 1)) * (func (n - 1) x) + ((n - 1) / (2 * n)) * (func (n - 2) x)

-- Main function
main = do 
   putStrLn "Please input n:"
   inputjar <- getLine
   let n = read inputjar :: Double

   putStrLn "Please input x:"
   inputjar <- getLine
   let x = read inputjar :: Double

   putStrLn "Result:"
   print (func n x)
