import System.IO  

get_variable_name :: String -> Int -> String
get_variable_name text index
   | (index >= (length text) - 2) = ""
   | ((text !! index) /= ' ') = [text !! index] ++ (get_variable_name text (index + 1))
   | otherwise = ""

get_variables :: String -> Int -> String
get_variables text index
   | (index >= (length text) - 3) = []
   | (current_word == "let ") = get_variable_name text (index + 4) ++ ", " ++ (get_variables text (index + 1))
   | otherwise = get_variables text (index + 1)
   where current_word = [(text !! index)] ++ [(text !! (index + 1))] ++ [(text !! (index + 2))] ++ [(text !! (index + 3))]

main = do 
   let list = []
   handle <- openFile "input.txt" ReadMode
   contents <- hGetContents handle

   let output = get_variables contents 0
   writeFile "output.txt" output
   
