main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let inputLines = lines fileContent
    let sanitisedInputLines = map sanitise inputLines
    let integers = map toInt sanitisedInputLines
    putStrLn (show (sum integers))

toInt :: String -> Integer
toInt string = read string :: Integer

sanitise :: String -> String
sanitise input = 
    if head input == '+'
        then tail input
    else input