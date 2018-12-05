main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let inputLines = lines fileContent
    let sanitisedInputLines = map sanitise inputLines
    let integers = map toInt sanitisedInputLines
    let repeatedIntegers = cycle integers
    putStrLn (show (findRepeatedSum repeatedIntegers [] 0))

toInt :: String -> Integer
toInt string = read string :: Integer

sanitise :: String -> String
sanitise input = 
    if head input == '+'
        then tail input
    else input

findRepeatedSum :: [Integer] -> [Integer] -> Integer -> Integer
findRepeatedSum (head:tail) foundSums currentSum = 
    if newSum `elem` foundSums
        then newSum
    else findRepeatedSum tail (foundSums ++ [newSum]) newSum
    where newSum = currentSum + head 