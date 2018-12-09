main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let inputLines = lines fileContent
    let doublesAndTripples = map hasDoublesAndTripples inputLines
    let (doubles, tripples) = toCount doublesAndTripples
    print (doubles * tripples)

hasDoublesAndTripples :: String -> (Bool, Bool)
hasDoublesAndTripples input = _hasDoublesAndTripples input [] []

_hasDoublesAndTripples :: [Char] -> [Char] -> [Char] -> (Bool, Bool)
_hasDoublesAndTripples [] doubles tripples =
    (not (null filteredDoubles), not (null tripples))
    where 
        filteredDoubles = filter (\double -> double `notElem` tripples) doubles

_hasDoublesAndTripples (h:t) doubles tripples = 
    _hasDoublesAndTripples t newDoubles newTripples
    where 
        newDoubles = if (h `elem` t) then doubles ++ [h] else doubles
        newTripples = if (h `elem` t && h `elem` doubles) then tripples ++ [h] else tripples

toCount :: [(Bool, Bool)] -> (Integer, Integer)
toCount doublesAndTripples = foldl increment (0, 0) doublesAndTripples

increment :: (Integer, Integer) -> (Bool, Bool) -> (Integer, Integer)
increment (doubles, tripples) (hasDouble, hasTripple) =
    (doubles + (toAddend hasDouble), tripples + (toAddend hasTripple))

toAddend :: Bool -> Integer
toAddend x = if x then 1 else 0
