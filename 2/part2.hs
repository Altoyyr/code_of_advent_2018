import qualified Data.List as List
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let inputLines = lines fileContent
    print (sameLetters (findIdsWithOneLetterDifference inputLines))

findIdsWithOneLetterDifference :: [String] -> (String, String)
findIdsWithOneLetterDifference [] = error "No Ids were found"
findIdsWithOneLetterDifference (h:t) = case foundItem of 
    Nothing -> findIdsWithOneLetterDifference t
    item -> (h, (Maybe.fromJust item))
    where 
        foundItem = (List.find (\x -> (sameLetterDifferences h x) == 1) t)

sameLetterDifferences :: [Char] -> [Char] -> Int
sameLetterDifferences a b = length (filter (\isSameLetter -> not isSameLetter) (mapToSameLetterIndicators a b))

mapToSameLetterIndicators :: String -> String -> [Bool]
mapToSameLetterIndicators = zipWith (\a b -> a == b)

sameLetters :: (String, String) -> String
sameLetters (a, b) =
    part1 ++ (tail part2)
    where
        indexOfDifferingLetter = Maybe.fromJust (List.elemIndex False (mapToSameLetterIndicators a b)) 
        (part1, part2) = splitAt indexOfDifferingLetter a