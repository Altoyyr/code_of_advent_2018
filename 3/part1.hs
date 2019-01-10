import Text.Parsec
import Control.Monad.Identity

type RawClaim = (Int, Int, Int, Int)
type Claim = [[Int]]

parseClaim :: String -> RawClaim
parseClaim input = case parse claim "" input of 
    Left _ -> error ("Unable to parse claim '" ++ input ++ "'")
    Right claim -> claim

claim :: Parsec String () RawClaim
claim = do 
    char '#'
    many digit
    space
    char '@'
    space
    return ()
    rawOffsetX <- read <$> many digit
    char ','
    rawOffsetY <- read <$> many digit
    char ':'
    space
    rawSizeX <- read <$> many digit
    char 'x'
    rawSizeY <- read <$> many digit
    return (rawOffsetX, rawOffsetY, rawSizeX , rawSizeY)

readFileByLines :: IO [String]
readFileByLines = do
    fileContent <- readFile "input.txt"
    return $ lines fileContent

rawClaimToClaim :: RawClaim -> Claim
rawClaimToClaim (offsetX, offsetY, width, height) =
    [row y | y <- [0..offsetY+height-1]]
    where
        row y = map (value y) [0..offsetX+width-1]  
        value y x = if x < offsetX || y < offsetY then 0 else 1

combineClaims :: Claim -> Claim -> Claim
combineClaims a b = zipWithPadding a b rowZip ([], [])
    where 
        rowZip a' b' = zipWithPadding a' b' (+) (0,0)

zipWithPadding :: [a] -> [b] -> (a -> b -> c) -> (a,b) -> [c]
zipWithPadding [] [] _ _ = []
zipWithPadding (a:as) [] f defaults@(_,b) = f a b : zipWithPadding as [] f defaults
zipWithPadding [] (b:bs) f defaults@(a,_) = f a b : zipWithPadding [] bs f defaults
zipWithPadding (a:as) (b:bs) f defaults = f a b : zipWithPadding as bs f defaults

santaSuite = do
    lines <- readFileByLines
    return $ length $ filter (>1) $ concat $ foldl combineClaims [[]] $ map rawClaimToClaim $ map parseClaim lines


