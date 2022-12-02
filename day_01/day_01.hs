notEmpty :: String -> Bool
notEmpty s = s /= ""

strtok :: [String] -> [String]
strtok list = takeWhile (notEmpty) list 

convertToInt :: String -> Int 
convertToInt [] = -1
convertToInt s = read s :: Int

append :: Int -> [Int] -> [Int]
append a [] = [a]
append a (h:t) = h : append a t

buildCaloryList :: [String] -> [Int]
buildCaloryList [] = []
buildCaloryList content = 
    let cal_list = map convertToInt (strtok content)
    in let cal = foldr (+) 0 cal_list 
    in append cal (buildCaloryList (drop 1 content))

main = do
    input <- lines <$> readFile "input"
    print (foldr max 0 (buildCaloryList input))
