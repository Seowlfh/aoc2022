score :: String -> Int
score "A" = 1
score "B" = 2
score "C" = 3
score "X" = 2 -- Lose
score "Y" = 0 -- Draw
score "Z" = 1 -- Win
score s = 0  

scoreRound :: Int -> Int -> Int
scoreRound opp your | opp == your               = 3
                    | opp == 3 && your == 1     = 6
                    | opp == 1 && your == 3     = 0
                    | opp > your                = 0
                    | opp < your                = 6

computeScore :: String -> Int -> Int
computeScore opp_move your_move = 
    let opp_score = score opp_move 
        in your_move + scoreRound opp_score your_move

computeMove :: String -> String -> Int
computeMove opp_move outcome =
    let opp_score = score opp_move
        out_score = score outcome
        move = 1 + (mod (opp_score - 1 + out_score) 3)
        in computeScore opp_move move

sumScore :: [String] -> Int
sumScore [] = 0
sumScore (x:[]) = 0
sumScore input = let (opp:r) = input
                     (your:t) = r
    in computeMove opp your + sumScore t

main = do
    input <- words <$> readFile "input"
    print $ (sumScore input)
