countWays :: Int -> [Int] -> Int
countWays 0 _ = 1 
countWays sum coins
    | sum < 0 = 0
    | null coins = 0
    | otherwise = countWays (sum - head coins) coins + countWays sum (tail coins)
 
main :: IO ()
main = do
    putStrLn "Test 1 (Sum 4 with coins [1, 2]):"
    print (countWays 4 [1, 2])
    
    putStrLn "\nTest 2 (Sum 10 with coins [2, 3, 5, 6]):"
    print (countWays 10 [2, 3, 5, 6])
