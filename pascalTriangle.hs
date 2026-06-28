nextRow :: [Int] -> [Int]
nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

pascalTriangle :: [[Int]]
pascalTriangle = iterate nextRow [1]

printTriangle :: Int -> IO ()
printTriangle n = mapM_ putStrLn formattedRows
  where
    selectedRows = take n pascalTriangle
    rowsAsStrings = map (unwords . map show) selectedRows
    maxLen = length (last rowsAsStrings)
    formattedRows = map (\r -> replicate ((maxLen - length r) `div` 2) ' ' ++ r) rowsAsStrings

main :: IO ()
main = do
    printTriangle 18
