import System.IO

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
        mdls = map read ls
    print $ summing mdls
    hClose handle

calc :: Int -> Int
calc x = x `div` 3 - 2

summing :: [Int]-> Int
summing [l] = calc l
summing (l:ls) = calc l + summing ls