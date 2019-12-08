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
summing [l] = fuelRequirement l
summing (l:ls) = fuelRequirement l + summing ls

fuelRequirement :: Int -> Int
fuelRequirement x   |calc x > 0 = calc x + fuelRequirement (calc x)
                    |calc x <= 0 = 0
