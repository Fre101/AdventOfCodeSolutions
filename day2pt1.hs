import System.IO
import Data.List.Split
import Data.Sequence
import Prelude hiding (lookup)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let temp = map read $ nmbrs contents :: [Int]
        temp1 = fromList temp
        temp2 = update 1 12 temp1
        ops = update 2 2 temp2
    print $ opfn 0 ops
    hClose handle

nmbrs :: String -> [String]
nmbrs contents = splitOn "," contents

opfn :: Int -> Seq Int -> Seq Int
opfn i ops
    | opcode == 1   = opfn next $ sum' i ops 
    | opcode == 2   = opfn next $ product' i ops
    | opcode == 99  = ops
    | otherwise     = error "unkown code encountered"
    where opcode = case lookup i ops of
            Nothing -> error "opcode index out of bound"
            Just x -> x
          next = i + 4

sum' :: Int -> Seq Int -> Seq Int
sum' i ops =
    let adra = case lookup (i+1) ops of
            Nothing -> error "index out of bound"
            Just x -> x
        vala = case lookup adra ops of
            Nothing -> error "index out of bound"
            Just x -> x
        adrb = case lookup (i+2) ops of
            Nothing -> error "index out of bound"
            Just x -> x
        valb = case lookup adrb ops of
            Nothing -> error "index out of bound"
            Just x -> x
        adr = case lookup (i+3) ops of
            Nothing -> error "index out of bound"
            Just x -> x
    in update adr (vala+valb) ops

product' :: Int -> Seq Int -> Seq Int
product' i ops =
    let adra = case lookup (i+1) ops of
            Nothing -> error "index out of bound"
            Just x -> x
        vala = case lookup adra ops of
            Nothing -> error "index out of bound"
            Just x -> x
        adrb = case lookup (i+2) ops of
            Nothing -> error "index out of bound"
            Just x -> x
        valb = case lookup adrb ops of
            Nothing -> error "index out of bound"
            Just x -> x
        adr = case lookup (i+3) ops of
            Nothing -> error "index out of bound"
            Just x -> x
    in update adr (vala*valb) ops