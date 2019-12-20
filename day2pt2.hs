import System.IO
import Data.List.Split
import Data.Sequence
import Prelude hiding (lookup)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let ops = map read $ nmbrs contents :: [Int]
        temp = fromList ops
        rslt = insNoun 0 temp
    print $ 100*(nn rslt) + vrb rslt
    hClose handle

--Data structuren

data NounVerb = NounVerb {nn :: Int, vrb :: Int, answr :: Bool}


--Methods

nmbrs :: String -> [String]
nmbrs contents = splitOn "," contents

insNoun :: Int -> Seq Int-> NounVerb
insNoun noun ops = if answr rslt
                     then rslt
                     else if noun == 99 --insVerb
                          then rslt
                          else insNoun (noun+1) ops
                   where rslt = insVerb (noun, 0) ops
          

insVerb :: (Int, Int) -> Seq Int -> NounVerb
insVerb (noun, verb) ops = if check (noun, verb) ops 
                            then NounVerb noun verb True
                            else if verb == 99 
                                 then NounVerb noun verb False
                                 else insVerb (noun, verb+1) ops

check :: (Int, Int) -> Seq Int -> Bool
check (noun, verb) ops
    | opcode == 19690720 = True
    | otherwise          = False
    where temp = update 1 noun $ update 2 verb ops
          rslt = opfn 0 temp
          opcode = case lookup 0 rslt of
            Nothing -> error "opcodes empty"
            Just x -> x

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

--Specific practical functions
