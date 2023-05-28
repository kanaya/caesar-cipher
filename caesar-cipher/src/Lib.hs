module Lib
    ( someFunc
    ) where

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

sample_string :: String
sample_string = "haskell is fun"

someFunc :: IO ()
someFunc = putStrLn (encode 3 sample_string)

