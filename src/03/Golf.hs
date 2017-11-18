module Golf where

import Data.List (group,sort,transpose)

skips :: [a] -> [[a]]
skips list = [each i list | i <- [1..length list]]

each :: Int -> [a] -> [a]
each n list = [list !! (i - 1) | i <- [n..length list], i `mod` n == 0]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_))
    | y > x && y > z = [y] ++ localMaxima xs
    | otherwise = localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
histogram list = (unlines . reverse $ map stars tranposedGroup) ++ "==========\n0123456789\n"
    where tranposedGroup = transpose $ group $ sort list

stars :: [Integer] -> String
stars xs = concat [if i `elem` xs then "*" else " " | i <- [0..9]]