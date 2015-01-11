{-
Take tab written in columns (one chord per row) and make a line of tab in row format.

Copyright (C) 2015 Nicholas Martin Booker

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

-}

import Data.List (transpose, intercalate)

main :: IO ()
main = interact makeTabRow

makeTabRow :: String -> String
makeTabRow = unlines . linesToTabRow . lines

linesToTabRow :: [String] -> [String]
linesToTabRow = (map typesetString) . transpose . (map (padRMax '-' . splitNoteLine))

splitNoteLine :: String -> [String]
splitNoteLine = words

typesetString :: [String] -> String
typesetString stringSpec = "|-" ++ (joinNotesOnString stringSpec) ++ "-|"

joinNotesOnString :: [String] -> String
joinNotesOnString str = intercalate "-" str

padR :: a -> Int -> [a] -> [a]
padR padItem len xs =
    xs ++ (take padLen $ repeat padItem)
    where padLen = max 0 (len - (length xs))

padRMax :: a -> [[a]] -> [[a]]
padRMax padItem ls =
    map (padR padItem (maxLen ls)) ls

maxLen :: [[a]] -> Int
maxLen = maximum . (map length)
