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
linesToTabRow = (map typesetString) . transpose . (map splitNoteLine)

splitNoteLine :: String -> [String]
splitNoteLine = words

typesetString :: [String] -> String
typesetString stringSpec = "|-" ++ (joinNotesOnString stringSpec) ++ "-|"

joinNotesOnString str = intercalate "-" (map pad2chars str)

pad2chars :: String -> String
pad2chars [a,b] = [a,b]
pad2chars [a] = [a, '-']
