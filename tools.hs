module Tools where

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy s l =
  first :
  splitBy
    s
    ( if null remain
        then remain
        else dropWhile (== ' ') (tail remain)
    )
  where
    first = firstValid l 0
    firstValid [] _ = []
    firstValid (x : xs) c
      | x == s && c == 0 = []
      | x == '(' = x : firstValid xs (c + 1)
      | x == ')' = x : firstValid xs (c - 1)
      | otherwise = x : firstValid xs c
    opening = length (filter (== '(') first)
    closing = length (filter (== ')') first)
    remain = drop (length first) l

removeEmpty :: [String] -> [String]
removeEmpty = filter (not . null)

extractData :: String -> [String]
extractData l = map (filter (/= ' ')) $ removeEmpty (splitBy '\n' l)

removeWhiteSpacesAfterComma :: String -> String
removeWhiteSpacesAfterComma [] = []
removeWhiteSpacesAfterComma (x : xs)
  | x == ',' = x : removeWhiteSpacesAfterComma (dropWhile (== ' ') xs)
  | otherwise = x : removeWhiteSpacesAfterComma xs

removeWhiteSpacesBeforeComma :: String -> String
removeWhiteSpacesBeforeComma l = reverse $ removeWhiteSpacesAfterComma $ reverse l

removeWhiteSpacesAroundComma :: String -> String
removeWhiteSpacesAroundComma = removeWhiteSpacesAfterComma . removeWhiteSpacesAfterComma

removeEndSpace :: String -> String
removeEndSpace l = reverse $ dropWhile (== ' ') $ reverse l