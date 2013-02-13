module Helpful.String where

import Data.Char

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- "   ( huha bla )   " -> " huha bla "
removeWhiteSpaceAndBrackets str = case strippedWithBrackets of
    ("(", stripped, ")")  -> stripped
    otherwise             -> error $ "The string " ++ show str ++
                                     " is not in round brackets."
  where
    strippedWithBrackets =
        (\(x,(y,z)) ->
            (x, y, filter (not . isSpace) z)
        ) $ (\(x,y) ->
                (x, break (== ')') y)
            ) $ span (== '(') $ dropWhile isSpace str

