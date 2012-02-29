module Calculus.Helpful where

import Data.Char
import Data.Maybe

maybeRead :: Read a => String -> Maybe a
maybeRead =
    fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

