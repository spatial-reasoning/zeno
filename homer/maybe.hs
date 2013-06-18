
f :: Int -> Maybe Int
f a  | a > 0  = Just (a - 1)
     | otherwise = Nothing

comp :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
comp f g a  = do
    b <- f a
    c <- g b
    return c


comp2 :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
comp2 f g a = f a >>= (\b -> g b >>= (\c -> return c))
