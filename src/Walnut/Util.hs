module Walnut.Util
    ( replace
    ) where


replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace k v [] = [(k, v)]
replace k v (x:xs)
    | (a, _) <- x, a == k
    = (a, v) : xs
    | otherwise
    = x : replace k v xs
