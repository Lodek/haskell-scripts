--Redundant reimplementation of prelude functions

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs


intersperse' :: Char -> String -> String
intersperse' _ [] = []
intersperse' s [x] = [x]
intersperse' s (x:xs) = x : s : intersperse' s xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys
