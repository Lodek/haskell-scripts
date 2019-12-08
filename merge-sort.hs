
-- Merge two lists in ascending order.
-- Assume that both arguments are sorted lists.
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
    | x < y = x : (merge xs (y:ys))
    | y < x = y : (merge (x:xs) ys)
   
-- Implementation of mergesort
mergesort :: (Ord a) => [a] -> [a]
mergesort [a] = a:[]
mergesort xs = merge (mergesort (take half xs)) (mergesort (drop half xs))
    where half = (length xs) `div` 2
