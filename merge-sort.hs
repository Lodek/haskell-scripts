-- Merge two lists in ascending order.
-- Assume that both arguments are sorted lists.
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = [] 
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
   | x < y = x : (merge xs (y:ys))
   | otherwise = y : (merge (x:xs) ys)
   
-- Implementation of mergesort
mergesort :: (Ord a) => [a] -> [a]
mergesort [a] = a:[] -- if (lista.length == 1)
mergesort xs = merge (mergesort (take half xs)) (mergesort (drop half xs))
    where half = (length xs) `div` 2
