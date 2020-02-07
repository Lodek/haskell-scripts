
--Ex. 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst a [] = []
removeFst a (x:xs) | a == x = xs
                   | otherwise = x : (removeFst a xs)

--Count number of occurences of an element in a list 
--Ex. 1.13
countOcc :: (Eq a) => a -> [a] -> Int
countOcc x xs = foldl f 0 xs
                  where f acc e = if e == x then acc + 1 else acc

--Ex. 1.14
--Duplicate the nth element of a list n times and return list with duplicates
blowup :: [a] -> [a]

