betBuilder :: String -> [Int] -> [Int]
betBuilder token trees@(x:y:xs) = if token `elem` alllowedOps
                           then Tree token y x : xs
                           else singletonTree:trees
  where allowedOps = word "* / + -"
                             
betFromRps :: String -> Tree
betFromRps exp = head $ foldl f [] $ words exp

