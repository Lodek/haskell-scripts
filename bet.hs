import BinaryTree

betBuilder :: [Tree String] -> String -> [Tree String]
betBuilder trees@(x:y:xs) token = let allowedOps = words "+ / * -" in
                                    if token `elem` allowedOps
                                    then (Node token y x) : xs
                                    else (singleton token):trees
betBuilder trees token = singleton token : trees
                             
betFromRpn :: String -> Tree String
betFromRpn exp = head $ foldl betBuilder [] $ words exp


evaluateBet :: (Tree String) -> Int
evaluateBet (Node value EmptyTree EmptyTree) = read value :: Int
evaluateBet (Node op left right) = let a = evaluateBet left
                                       b = evaluateBet right in
                                     case op of "*" -> a * b
                                                "+" -> a + b
                                                "/" -> a `div` b
                                                "-" -> a - b
