
type Pattern = String
type Text = String

--Return True if Pattern occurs with shift in T else False
naiveMatch :: Pattern -> Text -> Bool
naiveMatch p [] = False
naiveMatch p t = if p `isPrefix` t then True else naiveMatch p $ tail t
  where isPrefix p t = all id $ zipWith (==) p t
