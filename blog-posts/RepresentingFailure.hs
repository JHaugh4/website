-- ~\~ language=Haskell filename=blog-posts/RepresentingFailure.hs
-- ~\~ begin <<tutorials/04-maybe.md|blog-posts/RepresentingFailure.hs>>[init]
module RepresentingFailure where

-- ~\~ begin <<tutorials/04-maybe.md|safeDiv>>[init]
safeDiv :: Float -> Float -> Maybe Float
safeDiv numerator divisor
    | divisor == 0 = Nothing
    | otherwise    = Just (numerator / divisor)

-- >>> safeDiv 10 5
-- Just 2.0

-- >>> safeDiv 10 0
-- Nothing

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeHead>>[init]
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- >>> safeHead []
-- Nothing

-- >>> safeHead [1,2,3]
-- Just 1

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeTail>>[init]
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

-- >>> safeTail []
-- Nothing

-- >>> safeTail [1,2,3]
-- Just [2,3]

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeHeadPair1>>[init]
safeHeadPair1 :: ([a], [b]) -> (Maybe a, Maybe b)
safeHeadPair1 (xs, ys) = (safeHead xs, safeHead ys)

-- >>> safeHeadPair1 ([1,2,3], [4,5])
-- (Just 1,Just 4)

-- >>> safeHeadPair1 ([], [4,5])
-- (Nothing,Just 4)

-- >>> safeHeadPair1 ([1,2,3], [])
-- (Just 1,Nothing)

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeHeadPair2>>[init]
safeHeadPair2 :: ([a], [b]) -> Maybe (a, b)
safeHeadPair2 (xs, ys) = 
    case safeHead xs of
        Nothing -> Nothing
        Just x  -> 
            case safeHead ys of
                Nothing -> Nothing
                Just y  -> Just (x, y)

-- >>> safeHeadPair2 ([1,2,3], [4,5])
-- Just (1,4)

-- >>> safeHeadPair2 ([], [4,5])
-- Nothing

-- >>> safeHeadPair2 ([1,2,3], [])
-- Nothing

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|caseMatch>>[init]
caseMatch :: Maybe a -> (a -> Maybe b) -> Maybe b
caseMatch ma famb =
    case ma of
        Nothing -> Nothing
        Just a  -> famb a
-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeHeadPair2CM>>[init]
safeHeadPair2CM :: ([a], [b]) -> Maybe (a, b)
safeHeadPair2CM (xs, ys) =
    caseMatch (safeHead xs) (\x ->
        caseMatch (safeHead ys) (\y ->
            Just (x, y)))

-- >>> safeHeadPair2CM ([1,2,3], [4,5])
-- Just (1,4)

-- >>> safeHeadPair2CM ([], [4,5])
-- Nothing

-- >>> safeHeadPair2CM ([1,2,3], [])
-- Nothing

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|unsafeEverySecondElem>>[init]
unsafeEverySecondElem :: [[a]] -> [a]
unsafeEverySecondElem xss = map (head . tail) xss

-- >>> unsafeEverySecondElem [[1,2,3], [4,5]]
-- [2,5]

-- >>> unsafeEverySecondElem [[1,2,3], [4], [5,6]]
-- Prelude.head: empty list

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|fish>>[init]
(<=<) :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
(<=<) bmc amb a = caseMatch (amb a) bmc
-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|flipStructure>>[init]
flipStructure :: [Maybe a] -> Maybe [a]
flipStructure [] = Just []
flipStructure (ma:mas) =
    caseMatch ma (\a ->
        caseMatch (flipStructure mas) (\as ->
            Just (a : as)))
-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeEverySecondElem>>[init]
safeEverySecondElem :: [[a]] -> Maybe [a]
safeEverySecondElem xss = flipStructure $ map (safeHead <=< safeTail) xss

-- >>> safeEverySecondElem [[1,2,3], [4,5], [6,7]]
-- Just [2,5,7]

-- >>> safeEverySecondElem [[1,2,3], [], [6,7]]
-- Nothing

-- ~\~ end

-- ~\~ begin <<tutorials/04-maybe.md|safeHeadPair2Do>>[init]
safeHeadPair2Do :: ([a], [b]) -> Maybe (a, b)
safeHeadPair2Do (xs, ys) = do
    x <- safeHead xs
    y <- safeHead ys
    Just (x, y)

-- >>> safeHeadPair2Do ([1,2,3], [4,5])
-- Just (1,4)

-- >>> safeHeadPair2Do ([], [4,5])
-- Nothing

-- >>> safeHeadPair2Do ([1,2,3], [])
-- Nothing

-- ~\~ end
-- ~\~ begin <<tutorials/04-maybe.md|safeEverySecondElemTrav>>[init]
safeEverySecondElemTrav :: [[a]] -> Maybe [a]
safeEverySecondElemTrav xss = traverse (safeHead <=< safeTail) xss

-- >>> safeEverySecondElemTrav [[1,2,3], [4,5], [6,7]]
-- Just [2,5,7]

-- >>> safeEverySecondElemTrav [[1,2,3], [], [6,7]]
-- Nothing

-- ~\~ end
-- ~\~ end

