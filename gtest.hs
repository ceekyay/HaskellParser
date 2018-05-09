newtype Prose = Prose {
  word :: [Char]
}
instance Show Prose where
  show a = word a
optional :: Parser a -> Parser ()
optional p = option () (try p *> pure ())
avgSentenceLength :: Int -> Int -> Float
avgSentenceLength nWords nSentences = (fromIntegral nWords / fromIntegral nSentences)
longestWord :: [String] -> String
longestWord inp = maximumBy (compare `on` length) inp
top10 :: [String] -> [(Int, String)]
top10 inp = L.take 10 $ filter (\(x,y) -> y `notElem` excludeWords) $
            reverse $ sort $
            map (\x -> (length x, head x)) $ group $ sort $ toAllLower inp
userReqWord :: [String] -> [String] -> [(Int, String)]
userReqWord ls w = filter (\(x, y) -> y `elem` w) $
                   map (\x -> (length x, head x)) $ group $ sort $ toAllLower ls
uniqueWords :: [String] -> [String] -> [String]
uniqueWords inp1 inp2 = filter (\x -> x `notElem` inp2) $
                        map (\x -> (head x)) $ group $ sort $ toAllLower inp1
avgWordLength :: [String] -> Float
avgWordLength ls = (fromIntegral (sum $ map (\x -> (length x)) ls)) / fromIntegral (length ls)
compareBooks :: [String] -> [String] -> [String]
compareBooks inp1 inp2 =
  let text1 = map(\x -> (head x)) $ group $ sort $
              filter (\x -> x `notElem` excludeWords) $ toAllLower inp1
      text2 = map(\y -> (head y)) $ group $ sort $
              filter (\x -> x `notElem` excludeWords) $ toAllLower inp2
  in filter (\xy -> xy `elem` text2) text1
  inputWords :: Parser Prose
  inputWords = Prose <$> many1' letter
  inputSentence :: Parser Prose
  inputSentence = Prose <$> many1' (letter <|> digit <|> space <|>
                  satisfy (inClass specialChars) <|> satisfy (inClass "――") )
  inputPara :: Parser Prose
  inputPara = Prose <$> many1' (letter <|> digit <|> space <|>
              satisfy (inClass specialChars) <|> satisfy (inClass "――.?!") )
wordSeparator :: Parser ()
wordSeparator = many1 (space <|> satisfy (inClass specialChars )
                <|> satisfy (inClass "0-9――.?!")) >> pure ()

sentenceSeparator :: Parser ()
sentenceSeparator = many1 (space <|> satisfy (inClass "*.?!")) >> pure ()

paraSeparator :: Parser ()
paraSeparator = many1 (satisfy (isSpace)) >> pure ()

wordParser :: String -> [Prose]
wordParser str = case parseOnly wp (T.pack str) of
    Left err -> error err
    Right x -> x
    where
        wp = optional wordSeparator *> inputWords `sepBy1` wordSeparator
nSizeWords :: [String] -> Int -> [String]
nSizeWords ls n = map(\x -> (head x)) $ group $ sort $
                  filter ((==n) . length) $ toAllLower ls
matching :: Eq a => [a] -> [a] -> Int -> Int
matching s1 s2 d = length $ filter
                   (\(c,i) -> not (null (matches s2 c i d)))
                   (zip s1 [0..])
matches :: Eq a => [a] -> a -> Int -> Int -> [Int]
matches str c i d = filter (<= d) $
                    map (dist i) (elemIndices c str)
  where dist a b = abs $ a - b
jaro :: Eq a => [a] -> [a] -> Double
jaro s1 s2
  | m == 0    = 0.0
  | otherwise = (1/3) * (m/ls1 + m/ls2 + (m-t)/m)
  where ls1 = toDouble $ length s1
        ls2 = toDouble $ length s2
        m' = matching s1 s2 d
        d = fromIntegral $
            max (length s1) (length s2) `div` 2-1
        m = toDouble m'
        t = toDouble $ (m' - matching s1 s2 0) `div` 2
