optional :: Parser a -> Parser ()
optional p = option () (try p *> pure ())
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
longestWord :: String -> String
longestWord inp = maximumBy (compare `on` length) $ buildWord inp
top10 :: String -> [(Int, String)]
top10 inp = L.take 10 $ filter (\(x,y) -> y `notElem` excludeWords) $
            reverse $ sort $
            map (\x -> (length x, head x)) $ group $ sort $
            map (map toLower) $ buildWord inp
userReqWord :: String -> [String] -> [(Int, String)]
userReqWord ls w = filter (\(x, y) -> y `elem` w) $
                   map (\x -> (length x, head x)) $ group $ sort $
                   map (map toLower) $ buildWord ls
uniqueWords :: String -> [String] -> [String]
uniqueWords inp1 inp2 = filter (\x -> x `notElem` inp2) $
                        map (\x -> (head x)) $ group $
                        map (map toLower) $ buildWord inp1
avgWordLength :: [String] -> Float
avgWordLength ls = (fromIntegral (sum $ map (\x -> (length x)) ls)) / fromIntegral (length ls)
compareBooks :: [String] -> [String] -> [String]
compareBooks inp1 inp2 =
  let text1 = map(\x -> (head x)) $ group $ sort $
              filter (\x -> x `notElem` excludeWords) $
              map(map toLower) inp1
      text2 = map(\y -> (head y)) $ group $ sort $
              filter (\x -> x `notElem` excludeWords) $
              map(map toLower) inp2
  in filter (\xy -> xy `elem` text2) text1
  