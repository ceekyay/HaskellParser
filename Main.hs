{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as Txt
import Data.Char
import Data.List as L
import Data.Function (on)
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)

newtype Prose = Prose {
  word :: [Char]
}

instance Show Prose where
  show a = word a

optional :: Parser a -> Parser ()
optional p = option () (try p *> pure ())

specialChars = ['-', '_', '…', '“', '”', '\"', '\'', '’', '@', '#', '$',
                '%', '^', '&', '*', '(', ')', '+', '=', '~', '`', '{', '}',
                '[', ']', '/', ':', ';', ',']

excludeWords = ["the", "a", "of", "be", "to", "and", "in", "that", "have", "I",
                "its", "is", "by", "it", "but", "they", "this", "than", "are", "was", "for", "not", "on",
                "with", "he", "she", "as", "you", "do", "their", "at", "his", "her", "who", "what", "where",
                "when", "how", "why", "which", "or", "we", "us", "them", "our", "your", "all", "for", "an", "will", "shall"
                ,"from", "that", "i", "has", "have", "those", "can"]

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

sentenceParser :: String -> [Prose]
sentenceParser str = case parseOnly wp (T.pack str) of
    Left err -> error err
    Right x -> x
    where
        wp = optional sentenceSeparator *> inputSentence `sepBy1` sentenceSeparator

paraParser :: String -> [Prose]
paraParser str = case parseOnly wp (T.pack str) of
    Left err -> error err
    Right x -> x
    where
      wp = optional paraSeparator *> inputPara `sepBy1` paraSeparator

buildWord :: String -> [String]
buildWord inp = map show ( wordParser inp)

toAllLower :: [String] -> [String]
toAllLower inp = map (map toLower) inp

toDouble :: Integral a => a -> Double
toDouble n = (fromIntegral n) :: Double

numOfWords :: [String] -> Int
numOfWords inp = length inp

numOfSentences :: String -> Int
numOfSentences inp = length inp

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

nSizeWords :: [String] -> Int -> [String]
nSizeWords ls n = map(\x -> (head x)) $ group $ sort $
                  filter ((==n) . length) $ toAllLower ls

matching :: String -> String -> Int -> Int
matching s1 s2 d = length $ filter
                   (\(c,i) -> not (null (matches s2 c i d)))
                   (zip s1 [0..])

matches :: Eq a => [a] -> a -> Int -> Int -> [Int]
matches str c i d = filter (<= d) $
                    map (dist i) (elemIndices c str)
  where dist a b = abs $ a - b

wordSimilarity :: String -> String -> Double
wordSimilarity s1 s2
  | m == 0    = 0.0
  | otherwise = (1/3) * (m/ls1 + m/ls2 + (m-t)/m)
  where ls1 = toDouble $ length s1
        ls2 = toDouble $ length s2
        m' = matching s1 s2 d
        d = fromIntegral $
            max (length s1) (length s2) `div` 2-1
        m = toDouble m'
        t = toDouble $ (m' - matching s1 s2 0) `div` 2

lexicalDensity :: [String] -> Float
lexicalDensity inp =
  let lexicalWords = fromIntegral $ length $ filter (\y -> y `notElem` excludeWords) inp
      totalWords = fromIntegral $ numOfWords inp
  in (lexicalWords/totalWords)*100

main :: IO()
main = do
  --Book-1 Name goes here
  input <- readFile "sample1.txt"
  --Book-2 Name goes here
  input2 <- readFile "sample2.txt"
  --Dont rename this, because its part of another integral function.
  input3 <- readFile "common.txt"
  let book1 = buildWord input
  let book2 = buildWord input2
  let commonWords = buildWord input3
  --Calling all the main useful functions
  let nWords = numOfWords book1
  let nSentences = length $ sentenceParser input
  let avgSentLen = avgSentenceLength nWords nSentences
  let para = paraParser input
  let longest = longestWord book1
  let topW = top10 book1
  let requestedWord = userReqWord book1 ["happiness", "plato"]
  let bookCompare = compareBooks book1 book2
  let unique = uniqueWords book1 commonWords
  let avgWord = avgWordLength book1
  let wordsOfn = nSizeWords book1 13
  let book2Longest = longestWord book2
  let wSim = wordSimilarity longest book2Longest
  let lexicalDen = lexicalDensity book1
  -- {--
  putStrLn "\n############ INSIGHTS ############\n"
  putStr "1. Number of Words: "
  print nWords
  putStr "2. Number of Sentences: "
  print nSentences
  putStr "3. Average sentence length: "
  print avgSentLen
  putStr "4. Number of Paras in the text: "
  print $ length para
  putStr "5. Longest Word in the text: "
  print longest
  putStrLn "\n---------------------------------\n"
  putStr "6. Top 10 Words with highest frequencies: "
  print topW
  putStrLn "\n---------------------------------\n"
  putStr "7. Occurencies of user requested word list: "
  print requestedWord
  putStrLn "\n---------------------------------\n"
  --putStr "8. Replaced the text with user request word. Check replace.txt "
  --putStrLn "\n---------------------------------\n"
  putStr "8. List of Common words from 2 Books: "
  print bookCompare
  putStrLn "\n---------------------------------\n"
  putStr "9. List of Unique words found in the text: "
  print unique
  putStrLn "\n---------------------------------\n"
  putStr "10. Average Word Length: "
  print avgWord
  putStrLn "\n---------------------------------\n"
  putStr "11. Words with 'N' Length: "
  print wordsOfn
  putStrLn "\n---------------------------------\n"
  putStr "12. Similarity Index for 2 longest words from 2 different books: "
  print wSim
  putStrLn "\n---------------------------------\n"
  putStr "13. Lexical Density of the text: "
  print lexicalDen
  putStrLn "\n############## END ##############\n"

  --x --}
