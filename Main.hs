{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as Txt
import Data.Char
import Data.List
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

inputWords :: Parser Prose
inputWords = Prose <$> many1' letter

inputSentence :: Parser Prose
inputSentence = Prose <$> many1' (letter <|> digit <|> space <|> satisfy (inClass specialChars) <|> satisfy (inClass "――") )

inputPara :: Parser Prose
inputPara = Prose <$> many1' (letter <|> digit <|> space <|> satisfy (inClass specialChars) <|> satisfy (inClass "――.?!") )

wordSeparator :: Parser ()
wordSeparator = many1 (space <|> satisfy (inClass specialChars ) <|> satisfy (inClass "0-9――.?!")) >> pure ()

sentenceSeparator :: Parser ()
sentenceSeparator = many1 (space <|> satisfy (inClass "*.?!")) >> pure ()

paraSeparator :: Parser ()
paraSeparator = many1 (satisfy(inClass "\r\t\n")) >> pure ()

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

numOfWords :: String -> Int
numOfWords inp = length $ wordParser inp

numOfSentences :: String -> Int
numOfSentences inp = length $ sentenceParser inp

avgSentenceLength :: Int -> Int -> Float
avgSentenceLength nWords nSentences = (fromIntegral nWords / fromIntegral nSentences)




main :: IO()
main = do
  input <- readFile "test.txt"
  let nWords = numOfWords input
  let nSentences = numOfSentences input
  let para = paraParser input
  print para
  print $ length para
  --putStrLn (unwords sentences)
  {--
  putStrLn "\n############ INSIGHTS ############\n"
  putStr "1. Number of Words: "
  print nWords
  putStr "2. Number of Sentences: "
  print $ nSentences
  putStr "3. Average sentence length: "
  print $ avgSentenceLength nWords nSentences
  putStrLn "\n############## END ##############\n"
  --}
