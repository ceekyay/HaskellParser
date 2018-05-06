{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as Txt
import Data.Char
import Data.List
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)


data Prose = Prose {
  word :: [Char]
} deriving Show

optional :: Parser a -> Parser ()
optional p = option () (try p *> pure ())

specialChars = ['-', '_', '…', '“', '”', '\"', '\'', '’', '@', '#', '$',
                '%', '^', '&', '*', '(', ')', '+', '=', '~', '`', '{', '}',
                '[', ']', '/', ':', ';', ',']

inputWords :: Parser Prose
inputWords = Prose <$> many1' letter

inputSentence :: Parser Prose
inputSentence = Prose <$> many1' (letter <|> digit <|> space <|> satisfy (inClass specialChars))


wordSeparator :: Parser ()
wordSeparator = many1 (space <|> satisfy (inClass specialChars ) <|> satisfy (inClass "0-9――.?!")) >> pure ()

sentenceSeparator :: Parser ()
sentenceSeparator = many1 (space <|> satisfy (inClass ".?!")) >> pure ()

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



main :: IO()
main = do
  input <- readFile "sample.txt"
  let words = wordParser input
  let sentences = sentenceParser input
  --print words
  print sentences
  --putStrLn "#######################################"
  --putStr "Number of words: "
  --print $ length words
  print $ length sentences
