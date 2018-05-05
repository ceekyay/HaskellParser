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

prose :: Parser Prose
prose = Prose <$> many1' letter

s1 =

separator :: Parser ()
separator = many1 (space <|> s1 ) >> pure ()

wordParser :: String -> [Prose]
wordParser str = case parseOnly wp (T.pack str) of
    Left err -> error err
    Right x -> x
    where
        wp = optional separator *> prose `sepBy1` separator


main :: IO()
main = do
  input <- readFile "small.txt"
  let words = wordParser input
  print words
  print $ length words
