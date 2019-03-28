{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.Random
import System.Environment

-- Position,Const,Created,Modified,Description,Title,URL,Title Type,IMDb Rating,Runtime (mins),Year,Genres,Num Votes,Release Date,Directors,Your Rating,Date Rated
data Movie = Movie
    { title :: Text
    , year :: Int
    , url :: Text
    } deriving (Show)

instance FromNamedRecord Movie where
    parseNamedRecord r =
        Movie
            <$> fmap T.decodeLatin1 (r .: "Title")
            <*> r .: "Year"
            <*> r .: "URL"

decodeItems :: ByteString -> Either String (Vector Movie)
decodeItems = fmap snd . decodeByName

processCSV :: StdGen -> ByteString -> IO ()
processCSV gen csv =
    case randomMovie gen <$> decodeItems csv of
        Left err -> putStrLn err
        Right m -> print m

randomMovie :: StdGen -> Vector Movie -> Movie
randomMovie gen v = v ! fst (randomR (0, V.length v - 1) gen :: (Int, StdGen))

-- Use WATCHLIST.csv as default when no file is provided
file :: [String] -> String
file [] = "WATCHLIST.csv"
file (a:_) = a

main :: IO ()
main = do
    args <- getArgs
    gen <- getStdGen
    csv <- BL.readFile $ file args
    processCSV gen csv
