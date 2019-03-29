{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import Data.Maybe
import Text.Read
import qualified Data.Text.Encoding as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.Random
import System.Environment

data Movie = Movie
    { title :: Text
    , year :: Int
    , url :: Text
    , runtime :: Maybe Int
    } deriving (Show)

instance FromNamedRecord Movie where
    parseNamedRecord r =
        Movie
            <$> fmap T.decodeLatin1 (r .: "Title")
            <*> r .: "Year"
            <*> r .: "URL"
            <*> fmap readMaybe (r .: "Runtime (mins)")

decodeItems :: ByteString -> Either String (Vector Movie)
decodeItems = fmap snd . decodeByName

processCSV :: StdGen -> Maybe Int -> ByteString -> IO ()
processCSV gen time csv =
    case randomMovie gen . filterMovies time <$> decodeItems csv of
        Left err -> putStrLn err
        Right m -> print m

randomMovie :: StdGen -> Vector Movie -> Movie
randomMovie gen v = v ! fst (randomR (0, V.length v - 1) gen :: (Int, StdGen))

filterMovies :: Maybe Int -> Vector Movie -> Vector Movie
filterMovies Nothing xs = xs
filterMovies (Just t) xs = V.filter (\m -> fromMaybe 0 (runtime m) <= t) xs

type Arguments = (String, Maybe Int)

parseArguments :: [String] -> Arguments
parseArguments args =
    case args of
        [] -> ("WATCHLIST.csv", Nothing)
        [a] -> (a, Nothing)
        (a:b:_) -> (a, readMaybe b :: Maybe Int)

main :: IO ()
main = do
    args <- getArgs
    gen <- getStdGen
    let (file, time) = parseArguments args
    csv <- BL.readFile file
    processCSV gen time csv
