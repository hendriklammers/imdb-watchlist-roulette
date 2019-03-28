{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- Position,Const,Created,Modified,Description,Title,URL,Title Type,IMDb Rating,Runtime (mins),Year,Genres,Num Votes,Release Date,Directors,Your Rating,Date Rated
data Movie = Movie
    { title :: Text
    , year :: Int
    , url :: Text
    } deriving (Show)

instance FromNamedRecord Movie where
    parseNamedRecord r =
        Movie <$> fmap T.decodeLatin1 (r .: "Title") <*> r .: "Year" <*>
        r .: "URL"

decodeItems :: ByteString -> Either String (Vector Movie)
decodeItems = fmap snd . decodeByName

main :: IO ()
main = do
    csv <- BL.readFile "watchlist.csv"
    case decodeItems csv of
        Left err -> putStrLn err
        Right v -> putStrLn ("Total movies: " ++ show (V.length v))
