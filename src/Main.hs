{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.Random
import System.Environment

data Movie = Movie
    { title :: Text
    , year :: Int
    , url :: Text
    , runtime :: Maybe Int
    }

instance Show Movie where
    show m =
        T.unpack (title m) ++ ", " ++ show (year m) ++ "\n" ++ T.unpack (url m)

instance FromNamedRecord Movie where
    parseNamedRecord m =
        Movie
            <$> fmap TE.decodeLatin1 (m .: "Title")
            <*> m .: "Year"
            <*> m .: "URL"
            <*> fmap readMaybe (m .: "Runtime (mins)")

decodeItems :: ByteString -> Either String (Vector Movie)
decodeItems = fmap snd . decodeByName

randomMovie :: StdGen -> Vector Movie -> Movie
randomMovie gen v = v ! fst (randomR (0, V.length v - 1) gen :: (Int, StdGen))

filterMovies :: Maybe Int -> Vector Movie -> Vector Movie
filterMovies Nothing xs = xs
filterMovies (Just t) xs = V.filter (maybe False (<= t) . runtime) xs

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
    case randomMovie gen . filterMovies time <$> decodeItems csv of
        Left err -> putStrLn err
        Right m -> print m
