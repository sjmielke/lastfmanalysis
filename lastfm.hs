{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types (Value, Parser, parse)

import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Aeson.Encode.Pretty

import Control.Applicative
import Data.Text (pack)
import Data.HashMap.Strict (fromList)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.User as LFM.User

data Scrobble = Scrobble { timestamp :: Int
                         , title :: String
                         , artist :: String
                         , album :: String
                         } deriving (Eq, Show)

instance FromJSON Scrobble where
    parseJSON (Object v) = Scrobble
            <$> (fmap read $ v .:? "date" .!= stddate >>= (.: "uts"))
            <*> (v .: "name")
            <*> (v .: "artist" >>= (.: "#text"))
            <*> (v .: "album" >>= (.: "#text"))
        where stddate = (fromList [("uts", "42")]) -- now playing doesn't have a "date"

main = do putStr "User name: "
          userName <- fmap pack getLine
          
          con <- LFM.newConnection
          response <- LFM.lastfm con $ LFM.User.getRecentTracks
                                    <*> LFM.user userName
                                    <*> LFM.apiKey "e38cc7822bd7476fe4083e36ee69748e" -- isn't even mine, heh.
                                    <* LFM.json
                                    <* LFM.from 0
          let res = case response of
                        (Right res) -> res
                        (Left e) -> error $ show e
          
          -- BSC.putStrLn . encodePretty $ res
          
          let trackListExtractor o = parseJSON o
                                 >>= (.: "recenttracks")
                                 >>= (.: "track")
          let scrobbleList = case parse trackListExtractor res of
                                 (Success sl) -> sl :: [Scrobble]
                                 (Error e) -> error e
          print $ scrobbleList
