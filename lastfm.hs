{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types (parse)

import Control.Applicative
import Data.Text (Text, pack)
import Data.HashMap.Strict (fromList)
import GHC.Int (Int64)

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

getScrobblePage :: LFM.Connection -> Text -> Text -> Int64 -> IO [Scrobble]
getScrobblePage con userName apiKey page = do
        putStrLn $ "Getting page " ++ show page
        response <- LFM.lastfm con $ LFM.User.getRecentTracks
                                  <*> LFM.user userName
                                  <*> LFM.apiKey apiKey
                                  <* LFM.json
                                  <* LFM.page page
                                  <* LFM.limit 200
        let res = case response of
                      (Right res) -> res
                      (Left e) -> error $ show e

        let trackListExtractor o = parseJSON o
                               >>= (.: "recenttracks")
                               >>= (.: "track")
        let scrobbleList = case parse trackListExtractor res of
                               (Success sl) -> sl :: [Scrobble]
                               (Error e) -> error e
        
        return scrobbleList

main = do putStr "User name: "
          userName <- fmap pack getLine
          
          con <- LFM.newConnection
          scrobbleList <- fmap concat $ mapM (getScrobblePage con userName "e38cc7822bd7476fe4083e36ee69748e") [1..111]
          
          writeFile "/tmp/scrobblelist" $ show scrobbleList
          
          print $ length $ scrobbleList
