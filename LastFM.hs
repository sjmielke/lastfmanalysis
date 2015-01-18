{-# LANGUAGE OverloadedStrings #-}

module LastFM (
      Scrobble(..)
    , scrobbleList
      --
    , partitionWithAttribute
    ) where

import Data.Aeson
import Data.Aeson.Types (parse)

import Control.Applicative
import Data.Function (on)
import Data.List (deleteFirstsBy, nubBy, groupBy, sortBy)
import Data.Ord (comparing)
import Data.Text (Text, pack)
import Data.HashMap.Strict (fromList)
import GHC.Int (Int64)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.User as LFM.User

data Scrobble = Scrobble { timestamp :: Int
                         , title :: String
                         , artist :: String
                         , album :: String
                         } deriving (Eq, Show, Read)

instance FromJSON Scrobble where
    parseJSON (Object v) = Scrobble
            <$> (fmap read $ v .:? "date" .!= stddate >>= (.: "uts"))
            <*> (v .: "name")
            <*> (v .: "artist" >>= (.: "#text"))
            <*> (v .: "album" >>= (.: "#text"))
        where stddate = (fromList [("uts", "42")]) -- now playing doesn't have a "date" - we filter this later

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
        
        -- Remove the "now playing" entry marked with 42 above
        return $ filter ((/=42) . timestamp) scrobbleList

scrobbleList :: IO [Scrobble]
scrobbleList = do --{- Retrieving data takes far too long, so...
                  putStr "User name: "
                  userName <- fmap pack getLine
                  
                  con <- LFM.newConnection
                  scrobbleList <- getPages con userName [] [1..]
                  
                  writeFile "scrobblelist" $ show scrobbleList
                  
                  -- load the scrobblelist from a file \o/ -}
                  fmap read $ readFile "scrobblelist"
    where getPages con userName prev (i:is) =
                do page <- getScrobblePage con
                                           userName
                                           "e38cc7822bd7476fe4083e36ee69748e"
                                           i
                   if page == prev
                   then return prev
                   else do rest <- getPages con userName page is 
                           return $ prev ++ rest

-- General utility stuff.
partitionWithAttribute :: (Ord b) => (a -> b) -> [a] -> [[a]]
partitionWithAttribute accessor = groupBy ((==) `on` accessor)
                                . sortBy (comparing accessor)
