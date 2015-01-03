{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types (parse)

import Control.Applicative
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (genericLength, deleteFirstsBy, nubBy, groupBy, sortBy, group, sort)
import Data.Text (Text, pack)
import Data.HashMap.Strict (fromList)
import GHC.Int (Int64)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.User as LFM.User

import qualified Database.SQLite as DB

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

sameTrack :: Scrobble -> Scrobble -> Bool
sameTrack (Scrobble _ t1 a1 _) (Scrobble _ t2 a2 _) = t1 == t2 && a1 == a2

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

getTrackLength :: DB.SQLiteHandle -> Scrobble -> IO (Maybe Int)
getTrackLength conn s@(Scrobble _ t ar al) = do
        result <- DB.execParamStatement conn
            ( "SELECT length/1000000000 FROM songs WHERE " ++
              "title=:title AND " ++
              "artist=:artist AND " ++
              "album=:album;" )
            [ (":title", DB.Text t)
            , (":artist", DB.Text ar)
            , (":album", DB.Text al) ]
            :: IO (Either String [[DB.Row DB.Value]])
        case result of
            Left err -> error $ show s ++ " made the database sad: " ++ err
            Right [rows] -> case rows of
                [[(_, DB.Int l)]] -> return $ Just $ fromIntegral l
                l -> -- Perhaps its just clementines history:
                     let differentlengths = map head
                                          $ group
                                          $ sort
                                          $ map (\[(_, DB.Int i)] -> i) l in
                     case differentlengths of
                        [] -> -- (putStr $ "Not found: " ++ show s) >>
                              return Nothing
                        [l] -> return $ Just $ fromIntegral l
                        ls -> -- They seem to be indistinguishable,
                              -- so let's just use average over the different lengths.
                              -- This case seems to be rare enough anyway (~0.25%).
                              return $ Just $ fromIntegral $ (sum ls) `div` (genericLength ls)

main = do {- Retrieving data takes far too long, so...
          putStr "User name: "
          userName <- fmap pack getLine
          
          con <- LFM.newConnection
          scrobbleList <- fmap concat $ mapM (getScrobblePage con userName "e38cc7822bd7476fe4083e36ee69748e") [1..111]
          
          writeFile "/home/sjm/scrobblelist" $ show scrobbleList
          
          -- load the scrobblelist from a file \o/ -}
          scrobbleListFile <- readFile "/home/sjm/downloads/scrobblelist"
          let scrobbleList = read scrobbleListFile
          
          let lastChristmas = 1387497600
          
          let (newScrobbles, oldScrobbles) = span ((>lastChristmas) . timestamp) scrobbleList
          
          let realNewScrobbles = deleteFirstsBy sameTrack
                                                (nubBy sameTrack newScrobbles)
                                                (nubBy sameTrack oldScrobbles)
          
          -- putStrLn $ "Unique track count: " ++ show (length $ nubBy sameTrack scrobbleList)
          -- putStrLn $ "Unique new tracks: " ++ show (length realNewScrobbles)
          
          let partitionWithAttribute accessor = groupBy ((==) `on` accessor)
                                              . sortBy (comparing accessor)
          
          let artists :: [[Scrobble]]
              artists = partitionWithAttribute artist realNewScrobbles
          let artistsAndTheirAlbums :: [[[Scrobble]]]
              artistsAndTheirAlbums = map (partitionWithAttribute album) artists
          let albums :: [[Scrobble]]
              albums = partitionWithAttribute album realNewScrobbles
          
          -- writeFile "/home/sjm/downloads/realnewtracks" $ printBeautifully $ map (\a -> (album $ head a, map title a)) albums
          
          conn <- DB.openReadonlyConnection "/home/sjm/.config/Clementine/clementine.db"
          
          sum <- mapM (getTrackLength conn) newScrobbles
          print $ length $ filter (== Nothing) sum
          print $ length $ filter (/= Nothing) sum
          
          getTrackLength conn (Scrobble {timestamp = 1388055375, title = "Gabriel's Oboe", artist = "Ennio Morricone", album = "The Mission"}) >>= print
          
          DB.closeConnection conn

printBeautifully :: [(String, [String])] -> String
printBeautifully = concatMap printSingle
    where printSingle (x, ss) = ("\n" ++ x ++ "\n") ++ concatMap (('\t':) . (++"\n")) ss
