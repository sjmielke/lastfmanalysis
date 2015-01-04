{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types (parse)

import Control.Applicative
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (genericLength, deleteFirstsBy, nubBy, groupBy, sortBy, group, sort)
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.HashMap.Strict (fromList)
import GHC.Int (Int64)

import qualified Network.Lastfm as LFM
import qualified Network.Lastfm.User as LFM.User

import qualified Database.SQLite.Simple as SQL

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

getTrackLength :: SQL.Connection -> Scrobble -> IO Int
getTrackLength clemDBConn s = getTrackLengthFromClementineDB clemDBConn s >>=
                              \x -> case x of
                                        Just l -> return l
                                        Nothing -> return 5

getTrackLengthFromClementineDB :: SQL.Connection -> Scrobble -> IO (Maybe Int)
getTrackLengthFromClementineDB clemDBConn s@(Scrobble _ t ar al) =
    getTrackLengthComparing (True, True)
    where getTrackLengthComparing (compAr, compAl) = do
            let query = fromString $
                  "SELECT length/1000000000 FROM songs WHERE " ++
                  (if compAr then "artist=:artist COLLATE NOCASE AND " else "") ++
                  (if compAl then
                      "(album=:album COLLATE NOCASE OR ((:album LIKE (album || '%') OR album LIKE (:album || '%')) AND (LENGTH(album) > 5) AND (LENGTH(:album) > 5))) AND "
                      else "") ++
                  "title=:title COLLATE NOCASE;"
            results <- SQL.queryNamed clemDBConn query $
                (if compAr then ((":artist" SQL.:= ar):) else id) $
                (if compAl then ((":album" SQL.:= al):) else id) $
                [":title" SQL.:= t]
                :: IO [SQL.Only Int]
            case results of
                [l] -> return $ Just $ SQL.fromOnly l
                ls -> -- Perhaps its just clementines history making duplicates:
                     let differentlengths = map head
                                          $ group
                                          $ sort
                                          $ map SQL.fromOnly ls in
                     case differentlengths of
                        [] -> do -- Lastfm corrects some artists, so try ignoring them.
                                 permissiveLength <- case (compAr, compAl) of
                                   (True, True) -> getTrackLengthComparing (False, True)
                                   (False, True) -> getTrackLengthComparing (True, False)
                                   (True, False) -> (putStrLn $ "Not found: " ++ show s) >>
                                                    return Nothing
                                 case permissiveLength of
                                   Nothing -> return Nothing
                                   Just l -> do -- putStrLn $ "Found " ++ show l ++ " for " ++ show s
                                                return permissiveLength
                        [l] -> return $ Just $ fromIntegral l
                        ls -> -- They seem to be indistinguishable,
                              -- so let's just use average over the different lengths.
                              -- This case seems to be rare enough anyway (~0.25%).
                              return $ Just
                                     $ fromIntegral
                                     $ (sum ls) `div` (genericLength ls) -- -}

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
          
          conn <- SQL.open "/home/sjm/.config/Clementine/clementine.db"
          
          allLengths <- mapM (getTrackLength conn) newScrobbles
          print $ sum allLengths
          -- TODO: Avg? Median? Does this make sense?
          
          -- getTrackLength conn (Scrobble {timestamp = 42, title = "One", artist = "Lamb", album = "What Sound Limited Edition"}) >>= print
          
          SQL.close conn

printBeautifully :: [(String, [String])] -> String
printBeautifully = concatMap printSingle
    where printSingle (x, ss) = ("\n" ++ x ++ "\n") ++ concatMap (('\t':) . (++"\n")) ss
