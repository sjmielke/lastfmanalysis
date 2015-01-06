{-# LANGUAGE OverloadedStrings #-}

module Lengths where

import LastFM

import Data.List (genericLength, group, sort, sortBy)
import Data.Ord (comparing)
import Data.String (fromString)

import qualified Database.SQLite.Simple as SQL

getTrackLength :: SQL.Connection -> Scrobble -> IO Int
getTrackLength clemDBConn s = getTrackLengthFromClementineDB clemDBConn s >>=
                              \x -> case x of
                                        Just l -> return l
                                        Nothing -> return (4*60) -- Reasonable default song length?

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
                                     $ (sum ls) `div` (genericLength ls)

main = do conn <- SQL.open "/home/sjm/.config/Clementine/clementine.db"
          
          scrobbleList <- scrobbleList
          
          scores <- mapM (\ss -> do
            allLengths <- mapM (getTrackLength conn) ss
            return (artist $ head ss, sum allLengths ) -- `div` length allLengths)
            )
            (partitionWithAttribute artist scrobbleList)
          
          mapM_ (\(a, s) -> putStrLn $  show (s `div` 3600)
                                     ++ ":"
                                     ++ show ((s `mod` 3600) `div` 60)
                                     ++ ":"
                                     ++ show ((s `mod` 3600) `mod` 60) -- I know the first mod is useless.
                                     ++ " (" ++ a ++ ")" )
              $ sortBy (comparing snd) scores
          
          -- getTrackLength conn (Scrobble {timestamp = 42, title = "Mensch", artist = "Herbert Grönemeyer", album = "Mensch"}) >>= print
          
          SQL.close conn
