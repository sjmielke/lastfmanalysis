{-# LANGUAGE OverloadedStrings #-}

module Lengths where

import LastFM

import Data.List (genericLength, group, sort, sortBy)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

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
                                   (True, False) -> -- (putStrLn $ "Not found: " ++ show s) >>
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
          
          let ppSeconds s =  show (s `div` 3600)
                          ++ ":"
                          ++ show ((s `mod` 3600) `div` 60)
                          ++ ":"
                          ++ show ((s `mod` 3600) `mod` 60) -- I know the first mod is useless.
          
          {-
          scores <- mapM (\ss -> do
            allLengths <- mapM (getTrackLength conn) ss
            return (artist $ head ss, sum allLengths ) -- `div` length allLengths)
            )
            (partitionWithAttribute artist scrobbleList)
          
          mapM_ (\(a, s) -> putStrLn $ ppSeconds s ++ " (" ++ a ++ ")" )
              $ sortBy (comparing snd) scores
          -- --}
          
          let firstTimestamp = timestamp $ last scrobbleList
          
          let (firstYear, firstMonth, firstDay) = toGregorian
                                                $ utctDay
                                                $ posixSecondsToUTCTime
                                                $ fromIntegral
                                                $ firstTimestamp
          
          now <- fmap round getPOSIXTime
          let listOfMonthStarts = takeWhile (< now)
                                $ concatMap (\year -> [ round
                                                      $ utcTimeToPOSIXSeconds
                                                      $ UTCTime (fromGregorian year month 1) 0
                                                      | month <- [1..12]
                                                      , (year, month) >= (firstYear, firstMonth)
                                                      ] )
                                            [firstYear, firstYear + 1 ..]
          
          let countAllScrobblesBetween start end = fmap sum
                                                 $ mapM (getTrackLength conn)
                                                 $ takeWhile ((>= start) . timestamp)
                                                 $ dropWhile ((> end) . timestamp)
                                                 $ scrobbleList
          
          let intervals = zip (firstTimestamp : tail listOfMonthStarts)
                              (tail listOfMonthStarts ++ [now])
          
          let getText (start, end) = do allseconds <- countAllScrobblesBetween start end
                                        let intToTimeString = show
                                                            . posixSecondsToUTCTime
                                                            . fromIntegral
                                        putStrLn $  (intToTimeString start)
                                                 ++ " - "
                                                 ++ (intToTimeString end)
                                                 ++ " -> "
                                                 ++ (show $ allseconds `div` 3600)
          
          mapM_ getText intervals
          
          SQL.close conn
