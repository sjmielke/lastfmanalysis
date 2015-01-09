{-# LANGUAGE OverloadedStrings #-}

module Lengths (
      getTrackLength
    , getMonthLengths
    , getSeasonLengths
    ) where

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

getLengthsPerFrom :: (Int -> Int) -> ([(Scrobble, Int)] -> Int) -> [(Scrobble, Int)] -> IO [(Int, Int, Int)]
getLengthsPerFrom nextFunc firstFunc scrobbleList = do
          let firstTimestamp = firstFunc scrobbleList
          
          now <- fmap round getPOSIXTime
          let listOfIntervalStarts = takeWhile (< now)
                                   $ iterate nextFunc firstTimestamp
          
          let countAllScrobblesBetween start end =
                let sumOfScrobbles = sum
                                   $ map snd
                                   $ takeWhile ((>= start) . timestamp . fst)
                                   $ dropWhile ((> end) . timestamp . fst)
                                   $ scrobbleList
                in (start, end, sumOfScrobbles)
          
          let intervals = zip (listOfIntervalStarts)
                              (tail listOfIntervalStarts ++ [now])
          
          return $ map (uncurry countAllScrobblesBetween) intervals

getMonthLengths :: [(Scrobble, Int)] -> IO [(Int, Int, Int)]
getMonthLengths = getLengthsPerFrom nextMonth (timestamp . fst . last)
    where nextMonth oldstamp = let (y, m, d) = getCalendarTupleFromTimestamp oldstamp
                               in   getTimestampFromCalendarTuple
                                  $ if m == 12
                                    then (y+1, 1, 1)
                                    else (y, m+1, 1)

getSeasonLengths :: [(Scrobble, Int)] -> IO [(Int, Int, Int)]
getSeasonLengths = getLengthsPerFrom nextSeason firstInSeason
    where nextSeason oldstamp = let (y, m, d) = getCalendarTupleFromTimestamp oldstamp
                                    thisYearsList = dropWhile (<= (m, d)) seasonDates
                                in if thisYearsList /= []
                                   then let (m, d) = head thisYearsList
                                        in getTimestampFromCalendarTuple (y, m, d)
                                   else let (m, d) = head seasonDates
                                        in getTimestampFromCalendarTuple (y+1, m, d)
          firstInSeason sl = let (y, m, d) = getCalendarTupleFromTimestamp
                                           $ timestamp
                                           $ fst
                                           $ last sl
                                 thisYearsList = takeWhile (<= (m, d)) seasonDates
                             in if thisYearsList /= []
                                then let (m, d) = last thisYearsList
                                     in getTimestampFromCalendarTuple (y, m, d)
                                else let (m, d) = last seasonDates
                                     in getTimestampFromCalendarTuple (y-1, m, d)
          seasonDates = [(3, 20), (6, 21), (9, 22), (12, 21)]

getTimestampFromCalendarTuple :: (Integer, Int, Int) -> Int
getTimestampFromCalendarTuple (y, m, d) = round
                                        $ utcTimeToPOSIXSeconds
                                        $ UTCTime (fromGregorian y m d) 0

getCalendarTupleFromTimestamp :: Int -> (Integer, Int, Int)
getCalendarTupleFromTimestamp = toGregorian
                              . utctDay
                              . posixSecondsToUTCTime
                              . fromIntegral
