module ArtistScores where

import LastFM
import Lengths

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Database.SQLite.Simple as SQL

main = do conn <- SQL.open "/home/sjm/.config/Clementine/clementine.db"
          
          scrobbleList <- scrobbleList
          
          putStrLn "Read scrobble list."
          
          -- Yay for more manual caching.
          {-
          scrobblesWithLength <- fmap (zip scrobbleList) $ mapM (getTrackLength conn) scrobbleList
          writeFile "/home/sjm/downloads/scrobblelistlengths" $ show scrobblesWithLength
          -- -}
          
          scrobblesWithLength <- fmap read $ readFile "/home/sjm/downloads/scrobblelistlengths"
          
          putStrLn "Retrieved lengths."
          
          let scores = map (\ss -> (artist . fst $ head ss, sum $ map snd ss))
                           (partitionWithAttribute (artist . fst) scrobblesWithLength)
          
          let interestingArtists = map fst $ filter (\(a, s) -> s >= 3 * 3600) scores
          
          now <- fmap round getPOSIXTime
          
          let intToTimeString = show
                              . posixSecondsToUTCTime
                              . fromIntegral
          let getText f ((start, end), val) = putStrLn $  (intToTimeString start)
                                                       ++ " - "
                                                       ++ (intToTimeString end)
                                                       ++ " -> "
                                                       ++ (f val)
          
          {-
          let filteredScrobbleList = filter (\(x, _) -> artist x == "Pat Metheny Group") scrobblesWithLength
          let lengths = getMonthLengths now filteredScrobbleList
          mapM_ (getText $ show . (`div` 3600)) lengths
          -- -}
          
          mapM_ (getText show) $ getHypedArtistsPerFrom now
                                                        (7 * 24 * 3600)
                                                        (2 * 3600)
                                                        0.3
                                                        scrobblesWithLength
          
          
          SQL.close conn
