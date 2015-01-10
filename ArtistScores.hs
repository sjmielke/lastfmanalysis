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
          
          -- {-
          let getText ((start, end), allseconds) = let intToTimeString = show
                                                                       . posixSecondsToUTCTime
                                                                       . fromIntegral
                                                   in putStrLn $  (intToTimeString start)
                                                               ++ " - "
                                                               ++ (intToTimeString end)
                                                               ++ " -> "
                                                               ++ (show $ allseconds `div` 3600)
          let filteredScrobbleList = filter (\(x, _) -> artist x == "Pat Metheny Group") scrobblesWithLength
          let lengths = getMonthLengths now filteredScrobbleList
          mapM_ getText lengths
          -- -}
          
          SQL.close conn
