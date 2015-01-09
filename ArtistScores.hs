module ArtistScores where

import LastFM
import Lengths

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
          
          let ppSeconds s =  show (s `div` 3600)
                          ++ ":"
                          ++ show ((s `mod` 3600) `div` 60)
                          ++ ":"
                          ++ show ((s `mod` 3600) `mod` 60) -- I know the first mod is useless.
          
          let filteredScrobbleList = filter (\(x, _) -> artist x == "Pat Metheny Group") scrobblesWithLength
          
          let scores = map (\ss -> (artist . fst $ head ss, sum $ map snd ss))
                           (partitionWithAttribute (artist . fst) scrobblesWithLength)
          
          mapM_ (\(a, s) -> putStrLn $ ppSeconds s ++ " (" ++ a ++ ")" )
              $ dropWhile ((< 3 * 3600) . snd)
              $ sortBy (comparing snd)
              $ scores
          
          let getText (start, end, allseconds) = let intToTimeString = show
                                                                     . posixSecondsToUTCTime
                                                                     . fromIntegral
                                                 in putStrLn $  (intToTimeString start)
                                                             ++ " - "
                                                             ++ (intToTimeString end)
                                                             ++ " -> "
                                                             ++ (show $ allseconds `div` 3600)
          
          lengths <- getMonthLengths filteredScrobbleList
          mapM_ getText lengths
          
          SQL.close conn
