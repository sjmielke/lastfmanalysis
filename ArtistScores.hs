module ArtistScores where

import LastFM
import Lengths

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.SQLite.Simple as SQL

main = do conn <- SQL.open "/home/sjm/.config/Clementine/clementine.db"
          
          scrobbleList <- scrobbleList
          
          let filteredScrobbleList = filter (\x -> artist x == "Pat Metheny Group") scrobbleList
          
          let ppSeconds s =  show (s `div` 3600)
                          ++ ":"
                          ++ show ((s `mod` 3600) `div` 60)
                          ++ ":"
                          ++ show ((s `mod` 3600) `mod` 60) -- I know the first mod is useless.
          
          scores <- mapM (\ss -> do
            allLengths <- mapM (getTrackLength conn) ss
            return (artist $ head ss, sum allLengths ) -- `div` length allLengths)
            )
            (partitionWithAttribute artist scrobbleList)
          
          mapM_ (\(a, s) -> putStrLn $ ppSeconds s ++ " (" ++ a ++ ")" )
              $ sortBy (comparing snd) scores
          
          let getText (start, end, allseconds) = let intToTimeString = show
                                                                     . posixSecondsToUTCTime
                                                                     . fromIntegral
                                                 in putStrLn $  (intToTimeString start)
                                                             ++ " - "
                                                             ++ (intToTimeString end)
                                                             ++ " -> "
                                                             ++ (show $ allseconds `div` 3600)
          
          lengths <- getMonthLengths conn filteredScrobbleList
          mapM_ getText lengths
          
          SQL.close conn
