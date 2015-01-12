module ArtistScores where

import LastFM
import Lengths

import Data.Maybe (catMaybes, fromJust)
import Data.List (sort, nub, sortBy)
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
          
          let hyped = getHypedArtistsPerFrom now
                                             (3 * 24 * 3600)
                                             (2 * 3600)
                                             0.4
                                             scrobblesWithLength
          mapM_ (getText show) hyped
          
          let allEverHypedArtists = sort $ nub $ map fst $ catMaybes $ map snd hyped
          
          let artistProgressions = zip allEverHypedArtists
                                 $ map ( \a -> getLengthsPerFrom
                                               (+ 14 * 24 * 3600)
                                               (const $ timestamp . last $ scrobbleList)
                                               now
                                             $ filter (\(x, _) -> artist x == a) 
                                                      scrobblesWithLength
                                       )
                                       allEverHypedArtists
          
          mapM_ (getText $ show . (`div` 3600)) $ fromJust
                                                $ lookup "World's End Girlfriend"
                                                $ artistProgressions
          
          let generateTSV (a, intervals) =  a
                                        ++ "\t"
                                        ++ concatMap ((++"\t") . show . snd) intervals
          
          let outTSVHead = ("\t"++)
                         $ concatMap ((++"\t") . intToTimeString . snd . fst)
                         $ snd
                         $ head artistProgressions -- assuming it found anything.
          let outTSVBody = unlines
                         $ map generateTSV artistProgressions
          
          writeFile "/tmp/hyped.tsv" $ outTSVHead ++ "\n" ++ outTSVBody
          
          SQL.close conn
