module ArtistScores
    ( normalizeArtistProgressions
    , getTSV
    , printOut
    , getArtistProgressions
    ) where

import LastFM
import Lengths

import Control.Arrow (second)
import Data.Maybe (catMaybes, fromJust)
import Data.List (sort, nub, sortBy)
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Database.SQLite.Simple as SQL
import Text.Printf

intToTimeString :: Int -> String
intToTimeString = show
                . posixSecondsToUTCTime
                . fromIntegral

normalizeArtistProgressions :: [(String, [((Int, Int), Int)])] -> [(String, [((Int, Int), Double)])]
normalizeArtistProgressions artistProgressions = map (second $ map (second $ (/ maxVal) . fromIntegral)) artistProgressions
    where maxVal = fromIntegral
                 $ maximum
                 $ map snd
                 $ concatMap snd artistProgressions

getTSV :: [(String, [((Int, Int), Int)])] -> String
getTSV artistProgressions = outTSVHead ++ "\n" ++ outTSVBody
    where generateTSV (a, intervals) =  a
                                    ++ "\t"
                                    ++ concatMap ((++"\t") . show . snd) intervals
          outTSVHead = ("\t"++)
                     $ concatMap ((++"\t") . intToTimeString . snd . fst)
                     $ snd
                     $ head artistProgressions -- assuming it found anything.
          outTSVBody = unlines
                     $ map generateTSV $ normalizeArtistProgressions artistProgressions

printOut :: [(String, [((Int, Int), Double)])] -> String
printOut as = unlines $ map printArtist as
    where printArtist (a, intervals) =  printf ("%" ++ maxArtistLength ++ "s ") a
                                     ++ map getCharFor intervals
          maxArtistLength = show $ maximum $ map (length . fst) as
          getCharFor (_, x) = " ░▒▓█" !! (min 4 $ round $ x * 5)

getArtistProgressions :: IO [(String, [((Int, Int), Int)])]
getArtistProgressions = do
    conn <- SQL.open "/home/sjm/.config/Clementine/clementine.db"
    
    scrobbleList <- scrobbleList
    
    -- Yay for more manual caching.
    {-
    scrobblesWithLength <- fmap (zip scrobbleList) $ mapM (getTrackLength conn) scrobbleList
    writeFile "scrobblelistlengths" $ show scrobblesWithLength
    -- -}
    scrobblesWithLength <- fmap read $ readFile "scrobblelistlengths"
    
    now <- fmap round getPOSIXTime
    
    let hyped = getHypedArtistsPerFrom now
                                       (3 * 24 * 3600)
                                       (2 * 3600)
                                       0.4
                                       scrobblesWithLength
    
    let allEverHypedArtists = sort $ nub $ map fst $ catMaybes $ map snd hyped
    
    let artistProgressions = zip allEverHypedArtists
                           $ map ( \a -> getLengthsPerFrom
                                         (+ 14 * 24 * 3600)
                                         (const $ timestamp . fst . last $ scrobblesWithLength)
                                         now
                                       $ filter (\(x, _) -> artist x == a) 
                                                scrobblesWithLength
                                 )
                                 allEverHypedArtists
    
    SQL.close conn
    
    return artistProgressions
