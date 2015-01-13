module Christmas where

import LastFM

main = do let lastChristmas = 1387497600
          
          (newScrobbles, oldScrobbles) <- fmap (span ((>lastChristmas) . timestamp)) scrobbleList
          
          let realNewScrobbles = deleteFirstsBy sameTrack
                                                (nubBy sameTrack newScrobbles)
                                                (nubBy sameTrack oldScrobbles)
          
          -- putStrLn $ "Unique track count: " ++ show (length $ nubBy sameTrack scrobbleList)
          -- putStrLn $ "Unique new tracks: " ++ show (length realNewScrobbles)
          
          let artists :: [[Scrobble]]
              artists = partitionWithAttribute artist realNewScrobbles
          let artistsAndTheirAlbums :: [[[Scrobble]]]
              artistsAndTheirAlbums = map (partitionWithAttribute album) artists
          let albums :: [[Scrobble]]
              albums = partitionWithAttribute album realNewScrobbles
          
          writeFile "realnewtracks" $ printBeautifully $ map (\a -> (album $ head a, map title a)) albums

printBeautifully :: [(String, [String])] -> String
printBeautifully = concatMap printSingle
    where printSingle (x, ss) = ("\n" ++ x ++ "\n") ++ concatMap (('\t':) . (++"\n")) ss

sameTrack :: Scrobble -> Scrobble -> Bool
sameTrack (Scrobble _ t1 a1 _) (Scrobble _ t2 a2 _) = t1 == t2 && a1 == a2
