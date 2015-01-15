module GraphRender where

import ArtistScores

main = do artistProgressions <- getArtistProgressions
          
          writeFile "/tmp/hyped.tsv" $ getTSV artistProgressions
          
          putStrLn $ printOut $ normalizeArtistProgressions artistProgressions
