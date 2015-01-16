module GraphRender where

import ArtistScores

import Data.Char (ord)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.SRGB.Linear (rgb)
import Data.List (genericLength)
import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG, B)
import Diagrams.Backend.Cairo (renderCairo, Cairo)

main = do artistProgressions <- getArtistProgressions
          
          let naps = normalizeArtistProgressions artistProgressions
          
          putStrLn "Got Data."
          
          renderSVG "/tmp/img.svg"
                    (mkSizeSpec (Just 1200) Nothing)
                    (dg naps :: Diagram B R2)
          
          renderCairo "/tmp/img.png"
                      (mkSizeSpec (Just 1200) Nothing)
                      (dg naps :: Diagram Cairo R2)
          

    where dg aps = strutX 0.1
                   |||
                   (     strutY 0.1
                     === ( text "Artist Progressions"
                         # font "Helvetica Neue"
                         # bold
                         # fontSize (Local 0.15)
                         <> strutY 0.2
                         )
                     === strutY 0.1
                     === vcat (map line aps) # center
                     === strutY 0.15
                   )
                   |||
                   strutX 0.2
          line (a, is) = (artistname 1 a ||| hcat (map (cell a) is) ||| artistname 0 a)
                       <> strutY 0.2
          artistname align name = (strutX (abs $ align - 0.1) |||
                                    (alignedText align 0.5 name # fontSize (Local 0.05)
                                                                # font "Helvetica Neue"
                                     <> strutY 0.1)
                                  ||| strutX (abs $ align - 0.9))
          cell a (_,x) = rect 0.1 0.15 # fc (uncurryRGB rgb $ hsl h s l) # lwL 0.001
            where h = fromIntegral $ sum (map ord a) `mod` 360
                  s = 0.8
                  l = 1 - sqrt x
