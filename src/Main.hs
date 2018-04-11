module Main where


import           Base
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Semigroup        ((<>))
import           Data.Time.Clock.POSIX
-- import           Graphics.Rendering.Cairo
import           GridArt

import           Text.Printf


main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = mkStdGen seed
    width = 60
    height = 60
    scaleAmount = 20
    scaleWidth = round $ fromIntegral width * scaleAmount
    scaleHeight = round $ fromIntegral height * scaleAmount
  sourface <- Base.sourface scaleWidth scaleHeight
  let world = Base.world width height seed scaleAmount
  Base.render' sourface world stdGen $
    do
      cairo $ scale scaleAmount scaleAmount
      renderSketch
  putStrLn "Generating art..."
  surfaceWriteToPNG sourface
    $ "images/example/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG sourface "images/example/latest.png"
