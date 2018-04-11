module Main where


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
    width = 20
    height = 20
    scaleAmount = 60
    scaleWidth = round $ fromIntegral width * scaleAmount
    scaleHeight = round $ fromIntegral height * scaleAmount
  sourface <- sourface scaleWidth scaleHeight
  let world = world width height seed scaleAmount
  render' sourface world stdGen $
    do
      cairo $ scale scaleAmount scaleAmount
      renderSketch
  putStrLn "Generating art..."
  surfaceWriteToPNG sourface
    $ "images/example/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG sourface "images/example/latest.png"
