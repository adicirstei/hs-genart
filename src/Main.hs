module Main where


-- import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import           Graphics.Rendering.Cairo (surfaceWriteToPNG)
import           GridArt

import           Text.Printf


main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  sourface <- GridArt.init seed

  putStrLn "Generating art..."
  surfaceWriteToPNG sourface
    $ "images/example/"
    <> show seed <> "-" <> show 20 <> ".png"
  surfaceWriteToPNG sourface "images/example/latest.png"
