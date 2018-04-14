module Main where


-- import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import           Graphics.Rendering.Cairo (surfaceWriteToPNG)
import           GridArt
import           Text.Printf
import           Tree
import           Walker

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime

  putStrLn "Generating art..."
--  sourface <- GridArt.init seed
  sourface <- Tree.generate seed
  surfaceWriteToPNG sourface
    $ "images/example/"
    <> show seed <> ".png"
  surfaceWriteToPNG sourface "images/example/latest.png"
