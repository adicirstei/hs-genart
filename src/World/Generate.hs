{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module World.Generate where

import           Control.Arrow            ((&&&))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Graphics.Rendering.Cairo

import           Data.Colour.RGBSpace     (RGB (..))
import           Data.Colour.RGBSpace.HSV (hsv)

data World = World
  { worldWidth  :: Integer
  , worldHeight :: Integer
  , worldSeed   :: Int
  , worldScale  :: Double
  }

type Generate a = RandT StdGen (ReaderT World Render) a

cairo :: Render a -> Generate a
cairo = lift . lift



getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)


fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill



hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v


