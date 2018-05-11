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
  , worldSeed   :: Integer
  , worldScale  :: Double
  }

type RandGen a = ReaderT World (Rand StdGen) a
type ColorFn = Double -> Render ()


type Generate a = RandT StdGen (ReaderT World Render) a

run :: World -> StdGen -> RandGen a -> (a, StdGen)
run w g =
  flip runRand g
  . flip runReaderT w

cairo :: Render a -> Generate a
cairo = lift . lift



getSize :: RandGen (Integer, Integer)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure ( w,  h)


-- fillScreen :: (Double -> Render a) -> Double -> Generate ()
-- fillScreen color opacity = do
--   (w, h) <- getSize @Double
--   cairo $ do
--     rectangle 0 0 w h
--     color opacity *> fill



hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v


