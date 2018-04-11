{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Base
  ( module Linear.V2
  , module Base
  , module Linear.Vector
  , module Graphics.Rendering.Cairo
  , module Data.Colour.RGBSpace
  , module Data.Colour.RGBSpace.HSV
  , module Control.Monad.Random
  , module Control.Monad.Reader
  ) where


import           Control.Arrow
import           Control.Concurrent
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Foldable            (for_)
import           Graphics.Rendering.Cairo
import           Linear.V2
import           Linear.Vector


data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }

type Generate a = RandT StdGen (ReaderT World Render) a


sourface = createImageSurface FormatARGB32
world = World

cairo :: Render a -> Generate a
cairo = lift . lift


getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)


fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

renderClosedPath :: [V2 Double] -> Render ()
renderClosedPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

render' :: Surface -> World -> StdGen -> Generate a -> IO ()
render' s w g
  = void
  . renderWith s
  . flip runReaderT w
  . flip runRandT g

