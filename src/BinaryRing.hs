{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module BinaryRing where

import           Control.Monad            (when)
import           Control.Monad.Random     (getRandomR, mkStdGen, runRandT)
import           Control.Monad.Reader     (runReaderT, void)
import           Data.Monoid              ((<>))
import           Debug.Trace
import           Graphics.Rendering.Cairo
import           World.Generate


wWidth = 100
wHeight = 100
scaleAmt = 10

data Particle = Particle
  { x   :: Double
  , y   :: Double
  , vx  :: Double
  , vy  :: Double
  , r   :: Double
  , age :: Int
  }

generate :: Int -> IO Surface
generate seed = do
  let
    w = World wWidth wHeight seed (fromIntegral scaleAmt)
    g = mkStdGen seed
  s <- createImageSurface FormatARGB32 (wWidth * scaleAmt) (wHeight * scaleAmt)
  void
    . renderWith s
    . flip runReaderT w
    . flip runRandT g
    $ do
      cairo $ scale (fromIntegral scaleAmt) (fromIntegral scaleAmt)

      render

  pure s

render :: Generate ()
render = do
  fillScreen black 1
  cairo save

  cairo $ translate (fromIntegral wWidth / 2) (fromIntegral wHeight / 2)
  cairo $ do
    white 0.25
    setLineWidth 0.1

  initialParts 2000
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
    >>= moveAllAndFlip
  cairo restore

move :: Particle -> Generate Particle
move Particle {..} = do
  rx <- getRandomR (-0.5, 0.5)
  ry <- getRandomR (-0.5, 0.5)
  let
    nx = x + vx
    ny = y + vy
    nvx = vx + rx
    nvy = vy + ry
    np = Particle nx ny nvx nvy r (age - 1)

  cairo $ do
    moveTo x y
    lineTo nx ny
    stroke

    moveTo (-x) y
    lineTo (-nx) ny
    stroke
  pure np


flipColor :: Generate Double
flipColor = do
  r <- getRandomR (0.0, 1.0)
  when (r < 0.05) (cairo $ white 0.25)
  when (r > 0.95) (cairo $ black 0.25)
  pure r


moveAllAndFlip :: [Particle] -> Generate [Particle]
moveAllAndFlip ps = do
  _ <- flipColor
  traverse move ps


initialParts :: Int -> Generate [Particle]
initialParts n = do
  (w,h) <- getSize
  let ps = part <$> [1..n]
  pure ps
    where
      part i =
        let
          x = (10 * sin (twoPi * fromIntegral i / fromIntegral n))
          y = (10 * cos (twoPi * fromIntegral i / fromIntegral n))
          r = pi * fromIntegral i / fromIntegral n
        in mkPart x y r


mkPart :: Double -> Double -> Double -> Particle
mkPart dx dy r =
  Particle dx dy (0.2*cos(r)) (0.2*sin(r)) r 200



white :: Double -> Render ()
white = hsva 0 0 1

black :: Double -> Render ()
black = hsva 0 0 0


twoPi = 2.0 * pi :: Double
