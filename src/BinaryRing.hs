{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module BinaryRing (init, step, render) where

import           Control.Monad            (when)
import           Control.Monad.Random     (getRandomR, getRandomRs, mkStdGen,
                                           runRandT)
import           Control.Monad.Reader     (runReaderT, void)
import           Data.Monoid              ((<>))
import           Debug.Trace
import           Graphics.Rendering.Cairo
import           Prelude                  hiding (init)
import           World.Generate

wWidth = 240
wHeight = 140
scaleAmt = 5
world seed = World wWidth wHeight seed (fromIntegral scaleAmt)
generator = mkStdGen



data Particle = Particle
  { x   :: Double
  , y   :: Double
  , vx  :: Double
  , vy  :: Double
  , r   :: Double
  , age :: Int
  }

init :: Generate [Particle]
init = createParticles

step :: [Particle] -> Generate [Particle]
step = moveAllAndFlip

render :: Int -> Generate [Particle] -> Render ()
render seed imgDef = do
  let
    w = World wWidth wHeight seed (fromIntegral scaleAmt)
    g = mkStdGen seed
  void
    . flip runReaderT w
    . flip runRandT g
    $ do
      cairo $ scale (fromIntegral scaleAmt) (fromIntegral scaleAmt)
      imgDef





createParticles :: Generate [Particle]
createParticles = do
  fillScreen black 1
  cairo save
  cairo $ translate (fromIntegral wWidth / 2) (fromIntegral wHeight / 2)
  cairo $ do
    white 0.24
    setLineWidth 0.1

  initialParts 1500



move :: Particle -> Generate Particle
move Particle {..} = do
  rx <- getRandomR (-0.5, 0.5)
  ry <- getRandomR (-0.5, 0.5)

  let
    nx = x + vx
    ny = y + vy
    nvx = vx + rx
    nvy = vy + ry
    np = if age > 200
            then Particle (10*sin r) (10*cos r)  0 0 r 0
            else Particle nx ny nvx nvy r (age + 1)

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
  ages <- getRandomRs (1, 200)
  let ps = part <$> [1..n] `zip` ages
  pure ps
    where
      part (i, a) =
        let
          x = (10 * sin (twoPi * fromIntegral i / fromIntegral n))
          y = (10 * cos (twoPi * fromIntegral i / fromIntegral n))
          r = pi * fromIntegral i / fromIntegral n
        in mkPart x y r a


mkPart :: Double -> Double -> Double -> Int -> Particle
mkPart dx dy r a =
  Particle dx dy (0.2*cos(r)) (0.2*sin(r)) r a



white :: Double -> Render ()
white = hsva 0 0 1

black :: Double -> Render ()
black = hsva 0 0 0


twoPi = 2.0 * pi :: Double
