{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module BinaryRing  where

import           Control.Monad            (when)
import           Control.Monad.Random     (getRandomR, getRandomRs, mkStdGen,
                                           runRandT, weighted)
import           Control.Monad.Reader     (runReaderT, void)
import           Data.Monoid              ((<>))
import           Debug.Trace
import           Graphics.Rendering.Cairo
import           World.Generate


data Particle = Particle
  { x   :: Double
  , y   :: Double
  , vx  :: Double
  , vy  :: Double
  , r   :: Double
  , age :: Int
  }

data ColorChoice = W | B

initialModel ::RandGen (ColorChoice,  [Particle])
initialModel = do
  p <- initialParts 1500
  pure (W, p)


step :: (ColorChoice, [Particle]) -> RandGen (ColorChoice, [Particle])
step (c, ps) = do
  p' <- traverse move ps
  change <- changeColor
  pure (flipC c change, p')

renderSetup = do
  (w,h) <- getSize
  pure $ do
    rectangle 0 0 (fromIntegral w ) (fromIntegral h )
    black 1 *> fill

renderModel :: (ColorChoice, [Particle]) -> RandGen (Render [()])
renderModel (c, ps) = do
  (w,h) <- getSize
  pure $ do
    setLineWidth 1
    --white 0.24
    translate (fromIntegral w / 2) (fromIntegral h / 2)
    let color =
          case c of
              W -> white
              B -> black

    traverse (renderPart color) ps

    where
      renderPart color (Particle x y vx vy _ _) = do
        moveTo x y
        lineTo (x + vx) (y + vy)
        color 0.24 *> stroke

        moveTo (-x) y
        lineTo (-x - vx) (y + vy)
        color 0.24 *> stroke

flipC :: ColorChoice -> Bool -> ColorChoice
flipC W True  = B
flipC B True  = W
flipC B False = B
flipC W False = W

move :: Particle -> RandGen Particle
move Particle {..} = do
  rx <- getRandomR (-0.5, 0.5)
  ry <- getRandomR (-0.5, 0.5)

  let
    nx = x + vx
    ny = y + vy
    nvx = vx + rx
    nvy = vy + ry
    np = if age > 200
            then Particle (30*sin r) (30*cos r)  0 0 r 0
            else Particle nx ny nvx nvy r (age + 1)

  pure np


changeColor :: RandGen Bool
changeColor = do
  r <- weighted [(False, 0.99), (True, 0.01)]
  pure r



initialParts :: Int -> RandGen [Particle]
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
