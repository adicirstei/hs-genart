{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module Substrate   {-- (    init, step, render   )  --}   where

import           Control.Monad            (when)
import           Control.Monad.Random     (getRandomR, getRandomRs, mkStdGen,
                                           runRandT)
import           Control.Monad.Reader     (runReaderT, void)
import           Data.Array
import           Data.Monoid              ((<>))
import           Debug.Trace
import           Graphics.Rendering.Cairo
import           Prelude                  hiding (init)
import           World.Generate



data SandPainter
data Crack

wWidth = 250
wHeight = 140
scaleAmt = 5

size = wWidth * wHeight - 1

emptyArrLst =
  (\i -> (i, 10001))
    <$> [0 .. size]



cgrid :: Generate (Array Int Int)
cgrid = do
  ixs <- getRandomRs (0, size)
  vals <- getRandomRs (0, 360)
  pure $ array (0, size) emptyArrLst // (take 16 $ zip ixs vals)








init :: Generate [Crack]
init = pure []

step :: [Crack] -> Generate [Crack]
step cs = do
  pure cs

render :: Int -> Generate [Crack] -> Render ()
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


palette = [hsva 129.1 0.9706 0.2667, hsva 71.4 1 0.5804, hsva 52.9 0.5570 0.8941, hsva 68.6 0.0571 0.9608, hsva 186.4 0.6878 0.8039]

white :: Double -> Render ()
white = hsva 0 0 1

black :: Double -> Render ()
black = hsva 0 0 0
