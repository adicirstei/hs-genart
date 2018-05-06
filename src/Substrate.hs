{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module Substrate   {-- (    init, step, render   )  --}   where

import           Control.Monad            (foldM, when)
import           Control.Monad.Random     (getRandomR, getRandomRs, mkStdGen,
                                           runRandT, uniform)
import           Control.Monad.Reader     (runReaderT, void)
import           Data.Array
import           Data.Maybe               (Maybe (..))
import           Data.Semigroup           ((<>))
import           Debug.Trace
import           Graphics.Rendering.Cairo
import           Prelude                  hiding (init)
import           World.Generate

data SandPainter = SandPainter
  { c :: ColorFn
  , g :: Double
  }

instance Show SandPainter where
  show (SandPainter _ g) = "SP {"<> show g <>"}"


data Crack = Crack
  { x  :: Double
  , y  :: Double
  , t  :: Double
  , sp :: SandPainter
  } deriving (Show)
type ColorFn = Double -> Render ()
type CGrid = Array Integer Double


wWidth = 250
wHeight = 140
scaleAmt = 5

size = wWidth * wHeight - 1

init :: Generate (CGrid, [Crack])
init = do
  g <- cgrid
  cs <- noNothings <$> traverse (\_ -> mkCrack g)  [1..3]
  fillScreen white 1
  pure $ (g,cs)

step :: (CGrid, [Crack]) -> Generate (CGrid, [Crack])
step (g,cs) = do
  (g',z) <- foldM folder (g,[]) cs

  pure $ (g', trace ("Z " <> show (length z))  z)
    where
      folder (g, cs) c = do
        (g', cs') <- move g c
        pure (g', cs <> cs')

render :: Int -> Generate (CGrid, [Crack]) -> Render ()
render seed imgDef = do
  let
    w = World (fromIntegral wWidth) (fromIntegral wHeight) seed (fromIntegral scaleAmt)
    g = mkStdGen seed
  void
    . flip runReaderT w
    . flip runRandT g
    $ do
      cairo $ scale (fromIntegral scaleAmt) (fromIntegral scaleAmt)
      imgDef


move :: CGrid -> Crack -> Generate (CGrid, [Crack])
move g c@(Crack x y t sp) = do
  let
    x' = x + 0.42 * cosa t
    y' = y + 0.42 * sina t
  cx <- round . (+x) <$> getRandomR (-0.33, 0.33)
  cy <- round . (+y) <$> getRandomR (-0.33, 0.33)
  dx <- getRandomR (-0.33, 0.33)
  dy <- getRandomR (-0.33, 0.33)
  regionColor g c
  cairo $ do
    rectangle (x'+dx) (y'+dy) 0.2 0.2
    black 0.85 *> fill
  let idx = cy * wWidth + cx
  if (cx >= 0 && cx < wWidth && cy >= 0 && cy < wHeight) && (g ! idx > 10000 || abs (t - g ! idx) < 5)
    then do
      let g' = g // [(idx,t)]
      pure (g', [c])
    else do
      mc <- mkCrack g
      case mc of
        Nothing -> pure (g, [c])
        Just c' -> pure (g, [c, c'])


regionColor :: CGrid -> Crack -> Generate Crack
regionColor g c@(Crack x y t sp) = do
  let (rx,ry) = calc x y
  sp' <- renderSand sp rx ry x y
  pure $ Crack x y t sp'
    where
      calc rx ry =
        if openSpace g (round rx) (round ry)
          then calc (rx + 0.81 * sina t) (ry - 0.81 * cosa t)
          else (rx, ry)


renderSand :: SandPainter -> Double -> Double -> Double -> Double -> Generate SandPainter
renderSand (SandPainter c g) x y ox oy = do
  g' <-  (g+) <$> getRandomR (-0.05, 0.05)
  let
    g'' = if g<0 then 0 else if g> 1 then 1 else g
    grains = 64
    w = g'' / (grains - 1)
  cairo $ traverse (drawGr grains w) [0..grains-1]
  pure $ SandPainter c g''
    where
      drawGr grains w i = do
        let a = 0.1 - i / (grains * 10.0)
        c a
        rectangle (ox + (x-ox) * sin (sin (i * w) )) (oy + (y - oy )* sin ( sin (i*w))) 0.2 0.2
        fill





noNothings :: [Maybe a] -> [a]
noNothings []            = []
noNothings (Nothing:xs)  = noNothings xs
noNothings ((Just x):xs) = x : noNothings xs


emptyArrLst =
  (\i -> (i, 10001))
    <$> [0 .. size]



cgrid :: Generate CGrid
cgrid = do
  ixs <- getRandomRs (0, toInteger size)
  vals <- getRandomRs (0.0, 360.0)
  pure $ array (0, size) emptyArrLst // (take 16 $ zip ixs vals)



openSpace :: CGrid -> Integer -> Integer -> Bool
openSpace g x y =
  if (x >= 0 && x < wWidth && y>=0 && y < wHeight) && (g ! (x + y * wWidth) > 10000)
    then True
    else False

someColor :: Generate ColorFn
someColor = uniform palette

mkCrack :: CGrid -> Generate (Maybe Crack)
mkCrack g = do
  xs <- getRandomRs (0, toInteger wWidth - 1)
  ys <- getRandomRs (0, toInteger wHeight - 1)
  let ixs = (filter isCrack . take 1000) (xs `zip` ys)
  case ixs of
    [] -> pure Nothing
    (x,y):_ -> do
      p <- mkSandPainter
      r <- getRandomR (-2, 2.1)
      r' <- uniform [90 + r, 90 - r]
      let a = g ! (y*wWidth + x) + r'

      pure $ Just (Crack ( fromIntegral x + 0.61*cosa a) (fromIntegral y + 0.61 * sina a) a p)
  where
    isCrack (x,y) =
      g ! (y*wWidth + x) < 10000

mkSandPainter :: Generate SandPainter
mkSandPainter = do
  c <- someColor
  g <- getRandomR (0.01, 0.1)
  pure $ SandPainter c g

palette = [hsva 129.1 0.9706 0.2667, hsva 71.4 1 0.5804, hsva 52.9 0.5570 0.8941, hsva 68.6 0.0571 0.9608, hsva 186.4 0.6878 0.8039]

white :: Double -> Render ()
white = hsva 0 0 1

black :: Double -> Render ()
black = hsva 0 0 0

twoPi = 2.0 * pi :: Double

cosa = cos . (*(pi/180))
sina = sin . (*(pi/180))
