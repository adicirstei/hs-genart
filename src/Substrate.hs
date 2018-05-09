module Substrate   {-- (    init, step, render   )  --}   where

import           Control.Monad            (foldM, when)
import           Control.Monad.Random     (Rand, StdGen, getRandomR,
                                           getRandomRs, lift, mkStdGen,
                                           runRandT, uniform)
import           Control.Monad.Reader     (runReaderT, void)
import           Data.Array
import           Data.Functor.Identity    (Identity)
import           Data.Maybe               (Maybe (..))
import           Data.Semigroup           ((<>))
import           Debug.Trace
import           Graphics.Rendering.Cairo

import           World.Generate


type RandGen a = Rand StdGen a


data SandPainter = SandPainter
  { c :: ColorFn
  , g :: Double
  }

-- instance Show SandPainter where
--   show (SandPainter _ g) = "SP {"<> show g <>"}"
--

data Crack = Crack
  { x  :: Double
  , y  :: Double
  , t  :: Integer
  , sp :: SandPainter
  } -- deriving (Show)
type ColorFn = Double -> Render ()
type CGrid = Array Integer Integer
type Model = (CGrid, [Crack])

wWidth = 700
wHeight = 700
scaleAmt = 1
maxCracks = 120


size = wWidth * wHeight - 1


initialModel ::RandGen (CGrid, [Crack])
initialModel = do
  grid <- cgrid
  cracks <- noNothings <$> traverse (\_ -> mkCrack grid) [1..3]
  pure (grid, cracks)

step :: (CGrid, [Crack]) -> RandGen (CGrid, [Crack])
step (g,cs) = do
  (g',z) <- foldM folder (g,[]) cs

  pure (g', z)
    where
      folder (g, acs) c = do
        let cCount = length cs
        (g', cs') <- move (cCount < maxCracks) g c
        pure (g', acs <> cs')



renderModel :: Model -> RandGen (Render [()])
renderModel (grid, cracks) = do
  rs <- traverse (renderCrack grid) cracks
  pure $ sequence rs



renderCrack :: CGrid -> Crack -> RandGen (Render ())
renderCrack g c@(Crack x y t sp) = do
  sand <- regionColor g c
  let z = 0.71
  dx <- getRandomR (-z,z)
  dy <- getRandomR (-z,z)
  pure $ do
    sand
    rectangle (x + dx) (y + dy) 1 1
    black 0.40 *> fill



regionColor :: CGrid -> Crack -> RandGen (Render [()])
regionColor g c@(Crack x y t sp) = do
  let (rx,ry) = calc (x + 0.81 * sina (fromIntegral t)) (y - 0.81 * cosa (fromIntegral t))

  renderSand sp rx ry x y
    where
      calc rx ry =
        if openSpace g (round rx) (round ry)
          then calc (rx + 0.81 * sina (fromIntegral t)) (ry - 0.81 * cosa (fromIntegral t))
          else (rx, ry)


move :: Bool -> CGrid -> Crack -> RandGen (CGrid, [Crack])
move b g c@(Crack x y t sp@(SandPainter col spg)) = do
  newg <- getRandomR (-0.05, 0.05)
  let 
    newg' 
      | newg + spg > 1 = 1
      | newg + spg < 0 = 0
      | otherwise = newg + spg
  let
    x' = x + 0.33 * cosa (fromIntegral t)
    y' = y + 0.33 * sina (fromIntegral t)
    newSp = SandPainter col newg'
    newC = Crack x' y' t newSp
    z = 0.33
  cx <- round . (+x') <$> getRandomR (-z,z)
  cy <- round . (+y') <$> getRandomR (-z,z)


  let idx = cy * wWidth + cx
  if cx >= 0 && cx < wWidth && cy >= 0 && cy < wHeight
  then
    if (g ! idx > 10000) || (abs (t - g ! idx) < 5)
    then do
      let g' = g // [(idx, t)]
      pure (g', [newC])
    else

      if abs (t - g ! idx) > 2
      then startNewCrack b g newC
      else pure (trace "noop" g, [newC])
  else startNewCrack b g newC


startNewCrack b g c = do
  old <- mkCrack g
  let
    cs = case old of
          Nothing -> []
          Just cc -> [cc]
  if b then do
    m <- mkCrack g
    case m of
      Nothing -> pure (g, cs)
      Just c' -> pure (g, c' : cs)
  else pure (g, cs)


renderSand :: SandPainter -> Double -> Double -> Double -> Double -> RandGen (Render [()])
renderSand (SandPainter c g) x y ox oy = do
  g' <-  (g+) <$> getRandomR (-0.05, 0.05)
  let
    g''
      | g<0 = 0
      | g > 1 = 1
      | otherwise = g
    grains = 64
    w = g'' / (grains - 1)
  pure $ traverse (drawGr grains w) [0..grains-1]
    where
      drawGr grains w i = do
        let a = 0.1001 - i / (grains * 10.0)
        rectangle (ox + (x-ox) * sinsin (i * w) ) (oy + (y - oy )* sinsin (i*w)) 1 1
        c a *> fill

noNothings :: [Maybe a] -> [a]
noNothings []           = []
noNothings (Nothing:xs) = noNothings xs
noNothings (Just x:xs)  = x : noNothings xs


emptyArrLst =
  (\i -> (i, 10001))
    <$> [0 .. size]



cgrid :: RandGen CGrid
cgrid = do
  ixs <- getRandomRs (0, toInteger size)
  vals <- getRandomRs (0, 360)
  pure $ array (0, size) emptyArrLst // take 20 (zip ixs vals)



openSpace :: CGrid -> Integer -> Integer -> Bool
openSpace g x y =
  (x >= 0 && x < wWidth && y>=0 && y < wHeight) && (g ! (x + y * wWidth) > 10000)


someColor :: RandGen ColorFn
someColor = uniform palette

mkCrack :: CGrid -> RandGen (Maybe Crack)
mkCrack g = do
  xs <- getRandomRs (0, toInteger wWidth - 1)
  ys <- getRandomRs (0, toInteger wHeight - 1)
  let ixs = filter isCrack (xs `zip` ys)
  case ixs of
    [] -> pure Nothing
    (x,y):_ -> do
      p <- mkSandPainter
      r <- getRandomR (-2, 2)
      r' <- uniform [90 + r, 90 - r]
      let a = r' + g ! (y*wWidth + x)

      pure $ Just (Crack ( fromIntegral x + 0.61*cosa (fromIntegral a)) (fromIntegral y + 0.61 * sina (fromIntegral a)) (a `mod` 360) p)
  where
    isCrack (x,y) =
      g ! (y*wWidth + x) < 10000

mkSandPainter :: RandGen SandPainter
mkSandPainter = do
  c <- someColor
  g <- getRandomR (0.05, 0.3)
  pure $ SandPainter c g

palette = [hsva 2.8 0.76 0.33, hsva 200 0.9 0.5, hsva 129.1 1 0.2667, hsva 71.4 1 0.5804, hsva 52.9 1 0.8941, hsva 68.6 0.7 0.9608, hsva 186.4 0.6878 0.8039]

white :: Double -> Render ()
white = hsva 0 0 1

black :: Double -> Render ()
black = hsva 0 0 0

twoPi = 2.0 * pi :: Double

cosa = cos . (*(pi/180))
sina = sin . (*(pi/180))


sinsin = sin . sin