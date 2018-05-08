{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


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
import           Prelude                  hiding (init)
import           World.Generate


type RandGen a = Rand StdGen a


data SandPainter = SandPainter
  { c :: ColorFn
  , g :: Double
  }

instance Show SandPainter where
  show (SandPainter _ g) = "SP {"<> show g <>"}"


data Crack = Crack
  { x  :: Double
  , y  :: Double
  , t  :: Integer
  , sp :: SandPainter
  } deriving (Show)
type ColorFn = Double -> Render ()
type CGrid = Array Integer Integer
type Model = (CGrid, [Crack])

wWidth = 300
wHeight = 300
scaleAmt = 1
maxCracks = 200


size = wWidth * wHeight - 1


initialModel ::RandGen (CGrid, [Crack])
initialModel = do
  grid <- cgrid
  cracks <- noNothings <$> traverse (\_ -> mkCrack grid) [1..3]
  pure (grid, cracks)

-- init :: Generate (CGrid, [Crack])
-- init = do
--   g <- cgrid
--   cs <- noNothings <$> traverse (\_ -> mkCrack g)  [1..3]
--   --fillScreen white 1
--   pure $ (g,cs)

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
  regionColor' g c
  let z = 0.33
  dx <- getRandomR (-z,z)
  dy <- getRandomR (-z,z)
  pure $ do
    rectangle (x + dx) (y + dy) 1 1
    black 0.80 *> fill


regionColor' :: CGrid -> Crack -> RandGen (Render [()])
regionColor' g c@(Crack x y t sp) = do
  let (rx,ry) = calc x y

  renderSand sp rx ry x y
    where
      calc rx ry =
        if openSpace g (round rx) (round ry)
          then calc (rx + 0.81 * sina (fromIntegral t)) (ry - 0.81 * cosa (fromIntegral t))
          else (rx, ry)


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



renderGrid :: CGrid -> Generate [()]
renderGrid g = do
  let
    l = assocs g
    fl = filter (\(i,v) -> v < 10000) l
    lll = (\(i, v) -> (i `mod` wWidth, i `div` wWidth) ) <$>  fl

  cairo $ traverse (\(x,y)-> do
      hsva 0 1 1 1
      rectangle (fromIntegral x - 1) (fromIntegral y - 1) 2 2
      fill ) lll


move :: Bool -> CGrid -> Crack -> RandGen (CGrid, [Crack])
move b g c@(Crack x y t sp) = do
  --_ <- renderGrid g
  let
    x' = x + 0.9 * cosa (fromIntegral t)
    y' = y + 0.9 * sina (fromIntegral t)
    newC = Crack x' y' t sp
    z = 0.5
  cx <- round . (+x') <$> getRandomR (-z,z)
  cy <- round . (+y') <$> getRandomR (-z,z)

  dx <- getRandomR (-z,z)
  dy <- getRandomR (-z,z)
  -- regionColor g newC
  -- cairo $ do
  --   rectangle (x' + dx) (y' + dy) 1 1
  --   black 0.85 *> fill
  let idx = cy * wWidth + cx
  if (cx >= 0 && cx < wWidth && cy >= 0 && cy < wHeight)
  then
    if (g ! idx > 10000) || (abs (t - g ! idx) < 5)
    then do
      let g' = g // [(idx, t)]
      pure (g', [newC])
    else
      if (abs (t - g ! idx) > 2)
      then startNewCrack b g newC
      else pure (trace "noop" g, [newC])
  else startNewCrack b g newC


startNewCrack b g c = do
  if b then do
    m <- mkCrack g
    case m of
      Nothing -> pure (g, [c])
      Just c' -> pure (g, [c', c])
  else pure (g, [c])

-- regionColor :: CGrid -> Crack -> Generate Crack
-- regionColor g c@(Crack x y t sp) = do
--   let (rx,ry) = calc x y
--   sp' <- renderSand sp rx ry x y
--   pure $ Crack x y t sp'
--     where
--       calc rx ry =
--         if openSpace g (round rx) (round ry)
--           then calc (rx + 0.81 * sina (fromIntegral t)) (ry - 0.81 * cosa (fromIntegral t))
--           else (rx, ry)


renderSand :: SandPainter -> Double -> Double -> Double -> Double -> RandGen (Render [()])
renderSand (SandPainter c g) x y ox oy = do
  g' <-  (g+) <$> getRandomR (-0.1, 0.1)
  let
    g'' = if g<0 then 0 else if g> 1 then 1 else g
    grains = 64
    w = g'' / (grains - 1)
  pure $ traverse (drawGr grains w) [0..grains-1]
    where
      drawGr grains w i = do
        let a = 0.1 - i / (grains * 10.0)
        c a
        rectangle (ox + (x-ox) * sin (sin (i * w) )) (oy + (y - oy )* sin ( sin (i*w))) 1 1
        fill





noNothings :: [Maybe a] -> [a]
noNothings []            = []
noNothings (Nothing:xs)  = noNothings xs
noNothings ((Just x):xs) = x : noNothings xs


emptyArrLst =
  (\i -> (i, 10001))
    <$> [0 .. size]



cgrid :: RandGen CGrid
cgrid = do
  ixs <- getRandomRs (0, toInteger size)
  vals <- getRandomRs (0, 360)
  pure $ array (0, size) emptyArrLst // (take 16 $ zip ixs vals)



openSpace :: CGrid -> Integer -> Integer -> Bool
openSpace g x y =
  if (x >= 0 && x < wWidth && y>=0 && y < wHeight) && (g ! (x + y * wWidth) > 10000)
    then True
    else False

someColor :: RandGen ColorFn
someColor = uniform palette

mkCrack :: CGrid -> RandGen (Maybe Crack)
mkCrack g = do
  xs <- getRandomRs (0, toInteger wWidth - 1)
  ys <- getRandomRs (0, toInteger wHeight - 1)
  let ixs = (filter isCrack) (xs `zip` ys)
  case ixs of
    [] -> pure Nothing
    (x,y):_ -> do
      p <- mkSandPainter
      r <- getRandomR (-2, 2)
      r' <- uniform [90 + r, 90 - r]
      let a = g ! (y*wWidth + x) + r'

      pure $ Just (Crack ( fromIntegral x + 0.61*cosa (fromIntegral a)) (fromIntegral y + 0.61 * sina (fromIntegral a)) a p)
  where
    isCrack (x,y) =
      g ! (y*wWidth + x) < 10000

mkSandPainter :: RandGen SandPainter
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
