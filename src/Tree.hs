{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module Tree where


import           Control.Arrow            ((&&&))
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Colour.RGBSpace     (RGB (..))
import           Data.Colour.RGBSpace.HSV (hsv)
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

wWidth = 100
wHeight = 100
scaleAmt = 10


type Generate a = RandT StdGen (ReaderT World Render) a

type LinePath = [V2 Double]



cairo :: Render a -> Generate a
cairo = lift . lift

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

branch :: Int -> Generate ()
branch ttl = do
  cairo $ do
    moveTo 0 0
    lineTo 0 (-50)
    stroke

--{--
  if ttl > 0 then do
    r1 <- getRandomR (-0.1, 0.1)
    r2 <- getRandomR (0, 0.3)
    cairo $ do
      translate 0 (-45)
      scale (0.7 + r2) (0.7 + r2)
      save
      rotate (-0.4 + r1)
    branch (ttl - 1)
    cairo $ do
      restore
      rotate (0.4 + r1)
    branch (ttl - 1)
  else
    pure ()
--}


render :: Generate ()
render = do
  fillScreen eggshell 1
  cairo $ setLineWidth 4
  (w, h) <- getSize
  color <- uniform palette

  cairo $ do
    save
    translate (w/2) h
    scale 0.6 0.6
    color 1
  branch 5
  cairo $ restore



getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)


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



renderPath :: [V2 Double] -> Render ()
renderPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
renderPath [] = pure ()






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

palette =
  [ hsva 355 0.68 0.84
  , hsva 11 0.40 0.92
  , hsva 81 0.25 0.94
  , hsva 170 0.30 0.16
  ]

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

--genPath :: Generate LinePath
--genQuadGrid = do
--  (w, h) <- getSize @Int
--  vectors <- replicateM 800 $ do
--    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
--    pure $ v ^* 2
--  pure . nub . flip map vectors $ \v ->
--    let v' = fromIntegralVector v
--    in Quad v' (v' ^+^ V2 0 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0)


