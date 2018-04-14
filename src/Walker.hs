{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module Walker where


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

wWidth = 120
wHeight = 20
scaleAmt = 10


type Generate a = RandT StdGen (ReaderT World Render) a

type LinePath = [V2 Double]



cairo :: Render a -> Generate a
cairo = lift . lift

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

render :: Generate ()
render = do
  fillScreen eggshell 1
  cairo $ setLineWidth 0.4
  (w, h) <- getSize
  color <- uniform palette
  cairo $ moveTo 0 (h/2)
  for_ [1..w] $ \x -> do
    y <- getRandomR (h/3, 2*h/3)
    cairo $ lineTo x y
  cairo $ color 1 *> stroke


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






walker :: Int -> IO Surface
walker seed = do
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


