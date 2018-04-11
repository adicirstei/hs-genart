{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}


module GridArt  where


import           Data.List                (nub)
import qualified Numeric.Noise.Perlin     as P

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

init :: Int -> IO Surface
init seed = do
  let
    stdGen = mkStdGen seed
    width = 40
    height = 40
    scaleAmount = 30
    scaleWidth = round $ fromIntegral width * scaleAmount
    scaleHeight = round $ fromIntegral height * scaleAmount
  sourface <- createImageSurface FormatARGB32 scaleWidth scaleHeight
  let world = World width height seed scaleAmount
  render' sourface world stdGen $
    do
      cairo $ scale scaleAmount scaleAmount
      renderSketch
  return sourface




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

cairo :: Render a -> Generate a
cairo = lift . lift


getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

--  a---b
--  |   |
--  c---d
data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  , quadD :: V2 Double
  } deriving (Eq, Ord)


darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84


renderQuad :: Quad -> Render ()
renderQuad Quad{..} = renderClosedPath [quadA, quadB, quadC, quadD]


renderSketch :: Generate ()
renderSketch = do
  fillScreen eggshell 1

  cairo $ setLineWidth 0.15

  quads <- genQuadGrid
  noisyQuads <- traverse quadAddNoise quads

  for_ noisyQuads $ \quad -> do
    strokeOrFill <- weighted [(fill, 0.4), (stroke, 0.6)]
    color <- uniform
       [ teaGreen
       , vividTangerine
       , englishVermillion
       , darkGunmetal
       ]
    cairo $ do
      renderQuad quad
      color 1 *> strokeOrFill


genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Quad v' (v' ^+^ V2 0 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0)





quadAddNoise :: Quad -> Generate Quad
quadAddNoise Quad{..} = do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y)
      = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure $ Quad
    (addNoise quadA)
    (addNoise quadB)
    (addNoise quadC)
    (addNoise quadD)

