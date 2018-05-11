{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import qualified Substrate                         as D
--import           Control.Monad.Random              (runRandT)
import           Control.Concurrent                (forkIO, threadDelay, yield)
import           Control.Monad                     (forever)
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.IORef
import           Data.Semigroup                    ((<>))
import           Data.Time.Clock.POSIX
import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

import           World.Generate


import           Data.GI.Base
import           Debug.Trace
import qualified GI.Cairo
import qualified GI.GLib                           as GLib
import qualified GI.Gtk                            as Gtk

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))


wWidth :: Integer
wWidth = 800

wHeight :: Integer
wHeight = 450

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime

  let
    world = World wWidth wHeight (fromIntegral seed) 1
    runWithWorld = run world
    gen = mkStdGen seed
    initialModel = runWithWorld gen D.initialModel
  sourface <- createImageSurface FormatARGB32 (fromIntegral wWidth) (fromIntegral wHeight)
  let (r,_) = runWithWorld gen D.renderSetup
  renderWith sourface r

  modelRef <- newIORef initialModel
  imageRef <- newIORef sourface
  let step = do
                i <- readIORef imageRef
                (m,g) <- readIORef modelRef
                srf <- readIORef imageRef

                let (m', g') = runWithWorld g $ D.step m
                let (r, g'') = runWithWorld g' $ D.renderModel m'
                renderWith srf r

                atomicWriteIORef imageRef srf
                atomicWriteIORef modelRef (m',g')


  let drawCB modelRef imageRef ctx = do

        srf <- readIORef imageRef
        renderWithContext ctx $ do
          --scale 0.5 0.5
          setSourceSurface srf 0 0
          paint
          pure ()

        pure  True


  _ <- Gtk.init Nothing

  window <- new Gtk.Window [#defaultWidth := fromIntegral wWidth , #defaultHeight := fromIntegral wHeight ]

  on window #destroy $ saveImage imageRef seed "Substrate" >> Gtk.mainQuit

  on window #draw (drawCB modelRef imageRef)
  on window #keyPressEvent $ \_ -> do
    saveImage imageRef seed "Substrate"
    return False

  -- on window #buttonPressEvent $ \_ -> do
  --   seed' <- round . (*1000) <$> getPOSIXTime
  --   atomicWriteIORef ref D.init
  --   on window #draw (drawCB seed' ref)
  --   return False
  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 (step >> yield >> #queueDraw window >> yield >>  pure True)


  #showAll window

  Gtk.main


saveImage imgRef seed name = do
  r <- evalRandIO $ getRandomR(toInteger  10000, toInteger 99999)
  srf <- readIORef imgRef
  surfaceWriteToPNG srf
   $ "images/example/" <> name <> "-"
   <> show seed <> "-" <> show r <> ".png"
