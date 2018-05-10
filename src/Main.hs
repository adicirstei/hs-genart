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


main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime

  let
    world = World 700 700 seed 1
    runWithWorld = D.run world
    gen = mkStdGen seed
    initialModel = runWithWorld gen D.initialModel 
  sourface <- createImageSurface FormatARGB32 700 700
  renderWith sourface $ do
    rectangle 0 0 700 700
    D.white 1 *> fill

  modelRef <- newIORef initialModel
  imageRef <- newIORef sourface
  let step = do
                i <- readIORef imageRef
                (m,g) <- readIORef modelRef
                let nextVal = D.step m
                
                atomicWriteIORef modelRef (runWithWorld g nextVal)


  let drawCB modelRef imageRef ctx = do
        (m,g) <- readIORef modelRef
        srf <- readIORef imageRef
        renderWithContext ctx $ do
          setSourceSurface srf 0 0
          paint
          let (r,g') = runWithWorld g (D.renderModel m) 
          renderWith srf r
      
          pure ()
        atomicWriteIORef imageRef srf
        pure  True


  _ <- Gtk.init Nothing

  window <- new Gtk.Window [#defaultWidth := 700, #defaultHeight := 700]

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

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 20 (#queueDraw window >> step >> yield >> pure True)

  #showAll window

  Gtk.main


saveImage imgRef seed name = do
  r <- evalRandIO $ getRandomR(toInteger  10000, toInteger 99999)
  srf <- readIORef imgRef
  surfaceWriteToPNG srf
   $ "images/example/" <> name <> "-"
   <> show seed <> "-" <> show r <> ".png"
