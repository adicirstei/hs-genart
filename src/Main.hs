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


drawCB modelRef imageRef ctx = do
  (m,g) <- readIORef modelRef
  srf <- readIORef imageRef
  renderWithContext ctx $ do
    save
    setOperator OperatorSource
    setSourceRGB 1 1 1
    paint
    restore
    setSourceSurface srf 0 0
    paint
    let (r,g') = runRand (D.renderModel m) g
    renderWith srf r

    pure ()
  writeIORef imageRef srf
  pure  True



main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime

  let
    gen = mkStdGen seed
    initialModel = runRand D.initialModel gen
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

                atomicWriteIORef modelRef (runRand nextVal g)

  _ <- Gtk.init Nothing

  window <- new Gtk.Window [#defaultWidth := 700, #defaultHeight := 700]

  --taskId <- forkIO (computeDrawing ref)

  on window #destroy Gtk.mainQuit

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
  srf <- readIORef imgRef
  surfaceWriteToPNG srf
   $ "images/example/" <> name <> "-"
   <> show seed <> ".png"
