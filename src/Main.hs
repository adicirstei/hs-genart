{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import qualified Substrate                         as D
--import           Control.Monad.Random              (runRandT)
import           Control.Concurrent                (forkIO, threadDelay, yield)
import           Control.Monad                     (forever)
import           Control.Monad.Reader
import           Data.IORef
--import           Data.Semigroup                    ((<>))
import           Data.Time.Clock.POSIX
import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

--import           World.Generate


import           Data.GI.Base
import           Debug.Trace
import qualified GI.Cairo
import qualified GI.GLib                           as GLib
import qualified GI.Gtk                            as Gtk

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))


drawCB seed ref ctx = do
  imgDef <- readIORef ref
  renderWithContext ctx $ D.render seed imgDef
  pure  True

main :: IO ()
main = do

  ref <- newIORef D.init
  seed <- round . (*1000) <$> getPOSIXTime
  _ <- Gtk.init Nothing

  window <- new Gtk.Window [#defaultWidth := 700, #defaultHeight := 700]

  --taskId <- forkIO (computeDrawing ref)

  on window #destroy Gtk.mainQuit

  on window #draw (drawCB seed ref)

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 (#queueDraw window >> step ref >> yield >> pure True)

  #showAll window

  Gtk.main



step ref = do
  img <- readIORef ref
  atomicWriteIORef ref (img >>= D.step)
