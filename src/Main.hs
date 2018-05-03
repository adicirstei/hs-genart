{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           BinaryRing
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
import qualified GI.Cairo
import qualified GI.GLib                           as GLib
import qualified GI.Gtk                            as Gtk


renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))


computeDrawing ref =
  forever $ do
    g <- readIORef ref
    writeIORef ref (g >>= BinaryRing.step)
    threadDelay 100000



drawCB seed ref ctx = do
  imgDef <- readIORef ref
  --writeIORef ref (imgDef >>= BinaryRing.step)
  renderWithContext ctx $ BinaryRing.render seed imgDef
  pure True

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  _ <- Gtk.init Nothing

  window <- new Gtk.Window [#defaultWidth := 1300, #defaultHeight := 700]

  ref <- newIORef BinaryRing.init
  taskId <- forkIO (computeDrawing ref)

  on window #destroy Gtk.mainQuit

  on window #draw (drawCB seed ref)

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 16 (yield >> #queueDraw window >> pure True)

  #showAll window

  Gtk.main

  -- seed <- round . (*1000) <$> getPOSIXTime
  --
  -- putStrLn "Generating art..."
  --
  -- sourface <- BinaryRing.generate seed
  -- surfaceWriteToPNG sourface
  --   $ "images/example/"
  --   <> show seed <> ".png"
  -- surfaceWriteToPNG sourface "images/example/latest.png"
