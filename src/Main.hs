{-# LANGUAGE OverloadedLabels #-}

module Main where


import           BinaryRing
import           Control.Monad                     (when)
import           Control.Monad.Cont
import           Control.Monad.Random              (runRandT)
import           Control.Monad.Reader
import           Data.IORef
-- import           Data.Maybe                 (isJust)
import           Data.Semigroup                    ((<>))
import           Data.Time.Clock.POSIX
import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

import           World.Generate



import           Data.GI.Base
import qualified GI.Cairo
-- import qualified GI.Gdk                     as Gdk
-- import qualified GI.GdkPixbuf               as GP
import qualified GI.GLib                           as GLib
import qualified GI.Gtk                            as Gtk



renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))


drawCB seed cc ctx = do
  let
    w = BinaryRing.world seed
    g = BinaryRing.generator seed
  renderWithContext ctx $ do
    void
      . flip runReaderT w
      . flip runRandT g
      $ do
        cairo $ scale (fromIntegral scaleAmt) (fromIntegral scaleAmt)
        cc
  pure True


setTimeoutCallback :: Int -> Gtk.Window -> Generate [BinaryRing.Particle] -> IO ()
setTimeoutCallback s w c = do

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ do
    on w #draw (drawCB s c)
    #queueDraw w
    setTimeoutCallback s w (c >>= BinaryRing.moveAllAndFlip)
    pure True
  pure ()



main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  _ <- Gtk.init Nothing
  window <- new Gtk.Window []
  on window #destroy Gtk.mainQuit

  setTimeoutCallback seed window BinaryRing.createParticles



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
