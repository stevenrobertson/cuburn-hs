module Main where

import Control.Concurrent
import Data.StateVar
import Text.Show.Pretty
import qualified Data.ByteString as B

import Flam3

import Graphics.UI.GLUT

main = do
    Just (genp, ct) <- flam3Parse =<<
        B.readFile "testdata/electricsheep.244.23669.flam3"
    putStrLn . ppShow =<< flam3Peek (genp, ct)


main' = do
    (progname, args) <- getArgsAndInitialize
    initialDisplayCapabilities $= [ With  DisplayRGB
                                  , With  DisplaySamples
                                  , With  DisplayDouble ]
    win <- createWindow "cuburn"
    forkIO $ threadDelay 10000000 >> leaveMainLoop
    mainLoop


