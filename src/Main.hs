module Main where

import Control.Applicative
import Control.Concurrent
import System.Random

import qualified Data.ByteString as B
import qualified Data.Vector.Storable as SV
import Graphics.UI.GLUT
import Text.Show.Pretty (ppShow)

import Flam3
import Render

main = do
    Just (genp, ct) <- flam3Parse =<<
        B.readFile --"testdata/electricsheep.244.23669.flam3"
                    "testdata/sierpinski.flame"
    [genome] <- flam3Peek (genp, ct)

    (progname, args) <- getArgsAndInitialize
    initialDisplayCapabilities $= [ With  DisplayRGB
                                  , Where DisplayDepth IsAtLeast 16
                                  , With  DisplaySamples
                                  , Where DisplayStencil IsNotLessThan 2
                                  , With  DisplayDouble ]
    win <- createWindow "cuburn"
    Size scrW scrH <- get screenSize
    windowSize $= Size 800 600
    windowPosition $= Position (fromIntegral $ scrW `div` 2 - 400)
                               (fromIntegral $ scrH `div` 2 - 300)

    clearColor $= Color4 0.25 0.25 0.25 1
    shadeModel $= Flat
    depthFunc $= Just Lequal

    [texName] <- genObjectNames 1
    textureBinding Texture2D $= Just texName
    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')

    pdataVec <- render genome

    SV.unsafeWith pdataVec $ \ptr -> do
        let pdata = PixelData RGBA Float ptr
            (w, h) = (fromIntegral $ gnWidth genome,
                      fromIntegral $ gnHeight genome)
        -- Linear texture filtering requires mipmaps, and will silently
        -- fail if mipmaps aren't constructed.
        -- texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D 64 64) 0 pdata
        build2DMipmaps Texture2D RGBA' w h pdata

    textureBinding Texture2D $= Nothing

    reshapeCallback $= Just reshapeCB
    displayCallback $= redisplayCB texName
    keyboardMouseCallback $= Just keyMouseCB
    actionOnWindowClose $= MainLoopReturns

    mainLoop

reshapeCB :: Size -> IO ()
reshapeCB size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 1 30
    -- frustum (-1.0) 1 (-1.0) 1 1.5 20
    matrixMode $= Modelview 0

redisplayCB :: TextureObject -> IO ()
redisplayCB tex = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 0 0 (-3.6 :: GLfloat)

    texture Texture2D $= Enabled
    textureFunction $= Replace
    textureBinding Texture2D $= Just tex

    renderPrimitive Quads $ do
        texCo2f 0 1 >> vertex3f (-2) (-1) 0
        texCo2f 0 0 >> vertex3f (-2)   1  0
        texCo2f 1 0 >> vertex3f   0    1  0
        texCo2f 1 1 >> vertex3f   0  (-1) 0

        texCo2f 0 0 >> vertex3f 1 (-1) 0
        texCo2f 0 1 >> vertex3f 1   1  0
        texCo2f 1 1 >> vertex3f 2.41421   1  (-1.41421)
        texCo2f 1 0 >> vertex3f 2.41421 (-1) (-1.41421)
    textureBinding Texture2D $= Nothing
    texture Texture2D $= Disabled

    swapBuffers

  where
    texCo2f u v = texCoord (TexCoord2 u (v :: GLfloat))
    vertex3f x y z = vertex (Vertex3 x y (z :: GLfloat))

keyMouseCB :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyMouseCB (Char 'q') Down _ _ = leaveMainLoop
keyMouseCB _ _ _ _ = return ()

