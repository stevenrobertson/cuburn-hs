module Render where

{- A basic implementation of the flame algorithm, optimized for ease of
 - understanding over performance. This implementation will be sliced in
 - various ways and reimplemented piecemeal by members of our team for learning
 - purposes. It may also serve as a testbed for new ideas and performance
 - techniques.
 -
 - If you're part of the SD team, don't read the rest of this file just yet!
 -}

import Control.Applicative
import Control.Monad.Trans.State
import Data.Traversable (Traversable)
import Data.Bits
import Data.Maybe
import System.Random
import Debug.Trace

import Data.SG
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV

import Flam3
import Variations

data Camera = Camera
    { camPPU            :: Double
    , camNSamplesPerCP  :: Int
    , camBufSz          :: Int
    , camStride         :: Int
    , camProj           :: Matrix33
    } deriving (Show)

-- Hint for using matrix transforms: they stack like function applications.
--   addCenter . rotate . subCenter $ point
--   addCenterMat .* rotateMat .* subCenterMat `mul` point
-- Also, matrixComponents returns a list of rows, and translation's on the end
-- of each row. This *prints* the same way as the OpenGL docs, but needs to be
-- transposed (I think?)
-- Also also, `multMatrixGen` doesn't do a perspective divide.

-- SG doesn't provide a matrix-to-matrix multiply.
-- typesig is overly specific, but unlikely to need otherwise
(.*) :: (Num a, Applicative c, Traversable c)
     => SquareMatrix c a -> SquareMatrix c a -> SquareMatrix c a
a .* b = fromMatrixComponents $
    [[sum [x*y | (x,y) <- zip xs ys] | ys <- matrixComponents (transpose b)]
     | xs <- matrixComponents a]

scale2D :: Num a => a -> a -> Matrix33' a
scale2D x y = fromMatrixComponents [[x, 0, 0], [0, y, 0], [0, 0, 1]]

goforit cp = accumulate cp (computeCamera cp)

randomS :: Random a => State StdGen a
randomS = do
    (v, gen) <- gets random
    put gen
    return v

randomRS bounds = do
    (v, gen) <- gets $ randomR bounds
    put gen
    return v

traces x = trace (show x) x

computeCamera :: Genome -> Camera
computeCamera cp =
    let (w, h) = (gnWidth cp, gnHeight cp)
        (w', h') = (fromIntegral w, fromIntegral h)
        negPoint (Point2 (x, y)) = Point2 (-x, -y)
        rctr = gnRotCenter cp
        ppu = gnPixelsPerUnit cp
        proj = traces $ scale2D (0.5 * ppu) (0.5 * ppu)
            .* translate2D (Point2 (1, 1))
            .* translate2D (negPoint $ gnCenter cp)
            .* translate2D rctr
            .* rotateZaxis (gnRotate cp * 2 / pi)
            .* translate2D (negPoint rctr)
        qual = round $ w' * h' * gnSampleDensity cp
             -- / (fromIntegral $ gnNTemporalSamples cp)
    in  Camera (gnPixelsPerUnit cp) qual (w*h) w proj

fuse = 100

accumulate :: Genome -> Camera -> IO (SV.Vector Double)
accumulate cp cam = do
    buf <- MSV.replicate (4 * camBufSz cam) 0
    mapM_ (storePt buf) . take (camNSamplesPerCP cam)
                        $ evalState (iterateIFS cp cam) (mkStdGen 1)
    SV.freeze buf
  where
    storePt :: MSV.IOVector Double -> (Int, RGBAColor) -> IO ()
    storePt buf (idx, RGBAColor r g b a) = do
        let idx' = 4*idx
        MSV.write buf  idx'    r
        MSV.write buf (idx'+1) g
        MSV.write buf (idx'+2) b
        MSV.write buf (idx'+3) a

iterateIFS :: Genome -> Camera -> State StdGen [(Int, RGBAColor)]
iterateIFS cp cam = drop fuse <$> (loop =<< newPoint)
  where
    loop p = do
        (rs, p') <- go 0 p
        (rs:) <$> loop p'
    newPoint =
        (,) <$> (fmap Point2 $ (,) <$> randomRS (-1, 1) <*> randomRS (-1, 1))
            <*> randomRS (0, 1)
    densitySum = sum . map xfDensity $ gnXForms cp
    go 5 _ = go (-3) =<< newPoint
    go consecBad (pt, color) = do
        xf <- chooseXForm cp densitySum
        (midx, pt', col') <- applyXForm xf cam pt color
        if consecBad < 0 || isNothing midx
            then go (consecBad + 1) (pt', col')
            else return ((fromJust midx, lookupColor xf col'), (pt', col'))
    lookupColor xf col =
        let PaletteList pes = gnPalette cp
            colIdx = truncate $ col * (fromIntegral $ V.length pes)
            PaletteEntry _ (RGBAColor r g b _) = pes V.! colIdx
            opa = xfOpacity xf
            vis = if opa == 0 then 0 else 10 ** (negate $ logBase 2 (1.0/opa))
        in  RGBAColor (r*vis) (g*vis) (b*vis) (vis)

applyXForm :: XForm -> Camera -> Point2 -> Double
           -> State StdGen (Maybe Int, Point2, Double)
applyXForm xf cam pt color = do
    let s = xfColorSpeed xf
        color' = color * (1-s) + (xfColorCoord xf) * s
        ptxf = multMatrixGen (xfProj xf) pt
        addPt (Point2 (x1, y1)) (Point2 (x2, y2)) = Point2 (x1+x2, y1+y2)
    pt' <- fmap (maybe id multMatrixGen (xfProjPost xf) . foldl1 addPt)
         . mapM (uncurry (applyVar ptxf)) $ xfVars xf
    let idx = let Point2 (x, y) = multMatrixGen (camProj cam) pt'
              in  round y * camStride cam + round x
        isValidIdx = idx >= 0 && idx < (camBufSz cam)
    return $ (if isValidIdx then Just idx else Nothing, pt', color')

chooseXForm :: Genome -> Double -> State StdGen XForm
chooseXForm cp dsum = go (gnXForms cp) `fmap` randomRS (0, dsum)
  where
    go (x:[]) _ = x
    go (x:xs) val = if val < xfDensity x then x else go xs (val - xfDensity x)

applyVar (Point2 (x, y)) wgt var =
    Point2 <$> case var of
        Linear       -> return (x*wgt, y*wgt)
        Spherical    -> return (x*r2, y*r2)
        Handkerchief -> return ( x * wgt * rad * sin (at + rad)
                               , y * wgt * rad * cos (at - rad) )
        Ex           ->
            let n0 = sin (at + rad)
                n1 = cos (at - rad)
                m0 = n0 * n0 * n0 * rad
                m1 = n1 * n1 * n1 * rad
            in  return ( wgt * (m0 + m1), wgt * (m0 - m1) )
        Julia        -> do
            rnd <- randomS
            let offset = if rnd .&. (0x1 :: Int) == 1 then pi else 0
                at' = 0.5 * at + offset
                r = wgt * sqrt rad
            return (r * cos at', r * sin at')
  where
    at = atan2 y x
    r2 = wgt / (x * x + y * y + 1.0e-6)
    rad = sqrt (x * x + y * y)

