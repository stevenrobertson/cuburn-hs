{-# LANGUAGE BangPatterns #-}

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
import Foreign.C.Types
import System.Random
import Debug.Trace

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV

import Flam3
import Matrix
import Variations

data Camera = Camera
    { camPPU            :: Double
    , camNSamplesPerCP  :: Int
    , camBufSz          :: Int
    , camStride         :: Int
    , camProj           :: Matrix3
    } deriving (Show)

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
        ppu = gnPixelsPerUnit cp
        proj = traces $ scaleMat (0.5 * ppu) (0.5 * ppu)
            .* translateMat (1, 1)
            .* translateMat (negPt $ gnCenter cp)
            .* rotateMat (gnRotCenter cp) (gnRotate cp * 2 / pi)
        qual = round $ w' * h' * gnSampleDensity cp
             -- / (fromIntegral $ gnNTemporalSamples cp)
    in  Camera (gnPixelsPerUnit cp) qual (w*h) w proj

fuse = 100

render :: Genome -> IO (SV.Vector (RGBAColor Float))
render cp = do
    putStrLn $ "k1: " ++ show k1 ++ ", k2: " ++ show k2
    accum <- accumulate cp cam
    return $ flip SV.map accum $ \col@(RGBAColor _ _ _ a) ->
        let ls = k1 * log (1.0 + 256 * a * k2) / a
        in  cvtType $ scaleColor col ls
  where
    cam = computeCamera cp
    k1 = gnContrast cp * gnBrightness cp * 268.0 * (255/256)
    area = (fromIntegral $ gnWidth cp * gnHeight cp) / (camPPU cam ** 2.0)
    k2 = 1 / (gnContrast cp * gnBrightness cp * area *
              (fromIntegral $ camNSamplesPerCP cam))

accumulate :: Genome -> Camera -> IO (SV.Vector (RGBAColor Double))
accumulate cp cam = do
    buf <- MSV.replicate (camBufSz cam) (RGBAColor 0 0 0 0)
    mapM_ (storePt buf) . take (camNSamplesPerCP cam)
                        $ evalState (iterateIFS cp cam) (mkStdGen 1)
    SV.freeze buf
  where
    storePt buf (idx, color) =
        MSV.write buf idx =<< fmap (addColor color) (MSV.read buf idx)

iterateIFS :: Genome -> Camera -> State StdGen [(Int, RGBAColor Double)]
iterateIFS cp cam = drop fuse <$> (loop =<< newPoint)
  where
    loop p = do
        (rs, p') <- go 0 p
        (rs:) <$> loop p'
    newPoint =
        (,) <$> ((,) <$> randomRS (-1, 1) <*> randomRS (-1, 1))
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
        ptxf = xfProj xf *. pt
    pt' <- fmap (maybe id (*.) (xfProjPost xf) . foldl1 addPt)
         . mapM (uncurry (applyVar ptxf)) $ xfVars xf
    let idx = let (x, y) = camProj cam *. pt'
              in  round y * camStride cam + round x
        isValidIdx = idx >= 0 && idx < (camBufSz cam)
    return $ (if isValidIdx then Just idx else Nothing, pt', color')

chooseXForm :: Genome -> Double -> State StdGen XForm
chooseXForm cp dsum = go (gnXForms cp) `fmap` randomRS (0, dsum)
  where
    go (x:[]) _ = x
    go (x:xs) val = if val < xfDensity x then x else go xs (val - xfDensity x)

applyVar (x, y) wgt var =
    case var of
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

