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

import Control.Arrow (first)
import Control.Applicative
import Data.Traversable (Traversable)
import Data.Bits
import Data.Maybe
import Foreign.C.Types
import System.Random
import Debug.Trace

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV hiding (replicate)
import qualified Data.Vector.Storable.Mutable as SV

import Flam3
import Matrix
import Variations

data Camera = Camera
    { camPPU            :: CDouble
    , camNSamplesPerCP  :: CInt
    , camBufSz          :: CInt
    , camStride         :: CInt
    , camProj           :: Matrix3
    } deriving (Show)

instance Random CDouble where
    randomR (lo, hi) = first d2CD . randomR (realToFrac lo, realToFrac hi)
    random = first d2CD . random

traces x = trace (show x) x

computeCamera :: Genome -> Camera
computeCamera cp =
    let (w, h) = (gnWidth cp, gnHeight cp)
        (w', h') = (fromIntegral w, fromIntegral h)
        ppu = gnPixelsPerUnit cp
        proj = traces $ translateMat (0.5 * w', 0.5 * h')
            .* scaleMat ppu ppu
            .* translateMat (negPt $ gnCenter cp)
            .* rotateMat (gnRotCenter cp) (gnRotate cp * 2 / pi)
        qual = round $ w' * h' * gnSampleDensity cp
             -- / (fromIntegral $ gnNTemporalSamples cp)
    in  Camera (gnPixelsPerUnit cp) qual (w*h) w proj

fuse = 100

render :: Genome -> IO (SV.Vector CFloat)
render cp = do
    putStrLn $ "k1: " ++ show k1 ++ ", k2: " ++ show k2
    accum <- accumulate cp cam
    return . flip SV.concatMap accum $ \col@(RGBAColor r g b a) ->
        let ls = k1 * log (1.0 + 256 * a * k2) / a
            ls' = gammaAdjust (ls*a) / a
        in  SV.create $ do
                v <- SV.new 4
                SV.write v 0 . realToFrac $ r * ls'
                SV.write v 1 . realToFrac $ g * ls'
                SV.write v 2 . realToFrac $ b * ls'
                SV.write v 3 . realToFrac $ a * ls'
                return v
  where
    cam = computeCamera cp
    k1 = gnContrast cp * gnBrightness cp * 268.0 * (255/256)
    area = (fromIntegral $ gnWidth cp * gnHeight cp) / (camPPU cam ** 2.0)
    k2 = 1 / (gnContrast cp * gnBrightness cp * area *
              (fromIntegral $ camNSamplesPerCP cam))
    gam = gnGamma cp
    lin = gnGammaThreshold cp
    gammaAdjust dnorm =
        if dnorm > 0
           then if dnorm < lin
                   then let frac = dnorm / lin
                        in frac * ((1 - frac) * (lin ** gam) + dnorm ** gam)
                   else dnorm ** gam
           else 0

accumulate :: Genome -> Camera -> IO (SV.Vector RGBAColor)
accumulate cp cam = do
    buf <- SV.replicate (ci2I $ camBufSz cam) (RGBAColor 0 0 0 0)
    let nsamps = ci2I (camNSamplesPerCP cam)
    iter buf nsamps ((-100, -100), 0.5)
    SV.freeze buf
  where
    addColor' s (RGBAColor r g b _) (RGBAColor x y z w) =
        RGBAColor (r*s+x) (g*s+y) (b*s+z) (s+w)
    storePt buf idx sca col =
        SV.write buf idx =<< fmap (addColor' sca col) (SV.read buf idx)
    iter buf 0 _ = return ()
    iter buf n p = do
        (idx, (sca, col), p') <- iterateIFS cp cam p
        storePt buf idx sca col
        iter buf (n-1) p'

newPoint = (,) <$> ((,) <$> randomRIO (-1, 1) <*> randomRIO (-1, 1))
               <*> randomRIO (0, 1)

iterateIFS :: Genome -> Camera -> (Point2, CDouble)
           -> IO (Int, (CDouble, RGBAColor), (Point2, CDouble))
iterateIFS cp cam startPt = go 0 startPt
  where
    go 5 _ = go (-3 :: Int) =<< newPoint
    go consecBad (pt, color) = do
        xf <- chooseXForm cp
        (midx, pt', col') <- applyXForm xf cam pt color
        if consecBad < 0 || isNothing midx
            then go (consecBad + 1) (pt', col')
            else return $!
                    ( ci2I $ fromJust midx
                    , lookupColor xf col'
                    , (pt', col') )
    lookupColor xf col =
        let PaletteList pes = gnPalette cp
            colIdx = truncate $ col * (fromIntegral $ V.length pes)
            PaletteEntry _ col' = pes V.! colIdx
            opa = xfOpacity xf
            vis = if opa == 0 then 0 else 10 ** (negate $ logBase 2 (1.0/opa))
        in  (vis, col')

applyXForm :: XForm -> Camera -> Point2 -> CDouble
           -> IO (Maybe CInt, Point2, CDouble)
applyXForm xf cam pt color = do
    let s = xfColorSpeed xf
        color' = color * (1-s) + (xfColorCoord xf) * s
        ptxf = xfProj xf *. pt
    pt' <- fmap (foldl1 addPt) . mapM (uncurry (applyVar ptxf)) $ xfVars xf
    let (x, y) = camProj cam *. (maybe id (*.) (xfProjPost xf) $ pt')
        x' = round x
        idx = round y * camStride cam + x'
        isValidIdx = idx >= 0 && idx < (camBufSz cam) &&
                     x' >= 0  && x' < (camStride cam)
    return $ (if isValidIdx then Just idx else Nothing, pt', color')

chooseXForm :: Genome -> IO XForm
chooseXForm cp = go (gnXForms cp) `fmap` randomRIO (0, gnXFormTotalDensity cp)
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
            rnd <- randomIO
            let offset = if rnd .&. (0x1 :: Int) == 1 then pi else 0
                at' = 0.5 * at + offset
                r = wgt * sqrt rad
            return (r * cos at', r * sin at')
  where
    at = atan2 x y
    r2 = wgt / (x * x + y * y + 1.0e-6)
    rad = sqrt (x * x + y * y)

