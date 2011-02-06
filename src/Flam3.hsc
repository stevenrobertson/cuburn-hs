{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
{-# CFILES flam3helpers.c #-}

module Flam3 where

#include <flam3.h>

import Foreign
import Foreign.C
import Control.Applicative
import Data.List (intercalate)

import qualified Data.Vector as V
import Data.ByteString (ByteString, useAsCString)
import Data.SG
import Variations

type Point2 = Point2' Double
type Matrix33 = Matrix33' Double

peeki :: Ptr CInt -> IO Int
peeki = fmap fromIntegral . peek
peekd :: Ptr CDouble -> IO Double
peekd = fmap realToFrac . peek

data Floating a => RGBColor  a = RGBColor  !a !a !a deriving (Eq, Ord, Show)
data Floating a => RGBAColor a = RGBAColor !a !a !a !a deriving (Eq, Ord, Show)

instance (Floating a, Storable a) => Storable (RGBColor a) where
    sizeOf _ = 3 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr = do
        [r,g,b] <- peekArray 3 (castPtr ptr)
        return $! RGBColor r g b
    poke ptr (RGBColor r g b) = pokeArray (castPtr ptr) [r, g, b]

instance (Floating a, Storable a) => Storable (RGBAColor a) where
    sizeOf _ = 4 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr = do
        [r,g,b,a] <- peekArray 4 (castPtr ptr)
        return $! RGBAColor r g b a
    poke ptr (RGBAColor r g b a) = pokeArray (castPtr ptr) [r, g, b, a]

scaleColor (RGBAColor r g b a) s = RGBAColor (r*s) (g*s) (b*s) (a*s)
addColor (RGBAColor r g b a) (RGBAColor x y z w) =
    RGBAColor (r+x) (g+y) (b+z) (a+w)
cvtType (RGBAColor r g b a) =
    RGBAColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- WARNING: manually transcribed from filters.h, which is not exported
data FilterType = Gaussian
                | Hermite
                | Box
                | Triangle
                | Bell
                | BSpline
                | Lanczos3
                | Lanczos2
                | Mitchell
                | Blackman
                | Catrom
                | Hamming
                | Hanning
                | Quadratic
                deriving (Eq, Ord, Show, Enum)

data PaletteMode = PaletteLinear | PaletteStep deriving (Eq, Ord, Show)
data InterpType = InterpLog deriving (Eq, Ord, Show)


data PaletteEntry = PaletteEntry Double (RGBAColor Double)
                    deriving (Eq, Ord, Show)
-- | This newtype wrapper is pretty much just here for "Show"
newtype PaletteList = PaletteList (V.Vector PaletteEntry) deriving (Eq, Ord)
instance Show PaletteList where show _ = "PaletteList \"*Omitted*\""

instance Storable PaletteEntry where
    sizeOf _ = #size flam3_palette_entry
    alignment _ = 8 -- ?
    peek ptr = PaletteEntry
        <$> (#peek flam3_palette_entry, index) ptr
        <*> (#peek flam3_palette_entry, color) ptr

peekAffineToMatrix :: Ptr Double -> IO Matrix33
peekAffineToMatrix ptr = do
    [xx, yx, xy, yy, xo, yo] <- peekArray 6 ptr
    return $ fromMatrixComponents [[xx, xy, xo], [yx, yy, yo], [0, 0, 1]]

data XForm = XForm
    -- skipped: var
    { xfProj            :: Matrix33
    , xfProjPost        :: Maybe Matrix33
    , xfDensity         :: Double
    , xfColorCoord      :: Double
    , xfColorSpeed      :: Double
    -- skipped: animate
    , xfOpacity         :: Double
    -- skipped: vis_adjusted, padding, wind
    , xfPreblur         :: Double
    -- skipped (well, moved): has_post
    -- skipped: a hell of a lot of parameters (to be added later)
    , xfVars            :: [(Double, Variation)]
    -- skipped: motion_freq, motion_func, motion, num_motion
    } deriving (Eq, Show)
instance Storable XForm where
    sizeOf _ = #size flam3_xform
    alignment _ = 16
    peek ptr = do
        let nVars = #const flam3_nvariations
        weights <- peekArray nVars $ (#ptr flam3_xform, var) ptr
        let activeWeights = filter ((/= 0) . fst) $ zip weights [0..]
        vars <- mapM (\(w,i) -> (,) w <$> getVar i) activeWeights

        hasPost <- toBool <$> peeki ((#ptr flam3_xform, has_post) ptr)
        post <- if not hasPost then return Nothing
            else Just <$> peekAffineToMatrix ((#ptr flam3_xform, post) ptr)

        XForm   <$> peekAffineToMatrix ((#ptr flam3_xform, c) ptr)
                <*> pure post
                <*> peekd ((#ptr flam3_xform, density) ptr)
                <*> peekd ((#ptr flam3_xform, color) ptr)
                <*> peekd ((#ptr flam3_xform, color_speed) ptr)
                <*> peekd ((#ptr flam3_xform, opacity) ptr)
                <*> peekd ((#ptr flam3_xform, has_preblur) ptr)
                <*> pure vars
      where
        getVar :: CInt -> IO Variation
        getVar (#const VAR_LINEAR) = return Linear
        getVar (#const VAR_SPHERICAL) = return Spherical
        getVar (#const VAR_HANDKERCHIEF) = return Handkerchief
        getVar (#const VAR_EX) = return Ex
        getVar (#const VAR_JULIA) = return Julia
        getVar i = error $ "Could not identify variation " ++ show i

data Genome = Genome
    -- skipped: name
    { gnTime            :: Double
    -- skipped: interpolation, interpolation_type, palette_interpolation
    , gnXForms          :: [XForm]
    , gnFinalXForm      :: Maybe XForm
    -- skipped: chaos, chaos_enable. TODO: re-enable.
    -- skipped: genome_index, parent_fname, symmetry
    , gnPalette         :: PaletteList
    -- skipped: input_image, palette_index
    , gnBrightness      :: Double
    , gnContrast        :: Double
    , gnGamma           :: Double
    , gnHighlightPower  :: Double
    , gnWidth           :: Int
    , gnHeight          :: Int
    , gnSpatialOversample :: Int
    , gnCenter          :: Point2
    , gnRotCenter       :: Point2
    , gnRotate          :: Double
    , gnVibrancy        :: Double
    -- skipped: hue_rotation
    , gnBackground      :: RGBColor Double
    , gnZoom            :: Double
    , gnPixelsPerUnit   :: Double
    , gnSpatialFiltRadius :: Double
    , gnSpatialFiltType :: FilterType
    , gnSampleDensity   :: Double
    , gnNBatches        :: Int
    , gnNTemporalSamples :: Int
    , gnEstimator       :: Double
    , gnEstimatorCurve  :: Double
    , gnEstimatorMin    :: Double
    -- skipped: xmlDocPtr edits;
    , gnGammaThreshold  :: Double
    -- skipped: palette_index0 to palette_blend
    , gnTemporalFiltType :: FilterType
    , gnTemporalFiltWidth :: Double
    , gnTemporalFiltExp :: Double
    , gnPaletteMode     :: PaletteMode
    } deriving (Eq, Show)

instance Storable Genome where
    sizeOf _ = #size flam3_genome
    alignment _ = 16 -- ?

    peek ptr = do
        nxforms <- peeki $ (#ptr flam3_genome, num_xforms) ptr
        useFinal <- fmap toBool
                  . peeki $ (#ptr flam3_genome, final_xform_enable) ptr
        finalIdx <- peeki $ (#ptr flam3_genome, final_xform_index) ptr
        xforms <- peekArray nxforms =<< (#peek flam3_genome, xform) ptr
        let xforms' = if useFinal
                then if finalIdx == nxforms - 1
                        then take (finalIdx - 1) xforms
                        else take (finalIdx - 1) xforms ++ drop finalIdx xforms
                else xforms
            final = if useFinal then Just (xforms !! finalIdx) else Nothing

        Genome  <$> peekd ((#ptr flam3_genome, time) ptr)
                <*> pure xforms' <*> pure final
                <*> (PaletteList . V.fromList <$>
                     peekArray 256 ((#ptr flam3_genome, palette) ptr))
                <*> peekd ((#ptr flam3_genome, brightness) ptr)
                <*> peekd ((#ptr flam3_genome, contrast) ptr)
                <*> peekd ((#ptr flam3_genome, gamma) ptr)
                <*> peekd ((#ptr flam3_genome, highlight_power) ptr)
                <*> peeki ((#ptr flam3_genome, width) ptr)
                <*> peeki ((#ptr flam3_genome, height) ptr)
                <*> peeki ((#ptr flam3_genome, spatial_oversample) ptr)
                <*> readPoint2 ((#ptr flam3_genome, center) ptr)
                <*> readPoint2 ((#ptr flam3_genome, rot_center) ptr)
                <*> peekd ((#ptr flam3_genome, rotate) ptr)
                <*> peekd ((#ptr flam3_genome, vibrancy) ptr)
                <*> (#peek flam3_genome, background) ptr
                <*> peekd ((#ptr flam3_genome, zoom) ptr)
                <*> peekd ((#ptr flam3_genome, pixels_per_unit) ptr)
                <*> peekd ((#ptr flam3_genome, spatial_filter_radius) ptr)
                <*> (toEnum <$> peeki ((#ptr flam3_genome, spatial_filter_select) ptr))
                <*> peekd ((#ptr flam3_genome, sample_density) ptr)
                <*> peeki ((#ptr flam3_genome, nbatches) ptr)
                <*> peeki ((#ptr flam3_genome, ntemporal_samples) ptr)
                <*> peekd ((#ptr flam3_genome, estimator) ptr)
                <*> peekd ((#ptr flam3_genome, estimator_curve) ptr)
                <*> peekd ((#ptr flam3_genome, estimator_minimum) ptr)
                <*> peekd ((#ptr flam3_genome, gam_lin_thresh) ptr)
                <*> (toEnum <$> peek ((#ptr flam3_genome, temporal_filter_type) ptr))
                <*> peek ((#ptr flam3_genome, temporal_filter_width) ptr)
                <*> peek ((#ptr flam3_genome, temporal_filter_exp) ptr)
                <*> (getPalMode <$> peek ((#ptr flam3_genome, palette_mode) ptr))
      where
        getPalMode :: CInt -> PaletteMode
        getPalMode (#const flam3_palette_mode_step) = PaletteStep
        getPalMode (#const flam3_palette_mode_linear) = PaletteLinear
        readPoint2 ptr' = do
            x:y:[] <- peekArray 2 ptr'
            return $ Point2 (x, y)

foreign import ccall unsafe "flam3.h flam3_parse_xml2"
    flam3Parse' :: CString -> CString -> CInt -> Ptr CInt -> IO (Ptr Genome)

foreign import ccall unsafe "flam3helpers.c &flam3_free_genomes"
    flam3FreeGenomes :: FunPtr (Ptr () -> Ptr Genome -> IO ())

flam3Parse :: ByteString -> IO (Maybe (ForeignPtr Genome, Int))
flam3Parse src = do
    (genp, ct) <- withCString "(unknown filename)" $ \fname ->
        useAsCString src $ \srcp ->
            with 0 $ \nump ->
                (,) <$> flam3Parse' srcp fname 1 nump <*> peek nump
    if genp == nullPtr then return Nothing else do
        let env = intPtrToPtr $ fromIntegral ct
        genfp <- newForeignPtrEnv flam3FreeGenomes env genp
        return $ Just (genfp, fromIntegral ct)

flam3Peek :: (ForeignPtr Genome, Int) -> IO [Genome]
flam3Peek (genfp, ct) = withForeignPtr genfp (peekArray ct)

