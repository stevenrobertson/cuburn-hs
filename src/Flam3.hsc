{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
{-# CFILES flam3helpers.c #-}

module Flam3 where

#include <flam3.h>

import Foreign
import Foreign.C
import Control.Applicative

import qualified Data.Vector as V
import Data.ByteString (ByteString, useAsCString)

import Variations
import Matrix

iToBool :: CInt -> Bool
iToBool = toBool

-- | Cast a Double to a CDouble explicitly
d2CD :: Double -> CDouble
d2CD = realToFrac

-- | Cast a CInt to an Int explicitly
ci2I :: CInt -> Int
ci2I = fromIntegral

data RGBColor = RGBColor   {-# UNPACK #-} !CDouble
                           {-# UNPACK #-} !CDouble
                           {-# UNPACK #-} !CDouble
                 deriving (Eq, Ord, Show)
data RGBAColor = RGBAColor {-# UNPACK #-} !CDouble
                           {-# UNPACK #-} !CDouble
                           {-# UNPACK #-} !CDouble
                           {-# UNPACK #-} !CDouble
                 deriving (Eq, Ord, Show)

instance Storable RGBColor where
    sizeOf _ = sizeOf (undefined :: CDouble) * 3
    alignment _ = alignment (undefined :: CDouble)
    peek p = do
        let q = castPtr p
        r <- peekElemOff q 0
        g <- peekElemOff q 1
        b <- peekElemOff q 2
        return $ RGBColor r g b
    poke p (RGBColor r g b) = do
        let q = castPtr p
        pokeElemOff q 0 r
        pokeElemOff q 1 g
        pokeElemOff q 2 b


instance Storable RGBAColor where
    sizeOf _ = sizeOf (undefined :: CDouble) * 4
    alignment _ = alignment (undefined :: CDouble)
    peek p = do
        let q = castPtr p
        r <- peekElemOff q 0
        g <- peekElemOff q 1
        b <- peekElemOff q 2
        a <- peekElemOff q 3
        return $ RGBAColor r g b a
    poke p (RGBAColor r g b a) = do
        let q = castPtr p
        pokeElemOff q 0 r
        pokeElemOff q 1 g
        pokeElemOff q 2 b
        pokeElemOff q 3 a

scaleColor (RGBAColor r g b a) s = RGBAColor (r*s) (g*s) (b*s) (a*s)
addColor (RGBAColor r g b a) (RGBAColor x y z w) =
    RGBAColor (r+x) (g+y) (b+z) (a+w)

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


data PaletteEntry = PaletteEntry CDouble RGBAColor deriving (Eq, Ord, Show)
-- | This newtype wrapper is pretty much just here for "Show"
newtype PaletteList = PaletteList (V.Vector PaletteEntry) deriving (Eq, Ord)
instance Show PaletteList where show _ = "PaletteList \"*Omitted*\""

instance Storable PaletteEntry where
    sizeOf _ = #size flam3_palette_entry
    alignment _ = 8 -- ?
    peek ptr = PaletteEntry
        <$> (#peek flam3_palette_entry, index) ptr
        <*> (#peek flam3_palette_entry, color) ptr

peekAffineToMatrix :: Ptr CDouble -> IO Matrix3
peekAffineToMatrix ptr = do
    [xx, yx, xy, yy, xo, yo] <- peekArray 6 ptr
    return $ Matrix3 ((xx, xy, xo), (yx, yy, yo))

data XForm = XForm
    -- skipped: var
    { xfProj            :: Matrix3
    , xfProjPost        :: Maybe Matrix3
    , xfDensity         :: CDouble
    , xfColorCoord      :: CDouble
    , xfColorSpeed      :: CDouble
    -- skipped: animate
    , xfOpacity         :: CDouble
    -- skipped: vis_adjusted, padding, wind
    , xfPreblur         :: CDouble
    -- skipped (well, moved): has_post
    -- skipped: a hell of a lot of parameters (to be added later)
    , xfVars            :: [(CDouble, Variation)]
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

        hasPost <- iToBool <$> (#peek flam3_xform, has_post) ptr
        post <- if not hasPost then return Nothing
            else Just <$> peekAffineToMatrix ((#ptr flam3_xform, post) ptr)

        XForm   <$> peekAffineToMatrix ((#ptr flam3_xform, c) ptr)
                <*> pure post
                <*> (#peek flam3_xform, density) ptr
                <*> (#peek flam3_xform, color) ptr
                <*> (#peek flam3_xform, color_speed) ptr
                <*> (#peek flam3_xform, opacity) ptr
                <*> (#peek flam3_xform, has_preblur) ptr
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
    { gnTime                :: CDouble
    -- skipped: interpolation, interpolation_type, palette_interpolation
    , gnXForms              :: [XForm]
    , gnXFormTotalDensity   :: CDouble
    , gnFinalXForm          :: Maybe XForm
    -- skipped: chaos, chaos_enable. TODO: re-enable.
    -- skipped: genome_index, parent_fname, symmetry
    , gnPalette             :: PaletteList
    -- skipped: input_image, palette_index
    , gnBrightness          :: CDouble
    , gnContrast            :: CDouble
    , gnGamma               :: CDouble
    , gnHighlightPower      :: CDouble
    , gnWidth               :: CInt
    , gnHeight              :: CInt
    , gnSpatialOversample   :: CInt
    , gnCenter              :: Point2
    , gnRotCenter           :: Point2
    , gnRotate              :: CDouble
    , gnVibrancy            :: CDouble
    -- skipped: hue_rotation
    , gnBackground          :: RGBColor
    , gnZoom                :: CDouble
    , gnPixelsPerUnit       :: CDouble
    , gnSpatialFiltRadius   :: CDouble
    , gnSpatialFiltType     :: FilterType
    , gnSampleDensity       :: CDouble
    , gnNBatches            :: CInt
    , gnNTemporalSamples    :: CInt
    , gnEstimator           :: CDouble
    , gnEstimatorCurve      :: CDouble
    , gnEstimatorMin        :: CDouble
    -- skipped: xmlDocPtr edits;
    , gnGammaThreshold      :: CDouble
    -- skipped: palette_index0 to palette_blend
    , gnTemporalFiltType    :: FilterType
    , gnTemporalFiltWidth   :: CDouble
    , gnTemporalFiltExp     :: CDouble
    , gnPaletteMode         :: PaletteMode
    } deriving (Eq, Show)

instance Storable Genome where
    sizeOf _ = #size flam3_genome
    alignment _ = 16 -- ?

    peek ptr = do
        nxforms <- fmap ci2I $ (#peek flam3_genome, num_xforms) ptr
        useFinal <- iToBool <$> (#peek flam3_genome, final_xform_enable) ptr
        finalIdx <- (#peek flam3_genome, final_xform_index) ptr
        xforms <- peekArray nxforms =<< (#peek flam3_genome, xform) ptr
        let xforms' = if useFinal
                then if finalIdx == nxforms - 1
                        then take (finalIdx - 1) xforms
                        else take (finalIdx - 1) xforms ++ drop finalIdx xforms
                else xforms
            final = if useFinal then Just (xforms !! finalIdx) else Nothing

        Genome  <$> (#peek flam3_genome, time) ptr
                <*> pure xforms' <*> pure (sum $ map xfDensity xforms')
                <*> pure final
                <*> (PaletteList . V.fromList <$>
                     peekArray 256 ((#ptr flam3_genome, palette) ptr))
                <*> (#peek flam3_genome, brightness) ptr
                <*> (#peek flam3_genome, contrast) ptr
                <*> (#peek flam3_genome, gamma) ptr
                <*> (#peek flam3_genome, highlight_power) ptr
                <*> (#peek flam3_genome, width) ptr
                <*> (#peek flam3_genome, height) ptr
                <*> (#peek flam3_genome, spatial_oversample) ptr
                <*> readPoint2 ((#ptr flam3_genome, center) ptr)
                <*> readPoint2 ((#ptr flam3_genome, rot_center) ptr)
                <*> (#peek flam3_genome, rotate) ptr
                <*> (#peek flam3_genome, vibrancy) ptr
                <*> (#peek flam3_genome, background) ptr
                <*> (#peek flam3_genome, zoom) ptr
                <*> (#peek flam3_genome, pixels_per_unit) ptr
                <*> (#peek flam3_genome, spatial_filter_radius) ptr
                <*> (toEnum <$> (#peek flam3_genome, spatial_filter_select) ptr)
                <*> (#peek flam3_genome, sample_density) ptr
                <*> (#peek flam3_genome, nbatches) ptr
                <*> (#peek flam3_genome, ntemporal_samples) ptr
                <*> (#peek flam3_genome, estimator) ptr
                <*> (#peek flam3_genome, estimator_curve) ptr
                <*> (#peek flam3_genome, estimator_minimum) ptr
                <*> (#peek flam3_genome, gam_lin_thresh) ptr
                <*> (toEnum <$> (#peek flam3_genome, temporal_filter_type) ptr)
                <*> (#peek flam3_genome, temporal_filter_width) ptr
                <*> (#peek flam3_genome, temporal_filter_exp) ptr
                <*> (getPalMode <$> (#peek flam3_genome, palette_mode) ptr)
      where
        getPalMode :: CInt -> PaletteMode
        getPalMode (#const flam3_palette_mode_step) = PaletteStep
        getPalMode (#const flam3_palette_mode_linear) = PaletteLinear
        readPoint2 p = (,) <$> peekElemOff p 0 <*> peekElemOff p 1

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

