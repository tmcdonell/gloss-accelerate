{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Colours without an alpha component
--
module Graphics.Gloss.Accelerate.Data.Color.RGB (

  -- ** Color data type
  Color, RGB(..),
  makeColor,
  makeColor8,
  rawColor,
  rgbOfColor,
  packRGBA, packABGR,
  clampColor,

  -- ** Color functions
  mixColors,
  addColors,
  dim, brighten,
  lighten, darken,

  -- ** Pre-defined colors
  greyN, black, white,

  -- *** Primary
  red, green, blue,

  -- *** Secondary
  yellow, cyan, magenta,

  -- *** Tertiary
  rose, violet, azure, aquamarine, chartreuse, orange,

) where

import Prelude                                  as P
import Data.Typeable
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Data.Bits          as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..) )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )


-- | An abstract color value.
--
-- We keep the type abstract so we can be sure that the components are in the
-- required range. To make a custom color use 'makeColor'.
--
type Color = RGB Float

-- | Same as 'Graphics.Gloss.Accelerate.Data.Color.RGBA.RGBA', but colours don't
-- have an alpha component. All components like in the range [0..1).
--
-- We need to parameterise by a type so that we can have both Exp (RGB a) and
-- RGB (Exp a).
--
data RGB a = RGB a a a
  deriving (Show, P.Eq, Typeable)


instance P.Num a => P.Num (RGB a) where
  (+) (RGB r1 g1 b1 ) (RGB r2 g2 b2)
        = RGB (r1 + r2) (g1 + g2) (b1 + b2)

  (-) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 - r2) (g1 - g2) (b1 - b2)

  (*) (RGB r1 g1 b1) (RGB r2 g2 b2)
        = RGB (r1 * r2) (g1 * g2) (b1 * b2)

  abs (RGB r1 g1 b1)
        = RGB (abs r1) (abs g1) (abs b1)

  signum (RGB r1 g1 b1)
        = RGB (signum r1) (signum g1) (signum b1)

  fromInteger i
        = let f = fromInteger i
          in  RGB f f f

instance A.Num a => P.Num (Exp (RGB a)) where
  (+)           = lift2 ((+) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (-)           = lift2 ((-) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  (*)           = lift2 ((*) :: RGB (Exp a) -> RGB (Exp a) -> RGB (Exp a))
  abs           = lift1 (abs :: RGB (Exp a) -> RGB (Exp a))
  signum        = lift1 (signum :: RGB (Exp a) -> RGB (Exp a))
  fromInteger i = let f = fromInteger i :: Exp a
                  in lift $ RGB f f f

-- Represent colours in Accelerate as a 4-tuple
--
type instance EltRepr (RGB a) = EltRepr (a, a, a)

instance Elt a => Elt (RGB a) where
  eltType (_ :: RGB a)          = eltType (undefined :: (a,a,a))
  toElt c                       = let (r,g,b) = toElt c in RGB r g b
  fromElt (RGB r g b)           = fromElt (r,g,b)

instance Elt a => IsProduct Elt (RGB a) where
  type ProdRepr (RGB a)          = ((((),a), a), a)
  fromProd _ (RGB r g b)         = ((((), r), g), b)
  toProd _ ((((),r),g),b)        = RGB r g b
  prod cst _                     = prod cst (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RGB a) where
  type Plain (RGB a)    = RGB (Plain a)
  lift (RGB r g b)      = Exp . Tuple $ NilTup `SnocTup` lift r `SnocTup` lift g `SnocTup` lift b

instance Elt a => Unlift Exp (RGB (Exp a)) where
  unlift c      = let r = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` c
                      g = Exp $ SuccTupIdx ZeroTupIdx `Prj` c
                      b = Exp $ ZeroTupIdx `Prj` c
                  in RGB r g b


-- | Make a custom color. All components are clamped to the range  [0..1].
--
makeColor
    :: Exp Float        -- ^ Red component.
    -> Exp Float        -- ^ Green component.
    -> Exp Float        -- ^ Blue component.
    -> Exp Color
makeColor r g b
  = clampColor
  $ rawColor r g b


-- | Make a custom color.
--   You promise that all components are clamped to the range [0..1]
--
rawColor :: Exp Float -> Exp Float -> Exp Float -> Exp Color
rawColor r g b = lift (RGB r g b)


-- | Make a custom color from 8-bit values.
--
makeColor8
    :: Exp Word8        -- ^ Red component.
    -> Exp Word8        -- ^ Green component.
    -> Exp Word8        -- ^ Blue component.
    -> Exp Color
makeColor8 r g b
  = clampColor
  $ rawColor (A.fromIntegral r / 255)
             (A.fromIntegral g / 255)
             (A.fromIntegral b / 255)


-- | Take the RGB components of a color.
rgbOfColor :: Exp Color -> (Exp Float, Exp Float, Exp Float)
rgbOfColor c
  = let (RGB r g b) = unlift c
    in  (r, g, b)


-- Internal
-- --------

-- | Clamp components of a color into the required range.
--
clampColor :: Exp Color -> Exp Color
clampColor cc
  = let (r, g, b)       = rgbOfColor cc
    in  rawColor (A.min 1 r) (A.min 1 g) (A.min 1 b)


-- | Normalise a color to the value of its largest RGB component.
--
normaliseColor :: Exp Color -> Exp Color
normaliseColor cc
  = let (r, g, b)       = rgbOfColor cc
        m               = P.maximum [r, g, b]
    in  rawColor (r / m) (g / m) (b / m)


-- | Convert a color into a packed RGBA value.
--
packRGBA :: Exp Color -> Exp Word32
packRGBA c
  = let (r, g, b)       = rgbOfColor c
    in  word32OfFloat r `A.shiftL` 24
    .|. word32OfFloat g `A.shiftL` 16
    .|. word32OfFloat b `A.shiftL` 8
    .|. 0xFF

-- | Convert a colour into a packed BGRA value.
--
-- This is necessary as OpenGL reads pixel data as ABGR, rather than RGBA.
--
packABGR :: Exp Color -> Exp Word32
packABGR c
  = let (r, g, b)       = rgbOfColor c
        a               = 1.0
    in  word32OfFloat a `A.shiftL` 24
    .|. word32OfFloat b `A.shiftL` 16
    .|. word32OfFloat g `A.shiftL` 8
    .|. word32OfFloat r

word32OfFloat :: Exp Float -> Exp Word32
word32OfFloat f = A.truncate (f * 255)


-- Color functions ------------------------------------------------------------

-- | Mix two colors with the given ratios.
--
mixColors
    :: Exp Float        -- ^ Ratio of first color.
    -> Exp Float        -- ^ Ratio of second color.
    -> Exp Color        -- ^ First color.
    -> Exp Color        -- ^ Second color.
    -> Exp Color        -- ^ Resulting color.

mixColors ratio1 ratio2 c1 c2
  = let (r1, g1, b1)            = rgbOfColor c1
        (r2, g2, b2)            = rgbOfColor c2

        total   = ratio1 + ratio2
        m1      = ratio1 / total
        m2      = ratio2 / total
   in
   rawColor (m1 * r1 + m2 * r2)
            (m1 * g1 + m2 * g2)
            (m1 * b1 + m2 * b2)


-- | Add RGB components of a color component-wise, then normalise them to the
--   highest resulting one. The alpha components are averaged.
--
addColors :: Exp Color -> Exp Color -> Exp Color
addColors c1 c2
  = let (r1, g1, b1)            = rgbOfColor c1
        (r2, g2, b2)            = rgbOfColor c2
    in
    normaliseColor $ rawColor (r1 + r2) (g1 + g2) (b1 + b2)

-- | Make a dimmer version of a color, scaling towards black.
--
dim :: Exp Color -> Exp Color
dim c
  = let (r, g, b)               = rgbOfColor c
    in  rawColor (r / 1.2) (g / 1.2) (b / 1.2)

-- | Make a brighter version of a color, scaling towards white.
--
brighten :: Exp Color -> Exp Color
brighten c
  = let (r, g, b)               = rgbOfColor c
    in clampColor $ rawColor (r * 1.2) (g * 1.2) (b * 1.2)

-- | Lighten a color, adding white.
--
lighten :: Exp Color -> Exp Color
lighten c
  = let (r, g, b)               = rgbOfColor c
    in  clampColor $ rawColor (r + 0.2) (g + 0.2) (b + 0.2)

-- | Darken a color, adding black.
--
darken :: Exp Color -> Exp Color
darken c
  = let (r, g, b)               = rgbOfColor c
    in  clampColor $ rawColor (r - 0.2) (g - 0.2) (b - 0.2)


-- Pre-defined Colors ---------------------------------------------------------

-- | A greyness of a given magnitude.
--
greyN :: Exp Float      -- ^ Range is 0 = black, to 1 = white.
      -> Exp Color
greyN n         = rawColor n   n   n

black, white :: Exp Color
black           = rawColor 0.0 0.0 0.0
white           = rawColor 1.0 1.0 1.0

-- Colors from the additive color wheel.
red, green, blue :: Exp Color
red             = rawColor 1.0 0.0 0.0
green           = rawColor 0.0 1.0 0.0
blue            = rawColor 0.0 0.0 1.0

-- secondary
yellow, cyan, magenta :: Exp Color
yellow          = addColors red   green
cyan            = addColors green blue
magenta         = addColors red   blue

-- tertiary
rose, violet, azure, aquamarine, chartreuse, orange :: Exp Color
rose            = addColors red     magenta
violet          = addColors magenta blue
azure           = addColors blue    cyan
aquamarine      = addColors cyan    green
chartreuse      = addColors green   yellow
orange          = addColors yellow  red

