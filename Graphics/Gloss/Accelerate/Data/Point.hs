{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Graphics.Gloss.Accelerate.Data.Point (

  -- ** Point data type
  Point,

  -- ** Point creation
  makePoint,
  xyOfPoint,
  pointOfIndex,

  -- ** Testing points
  pointInBox,

) where

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product            ( TupleIdx(..), IsProduct(..), )
import Data.Array.Accelerate.Array.Sugar        ( Elt(..), EltRepr, Tuple(..) )

import Data.Typeable
import Prelude                                  ( fromInteger )   -- ghc < 8 bug
import qualified Prelude                        as P


-- | An abstract point value on the xy-plane.
--
type Point = XY Float

-- | A parameterised point in the xy-plane. This is so that the type can be both
-- Exp (Point' a) and Point' (Exp a).
--
data XY a = XY a a
  deriving (P.Show, P.Eq, Typeable)

-- | Pretend a point is a number.
--
-- Vectors aren't real numbers according to Haskell, because they don't support
-- the multiply and divide field operators. We can pretend they are though, and
-- use the (+) and (-) operators as component-wise addition and subtraction.
--
instance P.Num a => P.Num (XY a) where
  (+) (XY x1 y1) (XY x2 y2)             = XY (x1 + x2) (y1 + y2)
  (-) (XY x1 y1) (XY x2 y2)             = XY (x1 - x2) (y1 - y2)
  (*) (XY x1 y1) (XY x2 y2)             = XY (x1 * x2) (y1 * y2)
  signum (XY x y)                       = XY (signum x) (signum y)
  abs (XY x y)                          = XY (abs x) (abs y)
  negate (XY x y)                       = XY (negate x) (negate y)
  fromInteger i                         = let f = fromInteger i
                                          in  XY f f

-- Represent points in Accelerate as a tuple
--
type instance EltRepr (XY a) = EltRepr (a, a)

instance Elt a => Elt (XY a) where
  eltType (_ :: XY a)   = eltType (undefined :: (a,a))
  toElt p               = let (x,y) = toElt p in XY x y
  fromElt (XY x y)      = fromElt (x,y)

instance Elt a => IsProduct Elt (XY a) where
  type ProdRepr (XY a) = (((),a), a)
  fromProd _ (XY x y)      = (((), x), y)
  toProd   _ (((),x),y)    = XY x y
  prod cst _               = prod cst (undefined :: (a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (XY a) where
  type Plain (XY a) = XY (Plain a)
  lift (XY x y)         = Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance Elt a => Unlift Exp (XY (Exp a)) where
  unlift p      = let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` p
                      y = Exp $ ZeroTupIdx `Prj` p
                  in XY x y


-- | Make a custom point
--
makePoint
    :: Exp Float                -- ^ x-coordinate
    -> Exp Float                -- ^ y-coordinate
    -> Exp Point
makePoint x y = lift (XY x y)


-- | Take the components of a point
--
xyOfPoint
    :: Exp Point
    -> (Exp Float, Exp Float)
xyOfPoint p
  = let XY x y  = unlift p
    in  (x, y)


-- | Convert a two-dimensional index into a point centered in a plane of the
-- given width and height.
--
pointOfIndex
    :: Int                      -- ^ width
    -> Int                      -- ^ height
    -> Exp DIM2
    -> Exp Point
pointOfIndex sizeX sizeY ix
  = let -- Size of the raw plane
        fsizeX, fsizeY  :: Float
        fsizeX          = P.fromIntegral sizeX
        fsizeY          = P.fromIntegral sizeY

        fsizeX2, fsizeY2 :: Exp Float
        fsizeX2         = constant $ fsizeX / 2
        fsizeY2         = constant $ fsizeY / 2

        -- Midpoint of plane
        midX, midY :: Exp Int
        midX            = constant $ sizeX `div` 2
        midY            = constant $ sizeY `div` 2

        -- Centre coordinate in the plane
        Z :. y :. x     = unlift ix
        x'              = A.fromIntegral (x - midX) / fsizeX2
        y'              = A.fromIntegral (y - midY) / fsizeY2
    in
    makePoint x' y'


-- | Test whether a point lies within a rectangular box that is oriented
--   on the x-y plane. The points P1-P2 are opposing points of the box,
--   but need not be in a particular order.
--
-- @
--    P2 +-------+
--       |       |
--       | + P0  |
--       |       |
--       +-------+ P1
-- @
--
pointInBox
    :: Exp Point                -- ^ point to test
    -> Exp Point                -- ^ corner of box
    -> Exp Point                -- ^ opposite corner of box
    -> Exp Bool
pointInBox p0 p1 p2
  = let XY x0 y0        = unlift p0
        XY x1 y1        = unlift p1
        XY x2 y2        = unlift p2
    in
    x0 >= min x1 x2 &&
    x0 <= max x1 x2 &&
    y0 >= min y1 y2 &&
    y0 <= max y1 y2

