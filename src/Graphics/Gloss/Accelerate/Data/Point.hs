-- |
-- Module      : Graphics.Gloss.Accelerate.Data.Point
-- Copyright   : [2013..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

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
import Data.Array.Accelerate.Linear.V2

import qualified Prelude                        as P


-- | An abstract point value on the xy-plane.
--
type Point = V2 Float

-- | Make a custom point
--
makePoint
    :: Exp Float                -- ^ x-coordinate
    -> Exp Float                -- ^ y-coordinate
    -> Exp Point
makePoint = V2_


-- | Take the components of a point
--
xyOfPoint
    :: Exp Point
    -> (Exp Float, Exp Float)
xyOfPoint (V2_ x y) = (x, y)


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
pointInBox (V2_ x0 y0) (V2_ x1 y1) (V2_ x2 y2) =
  x0 >= min x1 x2 &&
  x0 <= max x1 x2 &&
  y0 >= min y1 y2 &&
  y0 <= max y1 y2

