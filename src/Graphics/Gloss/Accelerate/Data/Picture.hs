-- |
-- Module      : Graphics.Gloss.Accelerate.Data.Picture
-- Copyright   : [2013..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Graphics.Gloss.Accelerate.Data.Picture
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Sugar.Array
import qualified Data.Array.Accelerate.Representation.Array         as R

import Graphics.Gloss.Rendering

import Foreign.ForeignPtr


-- | Use an Accelerate array of RGBA data as a bitmap image. If the image
-- is generated programatically every frame, then the second parameter
-- should be `False`. If you have loaded it from static data then use
-- `True`.
--

-- TODO:
--
--   If CUDA is enabled, check whether the array already exists on the device
--   and if so blitz it straight to a texture as described below. Otherwise,
--   just use this method. See also the cuda examples in the
--   non-USE_TEXSUBIMAGE2D path.
--
--       1. (once) Allocate a new texture object
--       2. Run the CUDA computation, but do not copy the result back to the host
--       3. Map the texture resource to an array
--       4. Copy the CUDA result directly to the mapped texture
--
bitmapOfArray
    :: Array DIM2 Word32                -- The array data (packed RGBA)
    -> Bool                             -- Should the image be cached between frames?
    -> Picture
bitmapOfArray arrPixels cacheMe
  = let -- Size of the raw image
        Z :. sizeY :. sizeX = arrayShape arrPixels

        rawData         = let Array (R.Array _ adata) = arrPixels
                              ptr                     = unsafeGetValue (uniqueArrayData adata)
                          in
                          castForeignPtr ptr

        fmt             = BitmapFormat TopToBottom PxRGBA   -- assume little-endian host
        pic             = bitmapOfForeignPtr
                              sizeX sizeY                   -- image size
                              fmt                           -- image format
                              rawData                       -- raw image data
                              cacheMe
    in pic

