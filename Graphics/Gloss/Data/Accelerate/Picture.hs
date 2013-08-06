
module Graphics.Gloss.Data.Accelerate.Picture
  where

-- Standard library
import Prelude                                          as P
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

-- Gloss
import Graphics.Gloss.Data.Picture                      ( Picture(..), bitmapOfForeignPtr )

-- Accelerate
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Array.Data                 ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar                ( Array(..) )


-- | Use an Accelerate array of RGBA data as a bitmap image. If the image is
--   generated programatically every frame, then the second parameter should be
--   `False`. If you have loaded it from static data then use `True`.
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
    :: Array DIM2 Word32                -- The array data
    -> Bool                             -- Should the image be cached between frames?
    -> Picture
bitmapOfArray arrPixels cacheMe
  = let -- Size of the raw image
        Z :. sizeY :. sizeX     = arrayShape arrPixels

        -- Wrap the array data in a Foreign pointer and turn into a Gloss picture
        {-# NOINLINE rawData #-}
        rawData         = let (Array _ adata)   = arrPixels
                              ((),ptr)          = ptrsOfArrayData adata
                          in
                          unsafePerformIO       $ newForeignPtr_ (castPtr ptr)

        pic             = bitmapOfForeignPtr
                              sizeX sizeY               -- raw image size
                              rawData                   -- the image data
                              cacheMe
    in pic

