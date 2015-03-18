module Image
       (
         resizeImage,
         PNG,
         JPG,
         ResizeError
       ) where


import Data.ByteString (ByteString)


import Vision.Image.Storage.DevIL (SaveBSImageType, PNG, JPG)
import qualified Vision.Image.Storage.DevIL as Image

import Vision.Primitive       (Size)
import Vision.Primitive.Shape

import Vision.Image.Class     (shape)
import Vision.Image.RGBA.Type (RGBA)
import Vision.Image.Transform (resize, InterpolMethod(..))


------------------------------------------------------------------------------

data ResizeError = DimensionTooSmall | DevILError Image.StorageError
                 deriving Show


type Width = DIM1

------------------------------------------------------------------------------

-- | The bread and butter of this module
-- The purpose of this function is given a desired output type we will
-- scale the image so that the desired width matches the
-- specification.
--
-- If the width is already smaller then we do not resize the image
-- @
-- resizeImage PNG bs 300
-- @

resizeImage :: (SaveBSImageType t)
               => t                              -- ^ Type of image to save as, either PNG | JPG
               -> ByteString                     -- ^ The input bytestring
               -> Int                            -- ^ The max width of new image
               -> Either ResizeError ByteString

resizeImage imgType img w = load img >>= resizeToWidth (ix1 w) >>= save imgType


load :: ByteString -> Either ResizeError RGBA
load img = case Image.loadBS Image.Autodetect img of
            Left err -> Left (DevILError err)
            Right i  -> Right i

save :: (SaveBSImageType t) => t -> RGBA -> Either ResizeError ByteString
save imgType img = case Image.saveBS imgType img of
            Left err -> Left (DevILError err)
            Right i  -> Right i


resizeToWidth :: Width -> -- Width to resize to
                 RGBA ->    -- Image Manifest type
                 Either ResizeError RGBA -- Error or image

resizeToWidth width img = scaledSize (shape img) width >>=
                          \newSize -> Right (resize Bilinear newSize img :: RGBA)


scaledSize :: Size -- Original dimension
           -> Width -- Desired width
           -> Either ResizeError DIM2 -- Parameter error | new dimension

scaledSize orig@(Z:.y:.x) (Z:.width)
  | x <= 0 || y <= 0 || width <= 0 = Left DimensionTooSmall
  | x <= width = Right orig
  | otherwise = let aspectRatio = fromIntegral x / fromIntegral y
                    fWidth = fromIntegral width
                in Right (Z :. (floor (fWidth / aspectRatio)) :. width)
