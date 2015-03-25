module Image
       ( resizeImage
       , resizeImageCompiler
       , PNG(..)
       , JPG(..)
       , ResizeError
       ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Vision.Image.Storage.DevIL (SaveBSImageType, PNG(..), JPG(..))
import qualified Vision.Image.Storage.DevIL as Image

import Vision.Primitive       (Size)
import Vision.Primitive.Shape (ix1, (:.)(..), Z(..), DIM1(..), DIM2(..))

import Vision.Image.Class     (shape)
import Vision.Image.RGBA.Type (RGBA)
import Vision.Image.Transform (resize, InterpolMethod(..))

import Hakyll.Core.Compiler (Compiler, getResourceLBS, getResourceFilePath)
import Hakyll.Core.Item (Item, withItemBody)

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
               -> B.ByteString                     -- ^ The input bytestring
               -> Int                            -- ^ The max width of new image
               -> Either ResizeError B.ByteString

resizeImage imgType img w = load img >>= resizeToWidth (ix1 w) >>= save imgType


resizeImageCompiler :: (SaveBSImageType t)
                    => t
                    -> Int
                    -> Compiler (Item LB.ByteString)

resizeImageCompiler imgType sz = getResourceFilePath >>= \path -> getResourceLBS >>= withItemBody (go path)
  where
    go path img = case resizeImage imgType (LB.toStrict img) sz of
                      Left e -> error $ (show e ++ show path)
                      Right res -> return $ LB.fromStrict res


convError :: Either Image.StorageError b -> Either ResizeError b
convError (Left e)  = Left (DevILError e)
convError (Right b) = Right b


load :: B.ByteString
     -> Either ResizeError RGBA

load img = convError $ Image.loadBS Image.Autodetect img



save :: (SaveBSImageType t)
        => t
        -> RGBA
        -> Either ResizeError B.ByteString

save imgType img = convError $ Image.saveBS imgType img



resizeToWidth :: Width                   -- ^ Width to resize to
              -> RGBA                    -- ^ Image Manifest type
              -> Either ResizeError RGBA -- ^ Error or image

resizeToWidth width img = scaledSize (shape img) width >>=
                          \newSize -> Right (resize Bilinear newSize img :: RGBA)


scaledSize :: Size                    -- ^ Original dimensions
           -> Width                   -- ^ Desired width
           -> Either ResizeError DIM2 -- ^ Parameter error | new dimension

scaledSize orig@(Z:.y:.x) (Z:.width)
  | x <= 0 || y <= 0 || width <= 0 = Left DimensionTooSmall
  | x <= width = Right orig
  | otherwise = let aspectRatio = (fromIntegral x / fromIntegral y) :: Double
                    fWidth = (fromIntegral width) :: Double
                in Right (Z :. (floor (fWidth / aspectRatio)) :. width)
