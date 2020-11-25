{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_texture_compression_astc_hdr"
module Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr  ( PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT(..)
                                                              , EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION
                                                              , pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION
                                                              , EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
                                                              , pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
                                                              ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT"
data PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT" "textureCompressionASTC_HDR"
    textureCompressionASTC_HDR :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT

instance ToCStruct PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (textureCompressionASTC_HDR))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  peekCStruct p = do
    textureCompressionASTC_HDR <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
             (bool32ToBool textureCompressionASTC_HDR)


instance Storable PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  zero = PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
           zero


type EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION"
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1


type EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = "VK_EXT_texture_compression_astc_hdr"

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME"
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = "VK_EXT_texture_compression_astc_hdr"

