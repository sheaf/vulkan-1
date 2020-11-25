{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_fragment_shader_barycentric"
module Vulkan.Extensions.VK_NV_fragment_shader_barycentric  ( PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
                                                            , NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
                                                            , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
                                                            , NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
                                                            , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV))

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV"
data PhysicalDeviceFragmentShaderBarycentricFeaturesNV = PhysicalDeviceFragmentShaderBarycentricFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV" "fragmentShaderBarycentric"
    fragmentShaderBarycentric :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
#endif
deriving instance Show PhysicalDeviceFragmentShaderBarycentricFeaturesNV

instance ToCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShaderBarycentricFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentShaderBarycentric))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  peekCStruct p = do
    fragmentShaderBarycentric <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShaderBarycentricFeaturesNV
             (bool32ToBool fragmentShaderBarycentric)


instance Storable PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  zero = PhysicalDeviceFragmentShaderBarycentricFeaturesNV
           zero


type NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1


type NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = "VK_NV_fragment_shader_barycentric"

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = "VK_NV_fragment_shader_barycentric"

