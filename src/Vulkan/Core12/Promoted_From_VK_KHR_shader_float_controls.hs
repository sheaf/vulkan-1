{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_float_controls"
module Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls  ( PhysicalDeviceFloatControlsProperties(..)
                                                                 , StructureType(..)
                                                                 , ShaderFloatControlsIndependence(..)
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES))
import Vulkan.Core12.Enums.ShaderFloatControlsIndependence (ShaderFloatControlsIndependence(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceFloatControlsProperties"
data PhysicalDeviceFloatControlsProperties = PhysicalDeviceFloatControlsProperties
  { -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "denormBehaviorIndependence"
    denormBehaviorIndependence :: ShaderFloatControlsIndependence
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "roundingModeIndependence"
    roundingModeIndependence :: ShaderFloatControlsIndependence
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderSignedZeroInfNanPreserveFloat16"
    shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderSignedZeroInfNanPreserveFloat32"
    shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderSignedZeroInfNanPreserveFloat64"
    shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderDenormPreserveFloat16"
    shaderDenormPreserveFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderDenormPreserveFloat32"
    shaderDenormPreserveFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderDenormPreserveFloat64"
    shaderDenormPreserveFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderDenormFlushToZeroFloat16"
    shaderDenormFlushToZeroFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderDenormFlushToZeroFloat32"
    shaderDenormFlushToZeroFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderDenormFlushToZeroFloat64"
    shaderDenormFlushToZeroFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderRoundingModeRTEFloat16"
    shaderRoundingModeRTEFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderRoundingModeRTEFloat32"
    shaderRoundingModeRTEFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderRoundingModeRTEFloat64"
    shaderRoundingModeRTEFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderRoundingModeRTZFloat16"
    shaderRoundingModeRTZFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderRoundingModeRTZFloat32"
    shaderRoundingModeRTZFloat32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsProperties" "shaderRoundingModeRTZFloat64"
    shaderRoundingModeRTZFloat64 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFloatControlsProperties)
#endif
deriving instance Show PhysicalDeviceFloatControlsProperties

instance ToCStruct PhysicalDeviceFloatControlsProperties where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFloatControlsProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderFloatControlsIndependence)) (denormBehaviorIndependence)
    poke ((p `plusPtr` 20 :: Ptr ShaderFloatControlsIndependence)) (roundingModeIndependence)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat16))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat32))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat64))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat16))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat32))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (shaderDenormPreserveFloat64))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat16))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat32))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (shaderDenormFlushToZeroFloat64))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat16))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat32))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTEFloat64))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat16))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat32))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (shaderRoundingModeRTZFloat64))
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderFloatControlsIndependence)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ShaderFloatControlsIndependence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFloatControlsProperties where
  peekCStruct p = do
    denormBehaviorIndependence <- peek @ShaderFloatControlsIndependence ((p `plusPtr` 16 :: Ptr ShaderFloatControlsIndependence))
    roundingModeIndependence <- peek @ShaderFloatControlsIndependence ((p `plusPtr` 20 :: Ptr ShaderFloatControlsIndependence))
    shaderSignedZeroInfNanPreserveFloat16 <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderSignedZeroInfNanPreserveFloat32 <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderSignedZeroInfNanPreserveFloat64 <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderDenormPreserveFloat16 <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderDenormPreserveFloat32 <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    shaderDenormPreserveFloat64 <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat16 <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat32 <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    shaderDenormFlushToZeroFloat64 <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    shaderRoundingModeRTEFloat16 <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    shaderRoundingModeRTEFloat32 <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    shaderRoundingModeRTEFloat64 <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    shaderRoundingModeRTZFloat16 <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    shaderRoundingModeRTZFloat32 <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    shaderRoundingModeRTZFloat64 <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    pure $ PhysicalDeviceFloatControlsProperties
             denormBehaviorIndependence roundingModeIndependence (bool32ToBool shaderSignedZeroInfNanPreserveFloat16) (bool32ToBool shaderSignedZeroInfNanPreserveFloat32) (bool32ToBool shaderSignedZeroInfNanPreserveFloat64) (bool32ToBool shaderDenormPreserveFloat16) (bool32ToBool shaderDenormPreserveFloat32) (bool32ToBool shaderDenormPreserveFloat64) (bool32ToBool shaderDenormFlushToZeroFloat16) (bool32ToBool shaderDenormFlushToZeroFloat32) (bool32ToBool shaderDenormFlushToZeroFloat64) (bool32ToBool shaderRoundingModeRTEFloat16) (bool32ToBool shaderRoundingModeRTEFloat32) (bool32ToBool shaderRoundingModeRTEFloat64) (bool32ToBool shaderRoundingModeRTZFloat16) (bool32ToBool shaderRoundingModeRTZFloat32) (bool32ToBool shaderRoundingModeRTZFloat64)


instance Storable PhysicalDeviceFloatControlsProperties where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFloatControlsProperties where
  zero = PhysicalDeviceFloatControlsProperties
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero

