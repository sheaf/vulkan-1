{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_memory_budget"
module Vulkan.Extensions.VK_EXT_memory_budget  ( PhysicalDeviceMemoryBudgetPropertiesEXT(..)
                                               , EXT_MEMORY_BUDGET_SPEC_VERSION
                                               , pattern EXT_MEMORY_BUDGET_SPEC_VERSION
                                               , EXT_MEMORY_BUDGET_EXTENSION_NAME
                                               , pattern EXT_MEMORY_BUDGET_EXTENSION_NAME
                                               ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.APIConstants (MAX_MEMORY_HEAPS)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.APIConstants (pattern MAX_MEMORY_HEAPS)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceMemoryBudgetPropertiesEXT"
data PhysicalDeviceMemoryBudgetPropertiesEXT = PhysicalDeviceMemoryBudgetPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryBudgetPropertiesEXT" "heapBudget"
    heapBudget :: Vector DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryBudgetPropertiesEXT" "heapUsage"
    heapUsage :: Vector DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryBudgetPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT where
  withCStruct x f = allocaBytesAligned 272 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryBudgetPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (heapBudget)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapBudget is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (heapBudget)
    unless ((Data.Vector.length $ (heapUsage)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapUsage is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 144 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (heapUsage)
    f
  cStructSize = 272
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapBudget is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (mempty)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "heapUsage is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 144 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (mempty)
    f

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT where
  peekCStruct p = do
    heapBudget <- generateM (MAX_MEMORY_HEAPS) (\i -> peek @DeviceSize (((lowerArrayPtr @DeviceSize ((p `plusPtr` 16 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
    heapUsage <- generateM (MAX_MEMORY_HEAPS) (\i -> peek @DeviceSize (((lowerArrayPtr @DeviceSize ((p `plusPtr` 144 :: Ptr (FixedArray MAX_MEMORY_HEAPS DeviceSize)))) `advancePtrBytes` (8 * (i)) :: Ptr DeviceSize)))
    pure $ PhysicalDeviceMemoryBudgetPropertiesEXT
             heapBudget heapUsage


instance Storable PhysicalDeviceMemoryBudgetPropertiesEXT where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryBudgetPropertiesEXT where
  zero = PhysicalDeviceMemoryBudgetPropertiesEXT
           mempty
           mempty


type EXT_MEMORY_BUDGET_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_SPEC_VERSION"
pattern EXT_MEMORY_BUDGET_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MEMORY_BUDGET_SPEC_VERSION = 1


type EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_EXTENSION_NAME"
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"

