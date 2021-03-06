{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_group"
module Vulkan.Core11.Promoted_From_VK_KHR_device_group  ( DeviceGroupBindSparseInfo
                                                        , DeviceGroupCommandBufferBeginInfo
                                                        , DeviceGroupRenderPassBeginInfo
                                                        , DeviceGroupSubmitInfo
                                                        , MemoryAllocateFlagsInfo
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceGroupBindSparseInfo

instance ToCStruct DeviceGroupBindSparseInfo
instance Show DeviceGroupBindSparseInfo

instance FromCStruct DeviceGroupBindSparseInfo


data DeviceGroupCommandBufferBeginInfo

instance ToCStruct DeviceGroupCommandBufferBeginInfo
instance Show DeviceGroupCommandBufferBeginInfo

instance FromCStruct DeviceGroupCommandBufferBeginInfo


data DeviceGroupRenderPassBeginInfo

instance ToCStruct DeviceGroupRenderPassBeginInfo
instance Show DeviceGroupRenderPassBeginInfo

instance FromCStruct DeviceGroupRenderPassBeginInfo


data DeviceGroupSubmitInfo

instance ToCStruct DeviceGroupSubmitInfo
instance Show DeviceGroupSubmitInfo

instance FromCStruct DeviceGroupSubmitInfo


data MemoryAllocateFlagsInfo

instance ToCStruct MemoryAllocateFlagsInfo
instance Show MemoryAllocateFlagsInfo

instance FromCStruct MemoryAllocateFlagsInfo

