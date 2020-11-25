{-# language CPP #-}
-- No documentation found for Chapter "ObjectType"
module Vulkan.Core10.Enums.ObjectType  (ObjectType( OBJECT_TYPE_UNKNOWN
                                                  , OBJECT_TYPE_INSTANCE
                                                  , OBJECT_TYPE_PHYSICAL_DEVICE
                                                  , OBJECT_TYPE_DEVICE
                                                  , OBJECT_TYPE_QUEUE
                                                  , OBJECT_TYPE_SEMAPHORE
                                                  , OBJECT_TYPE_COMMAND_BUFFER
                                                  , OBJECT_TYPE_FENCE
                                                  , OBJECT_TYPE_DEVICE_MEMORY
                                                  , OBJECT_TYPE_BUFFER
                                                  , OBJECT_TYPE_IMAGE
                                                  , OBJECT_TYPE_EVENT
                                                  , OBJECT_TYPE_QUERY_POOL
                                                  , OBJECT_TYPE_BUFFER_VIEW
                                                  , OBJECT_TYPE_IMAGE_VIEW
                                                  , OBJECT_TYPE_SHADER_MODULE
                                                  , OBJECT_TYPE_PIPELINE_CACHE
                                                  , OBJECT_TYPE_PIPELINE_LAYOUT
                                                  , OBJECT_TYPE_RENDER_PASS
                                                  , OBJECT_TYPE_PIPELINE
                                                  , OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
                                                  , OBJECT_TYPE_SAMPLER
                                                  , OBJECT_TYPE_DESCRIPTOR_POOL
                                                  , OBJECT_TYPE_DESCRIPTOR_SET
                                                  , OBJECT_TYPE_FRAMEBUFFER
                                                  , OBJECT_TYPE_COMMAND_POOL
                                                  , OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT
                                                  , OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV
                                                  , OBJECT_TYPE_DEFERRED_OPERATION_KHR
                                                  , OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL
                                                  , OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
                                                  , OBJECT_TYPE_VALIDATION_CACHE_EXT
                                                  , OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR
                                                  , OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
                                                  , OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT
                                                  , OBJECT_TYPE_DISPLAY_MODE_KHR
                                                  , OBJECT_TYPE_DISPLAY_KHR
                                                  , OBJECT_TYPE_SWAPCHAIN_KHR
                                                  , OBJECT_TYPE_SURFACE_KHR
                                                  , OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
                                                  , OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION
                                                  , ..
                                                  )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkObjectType"
newtype ObjectType = ObjectType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_UNKNOWN"
pattern OBJECT_TYPE_UNKNOWN                         = ObjectType 0
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INSTANCE"
pattern OBJECT_TYPE_INSTANCE                        = ObjectType 1
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
pattern OBJECT_TYPE_PHYSICAL_DEVICE                 = ObjectType 2
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE"
pattern OBJECT_TYPE_DEVICE                          = ObjectType 3
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUEUE"
pattern OBJECT_TYPE_QUEUE                           = ObjectType 4
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SEMAPHORE"
pattern OBJECT_TYPE_SEMAPHORE                       = ObjectType 5
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_BUFFER"
pattern OBJECT_TYPE_COMMAND_BUFFER                  = ObjectType 6
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FENCE"
pattern OBJECT_TYPE_FENCE                           = ObjectType 7
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE_MEMORY"
pattern OBJECT_TYPE_DEVICE_MEMORY                   = ObjectType 8
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER"
pattern OBJECT_TYPE_BUFFER                          = ObjectType 9
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE"
pattern OBJECT_TYPE_IMAGE                           = ObjectType 10
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_EVENT"
pattern OBJECT_TYPE_EVENT                           = ObjectType 11
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUERY_POOL"
pattern OBJECT_TYPE_QUERY_POOL                      = ObjectType 12
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER_VIEW"
pattern OBJECT_TYPE_BUFFER_VIEW                     = ObjectType 13
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE_VIEW"
pattern OBJECT_TYPE_IMAGE_VIEW                      = ObjectType 14
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SHADER_MODULE"
pattern OBJECT_TYPE_SHADER_MODULE                   = ObjectType 15
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_CACHE"
pattern OBJECT_TYPE_PIPELINE_CACHE                  = ObjectType 16
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
pattern OBJECT_TYPE_PIPELINE_LAYOUT                 = ObjectType 17
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_RENDER_PASS"
pattern OBJECT_TYPE_RENDER_PASS                     = ObjectType 18
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE"
pattern OBJECT_TYPE_PIPELINE                        = ObjectType 19
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
pattern OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT           = ObjectType 20
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER"
pattern OBJECT_TYPE_SAMPLER                         = ObjectType 21
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
pattern OBJECT_TYPE_DESCRIPTOR_POOL                 = ObjectType 22
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET"
pattern OBJECT_TYPE_DESCRIPTOR_SET                  = ObjectType 23
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FRAMEBUFFER"
pattern OBJECT_TYPE_FRAMEBUFFER                     = ObjectType 24
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_POOL"
pattern OBJECT_TYPE_COMMAND_POOL                    = ObjectType 25
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT"
pattern OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT           = ObjectType 1000295000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV"
pattern OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV     = ObjectType 1000277000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR"
pattern OBJECT_TYPE_DEFERRED_OPERATION_KHR          = ObjectType 1000268000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL"
pattern OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL = ObjectType 1000210000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_NV       = ObjectType 1000165000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern OBJECT_TYPE_VALIDATION_CACHE_EXT            = ObjectType 1000160000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR"
pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR      = ObjectType 1000150000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
pattern OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT       = ObjectType 1000128000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
pattern OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT       = ObjectType 1000011000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
pattern OBJECT_TYPE_DISPLAY_MODE_KHR                = ObjectType 1000002001
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_KHR"
pattern OBJECT_TYPE_DISPLAY_KHR                     = ObjectType 1000002000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SWAPCHAIN_KHR"
pattern OBJECT_TYPE_SWAPCHAIN_KHR                   = ObjectType 1000001000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SURFACE_KHR"
pattern OBJECT_TYPE_SURFACE_KHR                     = ObjectType 1000000000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE      = ObjectType 1000085000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
pattern OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION        = ObjectType 1000156000
{-# complete OBJECT_TYPE_UNKNOWN,
             OBJECT_TYPE_INSTANCE,
             OBJECT_TYPE_PHYSICAL_DEVICE,
             OBJECT_TYPE_DEVICE,
             OBJECT_TYPE_QUEUE,
             OBJECT_TYPE_SEMAPHORE,
             OBJECT_TYPE_COMMAND_BUFFER,
             OBJECT_TYPE_FENCE,
             OBJECT_TYPE_DEVICE_MEMORY,
             OBJECT_TYPE_BUFFER,
             OBJECT_TYPE_IMAGE,
             OBJECT_TYPE_EVENT,
             OBJECT_TYPE_QUERY_POOL,
             OBJECT_TYPE_BUFFER_VIEW,
             OBJECT_TYPE_IMAGE_VIEW,
             OBJECT_TYPE_SHADER_MODULE,
             OBJECT_TYPE_PIPELINE_CACHE,
             OBJECT_TYPE_PIPELINE_LAYOUT,
             OBJECT_TYPE_RENDER_PASS,
             OBJECT_TYPE_PIPELINE,
             OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT,
             OBJECT_TYPE_SAMPLER,
             OBJECT_TYPE_DESCRIPTOR_POOL,
             OBJECT_TYPE_DESCRIPTOR_SET,
             OBJECT_TYPE_FRAMEBUFFER,
             OBJECT_TYPE_COMMAND_POOL,
             OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT,
             OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV,
             OBJECT_TYPE_DEFERRED_OPERATION_KHR,
             OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL,
             OBJECT_TYPE_ACCELERATION_STRUCTURE_NV,
             OBJECT_TYPE_VALIDATION_CACHE_EXT,
             OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR,
             OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT,
             OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT,
             OBJECT_TYPE_DISPLAY_MODE_KHR,
             OBJECT_TYPE_DISPLAY_KHR,
             OBJECT_TYPE_SWAPCHAIN_KHR,
             OBJECT_TYPE_SURFACE_KHR,
             OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
             OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: ObjectType #-}

conNameObjectType :: String
conNameObjectType = "ObjectType"

enumPrefixObjectType :: String
enumPrefixObjectType = "OBJECT_TYPE_"

showTableObjectType :: [(ObjectType, String)]
showTableObjectType =
  [ (OBJECT_TYPE_UNKNOWN                        , "UNKNOWN")
  , (OBJECT_TYPE_INSTANCE                       , "INSTANCE")
  , (OBJECT_TYPE_PHYSICAL_DEVICE                , "PHYSICAL_DEVICE")
  , (OBJECT_TYPE_DEVICE                         , "DEVICE")
  , (OBJECT_TYPE_QUEUE                          , "QUEUE")
  , (OBJECT_TYPE_SEMAPHORE                      , "SEMAPHORE")
  , (OBJECT_TYPE_COMMAND_BUFFER                 , "COMMAND_BUFFER")
  , (OBJECT_TYPE_FENCE                          , "FENCE")
  , (OBJECT_TYPE_DEVICE_MEMORY                  , "DEVICE_MEMORY")
  , (OBJECT_TYPE_BUFFER                         , "BUFFER")
  , (OBJECT_TYPE_IMAGE                          , "IMAGE")
  , (OBJECT_TYPE_EVENT                          , "EVENT")
  , (OBJECT_TYPE_QUERY_POOL                     , "QUERY_POOL")
  , (OBJECT_TYPE_BUFFER_VIEW                    , "BUFFER_VIEW")
  , (OBJECT_TYPE_IMAGE_VIEW                     , "IMAGE_VIEW")
  , (OBJECT_TYPE_SHADER_MODULE                  , "SHADER_MODULE")
  , (OBJECT_TYPE_PIPELINE_CACHE                 , "PIPELINE_CACHE")
  , (OBJECT_TYPE_PIPELINE_LAYOUT                , "PIPELINE_LAYOUT")
  , (OBJECT_TYPE_RENDER_PASS                    , "RENDER_PASS")
  , (OBJECT_TYPE_PIPELINE                       , "PIPELINE")
  , (OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT          , "DESCRIPTOR_SET_LAYOUT")
  , (OBJECT_TYPE_SAMPLER                        , "SAMPLER")
  , (OBJECT_TYPE_DESCRIPTOR_POOL                , "DESCRIPTOR_POOL")
  , (OBJECT_TYPE_DESCRIPTOR_SET                 , "DESCRIPTOR_SET")
  , (OBJECT_TYPE_FRAMEBUFFER                    , "FRAMEBUFFER")
  , (OBJECT_TYPE_COMMAND_POOL                   , "COMMAND_POOL")
  , (OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT          , "PRIVATE_DATA_SLOT_EXT")
  , (OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV    , "INDIRECT_COMMANDS_LAYOUT_NV")
  , (OBJECT_TYPE_DEFERRED_OPERATION_KHR         , "DEFERRED_OPERATION_KHR")
  , (OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL, "PERFORMANCE_CONFIGURATION_INTEL")
  , (OBJECT_TYPE_ACCELERATION_STRUCTURE_NV      , "ACCELERATION_STRUCTURE_NV")
  , (OBJECT_TYPE_VALIDATION_CACHE_EXT           , "VALIDATION_CACHE_EXT")
  , (OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR     , "ACCELERATION_STRUCTURE_KHR")
  , (OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT      , "DEBUG_UTILS_MESSENGER_EXT")
  , (OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT      , "DEBUG_REPORT_CALLBACK_EXT")
  , (OBJECT_TYPE_DISPLAY_MODE_KHR               , "DISPLAY_MODE_KHR")
  , (OBJECT_TYPE_DISPLAY_KHR                    , "DISPLAY_KHR")
  , (OBJECT_TYPE_SWAPCHAIN_KHR                  , "SWAPCHAIN_KHR")
  , (OBJECT_TYPE_SURFACE_KHR                    , "SURFACE_KHR")
  , (OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE     , "DESCRIPTOR_UPDATE_TEMPLATE")
  , (OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION       , "SAMPLER_YCBCR_CONVERSION")
  ]


instance Show ObjectType where
showsPrec =
  enumShowsPrec enumPrefixObjectType showTableObjectType conNameObjectType (\(ObjectType x) -> x) (showsPrec 11)


instance Read ObjectType where
  readPrec = enumReadPrec enumPrefixObjectType showTableObjectType conNameObjectType ObjectType

