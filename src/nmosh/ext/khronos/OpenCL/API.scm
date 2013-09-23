(c-library "mocl.gen-inc.c"
           (khronos OpenCL)

(attribute ((version "1.1") 
            stdcall)

(type cl_char int8)
(type cl_uchar uint8)
(type cl_short int16)
(type cl_ushort uint16)
(type cl_int int32)
(type cl_uint uint32)
(type cl_long int64)
(type cl_ulong uint64)

(type cl_half int16)
(type cl_float float)
(type cl_double double)
           
(type __nmosh_cl_id_ptr void* internal class-pointer)
(type __nmosh_cl_id uintptr internal class-pointer)
(type cl_platform_id __nmosh_cl_id_ptr)
(type cl_device_id __nmosh_cl_id_ptr)
(type cl_context __nmosh_cl_id_ptr)
(type cl_command_queue __nmosh_cl_id_ptr)
(type cl_mem __nmosh_cl_id_ptr)
(type cl_program __nmosh_cl_id)
(type cl_kernel __nmosh_cl_id)
(type cl_event __nmosh_cl_id_ptr)
(type cl_sampler __nmosh_cl_id)

(type cl_bool cl_uint
      (enum CL_FALSE
            CL_TRUE))

(type cl_bool_blocking cl_bool internal
      (enum CL_BLOCKING
            CL_NON_BLOCKING))

(type cl_bitfield cl_ulong)
(type cl_device_type cl_bitfield
      (bits
        CL_DEVICE_TYPE_DEFAULT
        CL_DEVICE_TYPE_CPU
        CL_DEVICE_TYPE_GPU
        CL_DEVICE_TYPE_ACCELERATOR
        CL_DEVICE_TYPE_ALL))

(type cl_platform_info cl_uint
      (enum
        CL_PLATFORM_PROFILE
        CL_PLATFORM_VERSION
        CL_PLATFORM_NAME
        CL_PLATFORM_VENDOR
        CL_PLATFORM_EXTENSIONS))
(type cl_device_info cl_uint
      (enum
        CL_DEVICE_TYPE_DEFAULT                      
        CL_DEVICE_TYPE_CPU                          
        CL_DEVICE_TYPE_GPU                          
        CL_DEVICE_TYPE_ACCELERATOR                  
        CL_DEVICE_TYPE_ALL                          
        CL_DEVICE_TYPE                              
        CL_DEVICE_VENDOR_ID                         
        CL_DEVICE_MAX_COMPUTE_UNITS                 
        CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS          
        CL_DEVICE_MAX_WORK_GROUP_SIZE               
        CL_DEVICE_MAX_WORK_ITEM_SIZES               
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR       
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT      
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT        
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG       
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT      
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE     
        CL_DEVICE_MAX_CLOCK_FREQUENCY               
        CL_DEVICE_ADDRESS_BITS                      
        CL_DEVICE_MAX_READ_IMAGE_ARGS               
        CL_DEVICE_MAX_WRITE_IMAGE_ARGS              
        CL_DEVICE_MAX_MEM_ALLOC_SIZE                
        CL_DEVICE_IMAGE2D_MAX_WIDTH                 
        CL_DEVICE_IMAGE2D_MAX_HEIGHT                
        CL_DEVICE_IMAGE3D_MAX_WIDTH                 
        CL_DEVICE_IMAGE3D_MAX_HEIGHT                
        CL_DEVICE_IMAGE3D_MAX_DEPTH                 
        CL_DEVICE_IMAGE_SUPPORT                     
        CL_DEVICE_MAX_PARAMETER_SIZE                
        CL_DEVICE_MAX_SAMPLERS                      
        CL_DEVICE_MEM_BASE_ADDR_ALIGN               
        CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE          
        CL_DEVICE_SINGLE_FP_CONFIG                  
        CL_DEVICE_GLOBAL_MEM_CACHE_TYPE             
        CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE         
        CL_DEVICE_GLOBAL_MEM_CACHE_SIZE             
        CL_DEVICE_GLOBAL_MEM_SIZE                   
        CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE          
        CL_DEVICE_MAX_CONSTANT_ARGS                 
        CL_DEVICE_LOCAL_MEM_TYPE                    
        CL_DEVICE_LOCAL_MEM_SIZE                    
        CL_DEVICE_ERROR_CORRECTION_SUPPORT          
        CL_DEVICE_PROFILING_TIMER_RESOLUTION        
        CL_DEVICE_ENDIAN_LITTLE                     
        CL_DEVICE_AVAILABLE                         
        CL_DEVICE_COMPILER_AVAILABLE                
        CL_DEVICE_EXECUTION_CAPABILITIES            
        CL_DEVICE_QUEUE_PROPERTIES                  
        CL_DEVICE_NAME                              
        CL_DEVICE_VENDOR                            
        CL_DRIVER_VERSION                           
        CL_DEVICE_PROFILE                           
        CL_DEVICE_VERSION                           
        CL_DEVICE_EXTENSIONS                        
        CL_DEVICE_PLATFORM                          
        CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF       
        CL_DEVICE_HOST_UNIFIED_MEMORY               
        CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR          
        CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT         
        CL_DEVICE_NATIVE_VECTOR_WIDTH_INT           
        CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG          
        CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT         
        CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE        
        CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF          
        CL_DEVICE_OPENCL_C_VERSION                  
        ))
(type cl_device_fp_config cl_bitfield
      (bits
        CL_FP_DENORM
        CL_FP_INF_NAN
        CL_FP_ROUND_TO_NEAREST
        CL_FP_ROUND_TO_ZERO
        CL_FP_ROUND_TO_INF
        CL_FP_FMA
        CL_FP_SOFT_FLOAT))

(type cl_device_mem_cache_type cl_uint
      (enum
        CL_NONE
        CL_READ_ONLY_CACHE
        CL_READ_WRITE_CACHE))
(type cl_device_local_mem_type cl_uint
      (enum
        CL_LOCAL
        CL_GLOBAL))
(type cl_device_exec_capabilities cl_bitfield
      (bits
        CL_EXEC_KERNEL
        CL_EXEC_NATIVE_KERNEL))
(type cl_command_queue_properties cl_bitfield
      (bits
        CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
        CL_QUEUE_PROFILING_ENABLE))

(type cl_context_properties intptr)
(type cl_context_info cl_uint
      (enum
        CL_CONTEXT_REFERENCE_COUNT
        CL_CONTEXT_DEVICES
        CL_CONTEXT_PROPERTIES
        CL_CONTEXT_NUM_DEVICES))

(type cl_command_queue_info cl_uint
      (enum
        CL_QUEUE_CONTEXT
        CL_QUEUE_DEVICE
        CL_QUEUE_REFERENCE_COUNT
        CL_QUEUE_PROPERTIES))

(type cl_channel_order cl_uint
      (enum
        CL_R                                        
        CL_A                                        
        CL_RG                                       
        CL_RA                                       
        CL_RGB                                      
        CL_RGBA                                     
        CL_BGRA                                     
        CL_ARGB                                     
        CL_INTENSITY                                
        CL_LUMINANCE                                
        CL_Rx                                       
        CL_RGx                                      
        CL_RGBx))

(type cl_channel_type cl_uint
      (enum
        CL_SNORM_INT8                               
        CL_SNORM_INT16                              
        CL_UNORM_INT8                               
        CL_UNORM_INT16                              
        CL_UNORM_SHORT_565                          
        CL_UNORM_SHORT_555                          
        CL_UNORM_INT_101010                         
        CL_SIGNED_INT8                              
        CL_SIGNED_INT16                             
        CL_SIGNED_INT32                             
        CL_UNSIGNED_INT8                            
        CL_UNSIGNED_INT16                           
        CL_UNSIGNED_INT32))

(type cl_mem_flags cl_bitfield
      (bits
        CL_MEM_READ_WRITE
        CL_MEM_WRITE_ONLY
        CL_MEM_READ_ONLY
        CL_MEM_USE_HOST_PTR
        CL_MEM_ALLOC_HOST_PTR
        CL_MEM_COPY_HOST_PTR))

(type cl_mem_object_type cl_uint
      (enum
        CL_MEM_OBJECT_BUFFER
        CL_MEM_OBJECT_IMAGE2D
        CL_MEM_OBJECT_IMAGE3D))

(type cl_mem_info cl_uint
      (enum
        CL_MEM_TYPE
        CL_MEM_FLAGS
        CL_MEM_SIZE
        CL_MEM_HOST_PTR
        CL_MEM_MAP_COUNT
        CL_MEM_REFERENCE_COUNT
        CL_MEM_CONTEXT
        CL_MEM_ASSOCIATED_MEMOBJECT
        CL_MEM_OFFSET))

(type cl_image_info cl_uint
      (enum
        CL_IMAGE_FORMAT
        CL_IMAGE_ELEMENT_SIZE
        CL_IMAGE_ROW_PITCH
        CL_IMAGE_SLICE_PITCH
        CL_IMAGE_WIDTH
        CL_IMAGE_HEIGHT
        CL_IMAGE_DEPTH))

(type cl_buffer_create_type cl_uint)
(type cl_addressing_mode cl_uint
      (enum
        CL_ADDRESS_NONE
        CL_ADDRESS_CLAMP_TO_EDGE
        CL_ADDRESS_CLAMP
        CL_ADDERSS_REPEAT
        CL_ADDRESS_MIRRORED_REPEAT))

(type cl_filter_mode cl_uint
      (enum
        CL_FILTER_NEAREST
        CL_FILTER_LINEAR))

(type cl_sampler_info cl_uint
      (enum
        CL_SAMPLER_REFERENCE_COUNT
        CL_SAMPLER_CONTEXT
        CL_SAMPLER_NORMALIZED_COORDS
        CL_SAMPLER_ADDRESSING_MODE
        CL_SAMPLER_FILTER_MODE))

(type cl_map_flags cl_bitfield
      (bits
        CL_MAP_READ
        CL_MAP_WRITE))

(type cl_program_info cl_uint
      (enum
        CL_PROGRAM_REFERENCE_COUNT
        CL_PROGRAM_CONTEXT
        CL_PROGRAM_NUM_DEVICES
        CL_PROGRAM_DEVICES
        CL_PROGRAM_SOURCE
        CL_PROGRAM_BINARY_SIZES
        CL_PROGRAM_BINARIES))

(type cl_program_build_info cl_uint
      (enum
        CL_PROGRAM_BUILD_STATUS
        CL_PROGRAM_BUILD_OPTIONS
        CL_PROGRAM_BUILD_LOG)) 
(type cl_build_status cl_int
      (enum
        CL_BUILD_SUCCESS
        CL_BUILD_NONE
        CL_BUILD_ERROR
        CL_BUILD_IN_PROGRESS))

(type cl_kernel_info cl_uint
      (enum
        CL_KERNEL_FUNCTION_NAME
        CL_KERNEL_NUM_ARGS
        CL_KERNEL_REFERENCE_COUNT
        CL_KERNEL_CONTEXT
        CL_KERNEL_PROGRAM))

(type cl_kernel_work_group_info cl_uint
      (enum
        CL_KERNEL_WORK_GROUP_SIZE
        CL_KERNEL_COMPILE_WORK_GROUP_SIZE
        CL_KERNEL_LOCAL_MEM_SIZE
        CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE
        CL_KERNEL_PRIVATE_MEM_SIZE))

(type cl_event_info cl_uint
      (enum
        CL_EVENT_COMMAND_QUEUE
        CL_EVENT_COMMAND_TYPE
        CL_EVENT_REFERENCE_COUNT
        CL_EVENT_COMMAND_EXECUTION_STATUS
        CL_EVENT_CONTEXT))

(type cl_command_type cl_uint
      (enum
        CL_COMMAND_NDRANGE_KERNEL                   
        CL_COMMAND_TASK                             
        CL_COMMAND_NATIVE_KERNEL                    
        CL_COMMAND_READ_BUFFER                      
        CL_COMMAND_WRITE_BUFFER                     
        CL_COMMAND_COPY_BUFFER                      
        CL_COMMAND_READ_IMAGE                       
        CL_COMMAND_WRITE_IMAGE                      
        CL_COMMAND_COPY_IMAGE                       
        CL_COMMAND_COPY_IMAGE_TO_BUFFER             
        CL_COMMAND_COPY_BUFFER_TO_IMAGE             
        CL_COMMAND_MAP_BUFFER                       
        CL_COMMAND_MAP_IMAGE                        
        CL_COMMAND_UNMAP_MEM_OBJECT                 
        CL_COMMAND_MARKER                           
        CL_COMMAND_ACQUIRE_GL_OBJECTS               
        CL_COMMAND_RELEASE_GL_OBJECTS               
        CL_COMMAND_READ_BUFFER_RECT                 
        CL_COMMAND_WRITE_BUFFER_RECT                
        CL_COMMAND_COPY_BUFFER_RECT                 
        CL_COMMAND_USER))

(type cl_profiling_info cl_uint
      (enum
        CL_PROFILING_COMMAND_QUEUED
        CL_PROFILING_COMMAND_SUBMIT
        CL_PROFILING_COMMAND_START
        CL_PROFILING_COMMAND_END))

(struct/type cl_image_format ((image_channel_order cl_channel_order)
                              (image_channel_data_type cl_channel_type)))

(struct/type cl_buffer_region ((origin size_t)
                               (size size_t)))

(type cl_int_error cl_int internal
      (enum
        CL_SUCCESS                                  
        CL_DEVICE_NOT_FOUND                         
        CL_DEVICE_NOT_AVAILABLE                     
        CL_COMPILER_NOT_AVAILABLE                   
        CL_MEM_OBJECT_ALLOCATION_FAILURE            
        CL_OUT_OF_RESOURCES                         
        CL_OUT_OF_HOST_MEMORY                       
        CL_PROFILING_INFO_NOT_AVAILABLE             
        CL_MEM_COPY_OVERLAP                         
        CL_IMAGE_FORMAT_MISMATCH                    
        CL_IMAGE_FORMAT_NOT_SUPPORTED               
        CL_BUILD_PROGRAM_FAILURE                    
        CL_MAP_FAILURE                              
        CL_MISALIGNED_SUB_BUFFER_OFFSET             
        CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST
        CL_INVALID_DEVICE_TYPE                      
        CL_INVALID_PLATFORM                         
        CL_INVALID_DEVICE                           
        CL_INVALID_CONTEXT                          
        CL_INVALID_QUEUE_PROPERTIES                 
        CL_INVALID_COMMAND_QUEUE                    
        CL_INVALID_HOST_PTR                         
        CL_INVALID_MEM_OBJECT                       
        CL_INVALID_IMAGE_FORMAT_DESCRIPTOR          
        CL_INVALID_IMAGE_SIZE                       
        CL_INVALID_SAMPLER                          
        CL_INVALID_BINARY                           
        CL_INVALID_BUILD_OPTIONS                    
        CL_INVALID_PROGRAM                          
        CL_INVALID_PROGRAM_EXECUTABLE               
        CL_INVALID_KERNEL_NAME                      
        CL_INVALID_KERNEL_DEFINITION                
        CL_INVALID_KERNEL                           
        CL_INVALID_ARG_INDEX                        
        CL_INVALID_ARG_VALUE                        
        CL_INVALID_ARG_SIZE                         
        CL_INVALID_KERNEL_ARGS                      
        CL_INVALID_WORK_DIMENSION                   
        CL_INVALID_WORK_GROUP_SIZE                  
        CL_INVALID_WORK_ITEM_SIZE                   
        CL_INVALID_GLOBAL_OFFSET                    
        CL_INVALID_EVENT_WAIT_LIST                  
        CL_INVALID_EVENT                            
        CL_INVALID_OPERATION                        
        CL_INVALID_GL_OBJECT                 
        CL_INVALID_BUFFER_SIZE                      
        CL_INVALID_MIP_LEVEL                        
        CL_INVALID_GLOBAL_WORK_SIZE                 
        CL_INVALID_PROPERTY))

(constant/signed
  CL_SUCCESS                                  
  CL_DEVICE_NOT_FOUND                         
  CL_DEVICE_NOT_AVAILABLE                     
  CL_COMPILER_NOT_AVAILABLE                   
  CL_MEM_OBJECT_ALLOCATION_FAILURE            
  CL_OUT_OF_RESOURCES                         
  CL_OUT_OF_HOST_MEMORY                       
  CL_PROFILING_INFO_NOT_AVAILABLE             
  CL_MEM_COPY_OVERLAP                         
  CL_IMAGE_FORMAT_MISMATCH                    
  CL_IMAGE_FORMAT_NOT_SUPPORTED               
  CL_BUILD_PROGRAM_FAILURE                    
  CL_MAP_FAILURE                              
  CL_MISALIGNED_SUB_BUFFER_OFFSET             
  CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST
  CL_INVALID_DEVICE_TYPE                      
  CL_INVALID_PLATFORM                         
  CL_INVALID_DEVICE                           
  CL_INVALID_CONTEXT                          
  CL_INVALID_QUEUE_PROPERTIES                 
  CL_INVALID_COMMAND_QUEUE                    
  CL_INVALID_HOST_PTR                         
  CL_INVALID_MEM_OBJECT                       
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR          
  CL_INVALID_IMAGE_SIZE                       
  CL_INVALID_SAMPLER                          
  CL_INVALID_BINARY                           
  CL_INVALID_BUILD_OPTIONS                    
  CL_INVALID_PROGRAM                          
  CL_INVALID_PROGRAM_EXECUTABLE               
  CL_INVALID_KERNEL_NAME                      
  CL_INVALID_KERNEL_DEFINITION                
  CL_INVALID_KERNEL                           
  CL_INVALID_ARG_INDEX                        
  CL_INVALID_ARG_VALUE                        
  CL_INVALID_ARG_SIZE                         
  CL_INVALID_KERNEL_ARGS                      
  CL_INVALID_WORK_DIMENSION                   
  CL_INVALID_WORK_GROUP_SIZE                  
  CL_INVALID_WORK_ITEM_SIZE                   
  CL_INVALID_GLOBAL_OFFSET                    
  CL_INVALID_EVENT_WAIT_LIST                  
  CL_INVALID_EVENT                            
  CL_INVALID_OPERATION                        
  CL_INVALID_GL_OBJECT                 
  CL_INVALID_BUFFER_SIZE                      
  CL_INVALID_MIP_LEVEL                        
  CL_INVALID_GLOBAL_WORK_SIZE                 
  CL_INVALID_PROPERTY                         
  CL_VERSION_1_0                              
  CL_VERSION_1_1                              
  CL_FALSE                                    
  CL_TRUE                                     
  CL_BLOCKING                                 
  CL_NON_BLOCKING                             
  CL_PLATFORM_PROFILE                         
  CL_PLATFORM_VERSION                         
  CL_PLATFORM_NAME                            
  CL_PLATFORM_VENDOR                          
  CL_PLATFORM_EXTENSIONS                      
  CL_DEVICE_TYPE_DEFAULT                      
  CL_DEVICE_TYPE_CPU                          
  CL_DEVICE_TYPE_GPU                          
  CL_DEVICE_TYPE_ACCELERATOR                  
  CL_DEVICE_TYPE_ALL                          
  CL_DEVICE_TYPE                              
  CL_DEVICE_VENDOR_ID                         
  CL_DEVICE_MAX_COMPUTE_UNITS                 
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS          
  CL_DEVICE_MAX_WORK_GROUP_SIZE               
  CL_DEVICE_MAX_WORK_ITEM_SIZES               
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR       
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT      
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT        
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG       
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT      
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE     
  CL_DEVICE_MAX_CLOCK_FREQUENCY               
  CL_DEVICE_ADDRESS_BITS                      
  CL_DEVICE_MAX_READ_IMAGE_ARGS               
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS              
  CL_DEVICE_MAX_MEM_ALLOC_SIZE                
  CL_DEVICE_IMAGE2D_MAX_WIDTH                 
  CL_DEVICE_IMAGE2D_MAX_HEIGHT                
  CL_DEVICE_IMAGE3D_MAX_WIDTH                 
  CL_DEVICE_IMAGE3D_MAX_HEIGHT                
  CL_DEVICE_IMAGE3D_MAX_DEPTH                 
  CL_DEVICE_IMAGE_SUPPORT                     
  CL_DEVICE_MAX_PARAMETER_SIZE                
  CL_DEVICE_MAX_SAMPLERS                      
  CL_DEVICE_MEM_BASE_ADDR_ALIGN               
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE          
  CL_DEVICE_SINGLE_FP_CONFIG                  
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE             
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE         
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE             
  CL_DEVICE_GLOBAL_MEM_SIZE                   
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE          
  CL_DEVICE_MAX_CONSTANT_ARGS                 
  CL_DEVICE_LOCAL_MEM_TYPE                    
  CL_DEVICE_LOCAL_MEM_SIZE                    
  CL_DEVICE_ERROR_CORRECTION_SUPPORT          
  CL_DEVICE_PROFILING_TIMER_RESOLUTION        
  CL_DEVICE_ENDIAN_LITTLE                     
  CL_DEVICE_AVAILABLE                         
  CL_DEVICE_COMPILER_AVAILABLE                
  CL_DEVICE_EXECUTION_CAPABILITIES            
  CL_DEVICE_QUEUE_PROPERTIES                  
  CL_DEVICE_NAME                              
  CL_DEVICE_VENDOR                            
  CL_DRIVER_VERSION                           
  CL_DEVICE_PROFILE                           
  CL_DEVICE_VERSION                           
  CL_DEVICE_EXTENSIONS                        
  CL_DEVICE_PLATFORM                          
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF       
  CL_DEVICE_HOST_UNIFIED_MEMORY               
  CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR          
  CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT         
  CL_DEVICE_NATIVE_VECTOR_WIDTH_INT           
  CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG          
  CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT         
  CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE        
  CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF          
  CL_DEVICE_OPENCL_C_VERSION                  
  CL_FP_DENORM                                
  CL_FP_INF_NAN                               
  CL_FP_ROUND_TO_NEAREST                      
  CL_FP_ROUND_TO_ZERO                         
  CL_FP_ROUND_TO_INF                          
  CL_FP_FMA                                   
  CL_FP_SOFT_FLOAT                            
  CL_NONE                                     
  CL_READ_ONLY_CACHE                          
  CL_READ_WRITE_CACHE                         
  CL_LOCAL                                    
  CL_GLOBAL                                   
  CL_EXEC_KERNEL                              
  CL_EXEC_NATIVE_KERNEL                       
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE      
  CL_QUEUE_PROFILING_ENABLE                   
  CL_CONTEXT_REFERENCE_COUNT                  
  CL_CONTEXT_DEVICES                          
  CL_CONTEXT_PROPERTIES                       
  CL_CONTEXT_NUM_DEVICES                      
  CL_CONTEXT_PLATFORM                         
  CL_QUEUE_CONTEXT                            
  CL_QUEUE_DEVICE                             
  CL_QUEUE_REFERENCE_COUNT                    
  CL_QUEUE_PROPERTIES                         
  CL_MEM_READ_WRITE                           
  CL_MEM_WRITE_ONLY                           
  CL_MEM_READ_ONLY                            
  CL_MEM_USE_HOST_PTR                         
  CL_MEM_ALLOC_HOST_PTR                       
  CL_MEM_COPY_HOST_PTR                        
  CL_KERNEL_WORK_GROUP_SIZE                   
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE           
  CL_KERNEL_LOCAL_MEM_SIZE                    
  CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE
  CL_KERNEL_PRIVATE_MEM_SIZE                  
  CL_EVENT_COMMAND_QUEUE                      
  CL_EVENT_COMMAND_TYPE                       
  CL_EVENT_REFERENCE_COUNT                    
  CL_EVENT_COMMAND_EXECUTION_STATUS           
  CL_EVENT_CONTEXT                            
  CL_COMMAND_NDRANGE_KERNEL                   
  CL_COMMAND_TASK                             
  CL_COMMAND_NATIVE_KERNEL                    
  CL_COMMAND_READ_BUFFER                      
  CL_COMMAND_WRITE_BUFFER                     
  CL_COMMAND_COPY_BUFFER                      
  CL_COMMAND_READ_IMAGE                       
  CL_COMMAND_WRITE_IMAGE                      
  CL_COMMAND_COPY_IMAGE                       
  CL_COMMAND_COPY_IMAGE_TO_BUFFER             
  CL_COMMAND_COPY_BUFFER_TO_IMAGE             
  CL_COMMAND_MAP_BUFFER                       
  CL_COMMAND_MAP_IMAGE                        
  CL_COMMAND_UNMAP_MEM_OBJECT                 
  CL_COMMAND_MARKER                           
  CL_COMMAND_ACQUIRE_GL_OBJECTS               
  CL_COMMAND_RELEASE_GL_OBJECTS               
  CL_COMMAND_READ_BUFFER_RECT                 
  CL_COMMAND_WRITE_BUFFER_RECT                
  CL_COMMAND_COPY_BUFFER_RECT                 
  CL_COMMAND_USER                             
  CL_COMPLETE                                 
  CL_RUNNING                                  
  CL_SUBMITTED                                
  CL_QUEUED                                   
  CL_BUFFER_CREATE_TYPE_REGION                
  CL_PROFILING_COMMAND_QUEUED                 
  CL_PROFILING_COMMAND_SUBMIT                 
  CL_PROFILING_COMMAND_START                  
  CL_PROFILING_COMMAND_END                    
  CL_ADDRESS_NONE                             
  CL_ADDRESS_CLAMP_TO_EDGE                    
  CL_ADDRESS_CLAMP                            
  CL_ADDRESS_REPEAT                           
  CL_ADDRESS_MIRRORED_REPEAT                  
  CL_FILTER_NEAREST                           
  CL_FILTER_LINEAR                            
  CL_SAMPLER_REFERENCE_COUNT                  
  CL_SAMPLER_CONTEXT                          
  CL_SAMPLER_NORMALIZED_COORDS                
  CL_SAMPLER_ADDRESSING_MODE                  
  CL_SAMPLER_FILTER_MODE                      
  CL_MAP_READ                                 
  CL_MAP_WRITE                                
  CL_PROGRAM_REFERENCE_COUNT                  
  CL_PROGRAM_CONTEXT                          
  CL_PROGRAM_NUM_DEVICES                      
  CL_PROGRAM_DEVICES                          
  CL_PROGRAM_SOURCE                           
  CL_PROGRAM_BINARY_SIZES                     
  CL_PROGRAM_BINARIES                         
  CL_PROGRAM_BUILD_STATUS                     
  CL_PROGRAM_BUILD_OPTIONS                    
  CL_PROGRAM_BUILD_LOG                        
  CL_BUILD_SUCCESS                            
  CL_BUILD_NONE                               
  CL_BUILD_ERROR                              
  CL_BUILD_IN_PROGRESS                        
  CL_KERNEL_FUNCTION_NAME                     
  CL_KERNEL_NUM_ARGS                          
  CL_KERNEL_REFERENCE_COUNT                   
  CL_KERNEL_CONTEXT                           
  CL_KERNEL_PROGRAM                           
  CL_R                                        
  CL_A                                        
  CL_RG                                       
  CL_RA                                       
  CL_RGB                                      
  CL_RGBA                                     
  CL_BGRA                                     
  CL_ARGB                                     
  CL_INTENSITY                                
  CL_LUMINANCE                                
  CL_Rx                                       
  CL_RGx                                      
  CL_RGBx                                     
  CL_SNORM_INT8                               
  CL_SNORM_INT16                              
  CL_UNORM_INT8                               
  CL_UNORM_INT16                              
  CL_UNORM_SHORT_565                          
  CL_UNORM_SHORT_555                          
  CL_UNORM_INT_101010                         
  CL_SIGNED_INT8                              
  CL_SIGNED_INT16                             
  CL_SIGNED_INT32                             
  CL_UNSIGNED_INT8                            
  CL_UNSIGNED_INT16                           
  CL_UNSIGNED_INT32                           
  CL_HALF_FLOAT                               
  CL_FLOAT                                    
  CL_MEM_OBJECT_BUFFER                        
  CL_MEM_OBJECT_IMAGE2D                       
  CL_MEM_OBJECT_IMAGE3D                       
  CL_MEM_TYPE                                 
  CL_MEM_FLAGS                                
  CL_MEM_SIZE                                 
  CL_MEM_HOST_PTR                             
  CL_MEM_MAP_COUNT                            
  CL_MEM_REFERENCE_COUNT                      
  CL_MEM_CONTEXT                              
  CL_MEM_ASSOCIATED_MEMOBJECT                 
  CL_MEM_OFFSET                               
  CL_IMAGE_FORMAT                             
  CL_IMAGE_ELEMENT_SIZE                       
  CL_IMAGE_ROW_PITCH                          
  CL_IMAGE_SLICE_PITCH                        
  CL_IMAGE_WIDTH                              
  CL_IMAGE_HEIGHT                             
  CL_IMAGE_DEPTH                              
  )

(func clCreateBuffer
      cl_mem
      ((context cl_context) 
       (flags cl_mem_flags) 
       (size size_t (length/byte host_ptr)) 
       (host_ptr void*) 
       (errcode_ret (box cl_int_error))))

;; clCreateSubBuffer 1.1

(func clCreateImage2D
      cl_mem
      ((context cl_context)
       (flags cl_mem_flags)
       (image_format cl_image_format* const)
       (image_width size_t)
       (image_height size_t)
       (image_row_pitch size_t)
       (host_ptr void*)
       (errcode_ret (box cl_int_error))))

(func clCreateImage3D
      cl_mem
      ((context cl_context)
       (flags cl_mem_flags)
       (image_format cl_image_format* const)
       (image_width size_t)
       (image_height size_t)
       (image_depth size_t)
       (image_row_pitch size_t)
       (image_slice_pitch size_t)
       (host_ptr void*)
       (errcode_ret (box cl_int_error))))

(func clRetainMemObject
      cl_int_error
      ((memobj cl_mem)))
(func clReleaseMemObject
      cl_int_error
      ((memobj cl_mem)))

(func clGetMemObjectInfo
      cl_int_error
      ((memobj cl_mem)
       (param_name cl_mem_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clGetImageInfo
      cl_int_error
      ((image cl_mem)
       (param_name cl_image_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

;; clSetMemObjectDestructorCallback 1.1

(func clCreateSampler
      cl_sampler
      ((context cl_context)
       (normalized_coords cl_bool)
       (addressing_mode cl_addressing_mode)
       (filter_mode cl_filter_mode)
       (errcode_ret (box cl_int_error))))

(func clRetainSampler
      cl_int_error
      ((sampler cl_sampler)))

(func clReleaseSampler
      cl_int_error
      ((sampler cl_sampler)))

(func clGetSamplerInfo
      cl_int_error
      ((sampler cl_sampler)
       (param_name cl_sampler_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clCreateProgramWithSource
      cl_program
      ((context cl_context)
       (count cl_uint (count strings lengths))
       (strings (* char*) const)
       (lengths (* size_t (count strings)) const)
       (errorcode_ret (box cl_int_error))))

(func clCreateProgramWithBinary
      cl_program
      ((context cl_context)
       (num_devices cl_uint (count device_list lengths binaries binary_status))
       (device_list (* cl_device_id) const)
       (lengths (* size_t) const)
       (binaries (* uchar*) const)
       (binary_status (* (box cl_int)))
       (errcode_ret (box cl_int_error))))

(func clRetainProgram
      cl_int_error
      ((program cl_program)))

(func clReleaseProgram
      cl_int_error
      ((program cl_program)))

(func clBuildProgram
      cl_int_error
      ((program cl_program)
       (num_devices cl_uint (count device_list))
       (device_list (* cl_device_id) const)
       (options char* const)
       (pfn_notify (func void ((program cl_program) (user_data void*))))
       (user_data void*)))

#|
(func clUnloadCompiler
      cl_int) ;; Deprecated
|#

(func clGetProgramInfo
      cl_int_error
      ((program cl_program)
       (param_name cl_program_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clGetProgramBuildInfo
      cl_int_error
      ((program cl_program)
       (device cl_device_id)
       (param_name cl_program_build_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clCreateKernel
      cl_kernel
      ((program cl_program)
       (kernel_name char* const)
       (errorcode_ret (box cl_int_error))))

(func clCreateKernelsInProgram
      cl_int_error
      ((program cl_program)
       (num_kernels cl_uint (count kernels))
       (kernels (* cl_kernel))
       (num_kernels_ret (box cl_uint))))

(func clRetainKernel
      cl_int_error
      ((kernel cl_kernel)))

(func clReleaseKernel
      cl_int_error
      ((kernel cl_kernel)))

(func clSetKernelArg
      cl_int_error
      ((kernel cl_kernel)
       (arg_index cl_uint)
       (arg_size size_t (lenth/byte arg_value))
       (arg_value void* const)))

(func clGetKernelInfo
      cl_int_error
      ((kernel cl_kernel)
       (param_name cl_kernel_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clGetKernelWorkGroupInfo
      cl_int_error
      ((kernel cl_kernel)
       (device cl_device_id)
       (param_name cl_kernel_work_group_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clWaitForEvents
      cl_int_error
      ((num_events cl_uint (count event_list))
       (event_list (* cl_event) const)))

(func clGetEventInfo
      cl_int_error
      ((event cl_event)
       (param_name cl_event_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

;; clCreateUserEvent 1.1

(func clRetainEvent
      cl_int_error
      ((event cl_event)))

(func clReleaseEvent
      cl_int_error
      ((event cl_event)))

;; clSetUserEventStatus
;; clSetEventCallback

(func clGetEventProfilingInfo
      cl_int_error
      ((event cl_event)
       (param_name cl_profiling_info)
       (param_value_size size_t (length/byte param_value))
       (param_value void*)
       (param_value_size_ret (box size_t))))

(func clFlush
      cl_int_error
      ((command_queue cl_command_queue)))

(func clFinish
      cl_int_error
      ((command_queue cl_command_queue)))

(func clEnqueueReadBuffer
      cl_int_error
      ((command_queue cl_command_queue)
       (buffer cl_mem)
       (blocking_read cl_bool_blocking)
       (offset size_t)
       (cb size_t)
       (ptr void*)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueWriteBuffer
      cl_int_error
      ((command_queue cl_command_queue)
       (buffer cl_mem)
       (blocking_write cl_bool_blocking)
       (offset size_t)
       (cb size_t)
       (ptr void* const)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueCopyBuffer
      cl_int_error
      ((command_queue cl_command_queue)
       (src_buffer cl_mem)
       (dst_buffer cl_mem)
       (src_offset size_t)
       (dst_offset size_t)
       (cb size_t)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

;; clEnqueueCopyBufferRect

(func clEnqueueReadImage
      cl_int_error
      ((command_queue cl_command_queue)
       (image cl_mem)
       (blocking_read cl_bool_blocking)
       (origin (* size_t) const)
       (region (* size_t) const)
       (row_pitch size_t)
       (slice_pitch size_t)
       (ptr void*)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueWriteImage
      cl_int_error
      ((command_queue cl_command_queue)
       (image cl_mem)
       (blocking_write cl_bool_blocking)
       (origin (* size_t) const (count 3))
       (region (* size_t) const (count 3))
       (input_row_pitch size_t)
       (input_slice_pitch size_t)
       (ptr void*)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueCopyImage
      cl_int_error
      ((command_queue cl_command_queue)
       (src_image cl_mem)
       (dst_image cl_mem)
       (src_origin (* size_t) const)
       (dst_origin (* size_t) const)
       (region (* size_t) const)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueCopyImageToBuffer
      cl_int_error
      ((command_queue cl_command_queue)
       (src_image cl_mem)
       (dst_buffer cl_mem)
       (src_origin (* size_t) const)
       (region (* size_t) const)
       (dst_offset size_t)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueCopyBufferToImage
      cl_int_error
      ((command_queue cl_command_queue)
       (src_buffer cl_mem)
       (dst_image cl_mem)
       (src_offset size_t)
       (dst_origin (* size_t) const)
       (region (* size_t) const)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueMapBuffer
      void*
      ((command_queue cl_command_queue)
       (buffer cl_mem) 
       (blocking_map cl_bool_blocking) 
       (map_flags cl_map_flags) 
       (offset size_t) 
       (cb size_t) 
       (num_events_in_wait_list cl_uint (count event_wait_list)) 
       (event_wait_list (* cl_event) const) 
       (event (box cl_event)) 
       (errorcode_ret (box cl_int_error))))

(func clEnqueueMapImage
      void*
      ((command_queue cl_command_queue)
       (image cl_mem)
       (blocking_map cl_bool_blocking)
       (map_flags cl_map_flags)
       (origin (* size_t) const)
       (region (* size_t) const)
       (image_row_pitch (box size_t))
       (image_slice_pitch (box size_t))
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))
       (errcode_ret (* cl_int_error))))

(func clEnqueueUnmapMemObject
      cl_int_error
      ((command_queue cl_command_queue)
       (memobj cl_mem)
       (mapped_ptr void*)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueNDRangeKernel
      cl_int_error
      ((command_queue cl_command_queue)
       (kernel cl_kernel)
       (work_dim cl_uint)
       (global_work_offset (* size_t) const) ;; MBZ: Reserved parameter
       (global_work_size (* size_t) const)
       (local_work_size (* size_t) const)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueTask
      cl_int_error
      ((command_queue cl_command_queue)
       (kernel cl_kernel)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event))))

(func clEnqueueNativeKernel
      cl_int_error
      ((command_queue cl_command_queue)
       (user_func (func void ((args void*))))
       (args void*)
       (cb_args size_t)
       (num_mem_objects cl_uint (count mem_list))
       (mem_list (* cl_mem) const)
       (args_mem_loc (* void*) const)
       (num_events_in_wait_list cl_uint (count event_wait_list))
       (event_wait_list (* cl_event) const)
       (event (box cl_event)))) 

(func clEnqueueMarker
      cl_int_error
      ((command_queue cl_command_queue)
       (event (box cl_event))))

(func clEnqueueWaitForEvents
      cl_int_error
      ((command_queue cl_command_queue)
       (num_events cl_uint (count event_list))
       (event_list (* cl_event) const))) 

(func clEnqueueBarrier
      cl_int_error
      ((command_queue cl_command_queue)))

(func clGetExtensionFunctionAddress
      void*
      ((func_name char* const)))

)) 
