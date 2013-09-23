# OpenCL
find_package( OpenCL )
if(OPENCL_FOUND)
    option(MOSHPLUGIN_OPENCL "build nmosh OpenCL plugin" ON)
else()
    option(MOSHPLUGIN_OPENCL "build nmosh OpenCL plugin" OFF)
endif()

if(MOSHPLUGIN_OPENCL)
    if(OPENCL_INCLUDE_DIRS)
        add_definitions(-DHAVE_CL)
        add_nmosh_plugin(mosh_opencl 
            LINK_LIBRARIES ${OPENCL_LIBRARIES}
            C_SOURCES mocl.c)
        # FIXME: Workaround for AMD APP SDK
        if(MINGW)
            set_source_files_properties(mocl.c
                PROPERTIES
                COMPILE_FLAGS "-I\"${OPENCL_INCLUDE_DIRS}\""
                )
        else()
            include_directories(AFTER ${OPENCL_INCLUDE_DIRS})
        endif()
    endif()
endif(MOSHPLUGIN_OPENCL)

