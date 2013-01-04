# Root toolchain file

# Make sure android.toolchain.cmake treated as a dependency of the build
# configuration
include("${CMAKE_CURRENT_LIST_DIR}/android.toolchain.cmake")

# Override libs output to libs.prebuilt. 
# FIXME: Remove this when we support MSVC
if(NOT _CMAKE_IN_TRY_COMPILE)
    set(LIBRARY_OUTPUT_PATH 
        "${LIBRARY_OUTPUT_PATH_ROOT}/libs.prebuilt/${ANDROID_NDK_ABI_NAME}" 
        CACHE PATH 
        "forced by nmosh configuration(Use LIBRARY_OUTPUT_PATH_ROOT instead)" 
        FORCE)
endif()
