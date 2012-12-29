set(CMAKE_LEGACY_CYGWIN_WIN32 0) # Remove when CMake >= 2.8.4 is required
cmake_minimum_required(VERSION 2.8)

# FIXME: trick
if(APPLE)
    # Enforce Clang for XCode
    set(CMAKE_XCODE_ATTRIBUTE_GCC_VERSION com.apple.compilers.llvm.clang.1_0)
    # Enforce Clang for Makefiles/Ninja
    if(NOT CMAKE_C_COMPILER)
        set(CMAKE_C_COMPILER "/usr/bin/clang" CACHE PATH
            "Enfoced by mosh")
    endif()
    if(NOT CMAKE_CXX_COMPILER)
        set(CMAKE_CXX_COMPILER "/usr/bin/clang++" CACHE PATH
            "Enforced by mosh")
    endif()
endif()

# Detect Win64 or other 64bit
if(NOT CMAKE_SIZEOF_VOID_P)
    message(WARNING "CMAKE_SIZEOF_VOID_P was not defined!!")
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(MOSH64 1)
elseif(CMAKE_SIZEOF_VOID_P EQUAL 4)
    set(MOSH32 1)
else()
    message(WARNING "Unsupported pointer size!(${CMAKE_SIZEOF_VOID_P}")
endif()


