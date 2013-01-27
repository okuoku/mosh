# please check doc/README.CMake for instructions
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

cmake_minimum_required(VERSION 2.8)
PROJECT(mosh)
include(MoshGlobals)
include(NmoshGstSDK)
include(NmoshPluginHost)
include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckTypeSize)
ENABLE_TESTING()

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(MOSH_WITH_NMOSH ON) # Psyntax support was dropped
option(MOSH_PORTABLE "build portable version of mosh interpreter" OFF)
option(MOSH_PREFIXLESS "build prefix-less version of mosh interpreter" ON)
option(MOSH_PRELOAD_CORE "always use precompiled base library" OFF)
option(NMOSH_PRELOAD_EMBED "Embed preload image to interpreter" OFF)
if(WIN32)
    option(MOSH_WITH_NMOSH_GUI "build GUI version of mosh interpreter" OFF)
    set(MOSH_GUI_EXECUTABLE_NAME "nmoshw" 
        CACHE STRING "GUI interpreter executable name")
endif()
option(MOSH_DEBUG_VERSION "build Debug version of mosh interpreter" OFF)
option(MOSH_GC_PARALLEL_MARK "Boehm GC: Turn on parallel-marking" OFF)
if(NOT ANDROID)
    if(WIN32) # FIXME: Allow MOSH_BUILD_MODULE on other platform
        option(MOSH_BUILD_MODULE 
            "build module(.so/.dll) version of mosh interpreter" OFF)
    else()
        set(MOSH_BUILD_MODULE OFF)
    endif()
else()
    # Nmosh will be built as a module on ANDROID
    set(MOSH_BUILD_MODULE ON)
endif()

set(MOSH_EXECUTABLE_NAME "nmosh" CACHE STRING "Interpreter executable name")

if(APPLE)
    option(MOSH_BUILD_OSX_BUNDLE "Build GUI Version (OSX)" OFF)
endif()

# Configuration

# Plugins
if(NMOSH_PRELOAD_EMBED)
    add_definitions(-DNMOSH_PRELOAD_EMBED)
    include_directories(${CMAKE_CURRENT_BINARY_DIR})
endif()

nmosh_plugin_reset()
include_directories(src/nmosh/include)
macro(add_plugin nam dir)
    add_nmosh_plugin_directory(ON ${nam} ${dir})
endmacro(add_plugin)

add_plugin(mosh_sdl src/nmosh/ext/sdl)
add_plugin(mosh_opencl src/nmosh/ext/khronos/OpenCL)
add_plugin(mosh_curses src/nmosh/ext/curses)
add_plugin(mosh_gobj src/nmosh/ext/gobject)
add_plugin(mosh_cairo src/nmosh/ext/cairo)
add_plugin(mosh_usb src/nmosh/ext/libusb1)
add_plugin(mosh_groonga src/nmosh/ext/groonga)
add_plugin(mosh_wx src/nmosh/ext/wx)
if(WIN32)
    add_plugin(mosh_winmm src/nmosh/win32/plugins/mm)
endif()
if(ANDROID)
    add_plugin(android_stubs src/nmosh/android)
endif()


# MOSH Core

# only ONE runtime source will be allowed.
set(nmosh_runtime_src
src/nmosh_image.cpp)

set(TARGET_FLAVOR nmosh)
add_definitions(-DWITH_NMOSH_DEFAULTS)
set(mosh_runtime_src ${nmosh_runtime_src})

if(MOSH_PRELOAD_CORE)
    add_definitions(-DWITH_PRELOAD_CORE)
endif()

if(MOSH_PORTABLE)
    add_definitions(-DWITH_NMOSH_PORTABLE)
    if(NOT MOSH_PREFIXLESS)
        # silently enable prefix-less mode.
        message(STATUS "Prefix-less mode was silently enabled.")
        add_definitions(-DWITH_NMOSH_PREFIXLESS)
    endif()
endif()

if(MOSH_GC_PARALLEL_MARK)
    add_definitions(-DPARALLEL_MARK)
endif()

if(MOSH_DEBUG_VERSION)
    add_definitions(-DDEBUG_VERSION)
endif()

if(MOSH_PREFIXLESS)
    add_definitions(-DWITH_NMOSH_PREFIXLESS)
endif()

# external libraries
# MOSH_GMP_DIR is MSVC only

# sanity check

if(NOT EXISTS ${PROJECT_SOURCE_DIR}/${mosh_runtime_src})
    message(FATAL_ERROR "this ${TARET_FLAVOR} tree is not configured properly. Please install release version of mosh and run ./gen-git-build.sh")
endif()

set(MOSH_VERSION_MAJOR "0")
set(MOSH_VERSION_MINOR "2")
set(MOSH_VERSION_MICRO "8")
set(MOSH_VERSION 
    "${MOSH_VERSION_MAJOR}.${MOSH_VERSION_MINOR}.${MOSH_VERSION_MICRO}" 
    CACHE STRING "mosh release version string")
set(MOSH_NAME "mosh" CACHE STRING "mosh PACKAGE_NAME")

if(MOSH_PORTABLE)
    set(MOSH_LIB_PATH "/lib")
elseif(MOSH_PREFIXLESS)
    set(MOSH_LIB_PATH "../share/${MOSH_NAME}/${MOSH_VERSION}/lib")
else()
    set(MOSH_LIB_PATH 
        "${CMAKE_INSTALL_PREFIX}/share/${MOSH_NAME}/${MOSH_VERSION}/lib")
endif()

mark_as_advanced(MOSH_WITH_NMOSH 
    MOSH_VERSION MOSH_BUGREPORT MOSH_NAME MOSH_LIB_PATH MOSH_DEBUG_VERSION)

add_definitions(-DHAVE_CONFIG_H
    -DMOSH_LIB_PATH=\"${MOSH_LIB_PATH}\"
    -DPACKAGE=\"${MOSH_NAME}\"
#   -DPACKAGE_BUGREPORT=\"${MOSH_BUGREPORT}\" 
    -DPACKAGE_VERSION=\"${MOSH_VERSION}\"
    -DUSE_BOEHM_GC
    -DGC_THREADS
    -D_REENTRANT
    -DLARGE_CONFIG=1
    -DTHREAD_LOCAL_ALLOC=1
    -DHAS_X86_THREAD_STATE64___RAX=1
    -DATOMIC_UNCOLLECTABLE=1
    -DNO_EXECUTE_PERMISSION=1
    -DALL_INTERIOR_POINTERS=1
    -DGC_GCJ_SUPPORT=1
    -DJAVA_FINALIZATION=1
    -DUSE_I686_PREFETCH
    -DDONT_ADD_BYTE_AT_END=1
    )

if(NOT WIN32 AND NOT ANDROID)
    add_definitions(-DHANDLE_FORK=1)
endif()
if(ANDROID)
    # add_definitions(-DGC_ASSERTIONS)
    # add_definitions(-DGC_PRINT_VERBOSE_STATS)
    add_definitions(-DUSE_MMAP)
    add_definitions(-DUSE_MUNMAP)
    add_definitions(-DGC_FORCE_UNMAP_ON_GCOLLECT)
    add_definitions(-DPLATFORM_ANDROID)
    add_definitions(-DGC_ANDROID_LOG)

    # To prevent adding main static data
    add_definitions(-DGC_NOT_DLL) 
    add_definitions(-DUSE_GET_STACKBASE_FOR_MAIN) # FIXME: Why ??
    add_definitions(-DGC_DONT_REGISTER_MAIN_STATIC_DATA)

    # for linker.h
    include_directories(src/nmosh/android/include)
endif()

if(MOSH_BUILD_MODULE AND NOT ANDROID)
    # add_definitions(-DGC_DLL)
endif()

if(MOSH_BUILD_MODULE OR WIN32)
    add_definitions(-DWITH_MOSH_ALTMAIN)
endif()

# OS Settings
if(WIN32)
    add_definitions(
        -DGC_NOT_DLL
        -DWINVER=0x501
        -DONIG_EXTERN=extern
        )
    set(ARCH_INCLUDE "${PROJECT_SOURCE_DIR}/src/win/include"
        "${PROJECT_SOURCE_DIR}/src/win32")
    add_definitions(-DWITH_MOSH_ALTMAIN)
    add_definitions(-DMOSH_HOST_OS=\"win32\")
    add_definitions(-DLINE_FEED_CODE_CRLF=1)
    # we set ARCH_* here. (Win32 only. when at other O/S, these flags are
    # guessed.)
    if(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86") 
        if(MOSH64)
            # FFI not supported
        else()
            add_definitions(-DARCH_IA32)
        endif()
    endif()
    if(MSVC)
        add_definitions(-DHAVE_TR1_HASHES=1)
        add_definitions(-DHAVE_TR1_UNORDERED_MAP=1)
        add_definitions(-D_CRT_SECURE_NO_WARNINGS) #nasty
    else() # MayBE MinGW
        set(arch_libs ${arch_libs} shlwapi ws2_32)
        set(MOSH_ADDFLAGS 
            "-fwide-exec-charset=ucs-4le -static-libgcc -Wno-deprecated")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${MOSH_ADDFLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} ${MOSH_ADDFLAGS}")
        add_definitions(-DMOSH_MINGW32)
        add_definitions(-DHAVE_EXT_HASHES=1)
    endif()
elseif(APPLE)
    # do not set arch here 
    # As MacOS X 10.4 workaround, we do not enable TR1 hashes...
    # FIXME: Do we still need this?
    add_definitions(-DMOSH_HOST_OS=\"darwin\")
    add_definitions(-DLINE_FEED_CODE_LF=1)
    if(XCODE_VERSION)
        add_definitions(-DHAVE_EXT_HASHES=1)
        # to disable direct-threaded code in XCode 3.x
        # FIXME: Do we still need this?
        add_definitions(-DUSE_XCODE) 
    endif()
else() 
    # so it is UNIX
    # Assume GCC 4.x or later with enough recent libstdc++
    add_definitions(-DHAVE_TR1_HASHES=1)
    add_definitions(-DHAVE_TR1_UNORDERED_MAP=1)
    add_definitions(-DLINE_FEED_CODE_LF=1)
    if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        add_definitions(-DGTEST_HAS_PTHREAD=0) # for GC
        if(ANDROID)
            add_definitions(-DMOSH_HOST_OS=\"android\")
            set(arch_libs ${arch_libs} dl)
        else()
            add_definitions(-DMOSH_HOST_OS=\"linux\")
            add_definitions(-D_GNU_SOURCE) # FIXME: Assume glibc here..
            set(arch_libs ${arch_libs} pthread dl)
        endif()
    elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
        add_definitions(-DMOSH_HOST_OS=\"bsd\")
        set(arch_libs ${arch_libs} pthread)
    else()
        add_definitions(-DMOSH_HOST_OS=\"unknown\")
    endif()
endif()

# includes
CHECK_INCLUDE_FILE(alloca.h HAVE_ALLOCA_H)
CHECK_INCLUDE_FILE(fcntl.h HAVE_FCNTL_H)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)
CHECK_INCLUDE_FILE(stdbool.h HAVE_STDBOOL_H)
CHECK_INCLUDE_FILE(stdarg.h HAVE_STDARG_H)
CHECK_INCLUDE_FILE(stdint.h HAVE_STDINT_H)
CHECK_INCLUDE_FILE(stdlib.h HAVE_STDLIB_H)
CHECK_INCLUDE_FILE(string.h HAVE_STRING_H)
CHECK_INCLUDE_FILE(sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE(sys/time.h HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILE(sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE(sys/wait.h HAVE_SYS_WAIT_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(utmp.h HAVE_UTMP_H)

# functions
CHECK_FUNCTION_EXISTS(alloca HAVE_ALLOCA)
CHECK_FUNCTION_EXISTS(memmove HAVE_MEMMOVE)
CHECK_FUNCTION_EXISTS(memset HAVE_MEMSET)
CHECK_FUNCTION_EXISTS(strtol HAVE_STRTOL)
if(NOT WIN32) # It's waste of time in Win32!
CHECK_FUNCTION_EXISTS(gettimeofday HAVE_GETTIMEOFDAY)
CHECK_FUNCTION_EXISTS(posix_spawn HAVE_POSIX_SPAWN)
CHECK_FUNCTION_EXISTS(ptrace HAVE_PTRACE)
CHECK_FUNCTION_EXISTS(fcntl HAVE_FCNTL)
CHECK_FUNCTION_EXISTS(poll HAVE_POLL)
CHECK_FUNCTION_EXISTS(sigaction HAVE_SIGACTION)
CHECK_FUNCTION_EXISTS(vfork HAVE_VFORK)
CHECK_FUNCTION_EXISTS(socket HAVE_SOCKET)
endif()

# sizeofs
CHECK_TYPE_SIZE(int SIZEOF_INT)
CHECK_TYPE_SIZE(short SIZEOF_SHORT)
CHECK_TYPE_SIZE(long SIZEOF_LONG)
CHECK_TYPE_SIZE(longlong SIZEOF_LONG_LONG)
CHECK_TYPE_SIZE(int64_t SIZEOF___INT64)
CHECK_TYPE_SIZE(off_t SIZEOF_OFF_T)
CHECK_TYPE_SIZE(void* SIZEOF_VOIDP)
CHECK_TYPE_SIZE(float SIZEOF_FLOAT)
CHECK_TYPE_SIZE(double SIZEOF_DOUBLE)

if(MOSH64)
    if(WIN32)
        add_definitions(-D_WIN64)
    endif()
endif()

if(HAVE_STDARG_H)
    add_definitions(-DHAVE_STDARG_PROTOTYPES)
endif()

# GMP things
# => GMP_INCLUDE_DIR, GMP_LIBRARY

set(GMP_SEARCH true)
if(APPLE)
    # use GMP.framework
    # set(CMAKE_FRAMEWORK_PATH .)
elseif(WIN32)
    if(CMAKE_HOST_WIN32)
        if(MOSH_GMP_DIR)
            set(GMP_SEARCH false)
            set(MOSH_GMP_DIR CACHE PATH 
                "path to MPIR library (mpir-2.X.X/[lib|dll]/Win32/[Debug|Release])")
            set(GMP_INCLUDE_DIR ${MOSH_GMP_DIR})
            if(MSVC)
                set(GMP_LIBRARY ${MOSH_GMP_DIR}/mpir.lib)
            else()
                set(GMP_LIBRARY ${MOSH_GMP_DIR}/mpir.dll)
            endif()
        endif()
    endif()
endif()

if(GMP_SEARCH)
    find_path(GMP_INCLUDE_DIR gmp.h)
    find_library(GMP_LIBRARY NAMES gmp)

    if(GMP_INCLUDE_DIR AND GMP_LIBRARY)
        set(GMP_FOUND TRUE)
    endif(GMP_INCLUDE_DIR AND GMP_LIBRARY)

    if(GMP_FOUND)
        message(STATUS "using GMP from : ${GMP_INCLUDE_DIR}, ${GMP_LIBRARY}")
    else()
        message(SEND_ERROR "GMP not found..")
    endif()
else()
    # check gmp.h in GMP_INCLUDE_DIR
    if(NOT EXISTS ${GMP_INCLUDE_DIR}/gmp.h)
        message(SEND_ERROR "MPIR/GMP is not installed at ${GMP_INCLUDE_DIR}! (gmp.h could not found. please build MPIR(http://www.mpir.org) first and specify mpir-2.X.X/[lib|dll]/Win32/[Debug|Release] to MOSH_GMP_DIR.")
    endif()
endif()


# GC things
# => gc_srcs

set(gc_core_srcs
extlibs/gc/allchblk.c
extlibs/gc/alloc.c
extlibs/gc/blacklst.c
extlibs/gc/checksums.c
extlibs/gc/dbg_mlc.c
extlibs/gc/finalize.c
extlibs/gc/gc_dlopen.c
extlibs/gc/gcj_mlc.c
extlibs/gc/headers.c
extlibs/gc/malloc.c
extlibs/gc/mallocx.c
extlibs/gc/mark.c
extlibs/gc/mark_rts.c
extlibs/gc/gc_misc.c
extlibs/gc/new_hblk.c
extlibs/gc/obj_map.c
extlibs/gc/os_dep.c
extlibs/gc/mach_dep.c
extlibs/gc/pcr_interface.c
extlibs/gc/ptr_chck.c
extlibs/gc/real_malloc.c
extlibs/gc/reclaim.c
extlibs/gc/specific.c
extlibs/gc/stubborn.c
extlibs/gc/typd_mlc.c
extlibs/gc/backgraph.c
extlibs/gc/thread_local_alloc.c
)

set(gc_dyn_srcs
    extlibs/gc/dyn_load.c)

set(gc_pthread_srcs
    extlibs/gc/pthread_start.c
    extlibs/gc/pthread_support.c
    extlibs/gc/pthread_stop_world.c)
set(gc_darwin_srcs
    extlibs/gc/darwin_stop_world.c)
set(gc_win32_srcs
    extlibs/gc/win32_threads.c)

if(WIN32)
    set(gc_srcs
        ${gc_core_srcs}
        ${gc_dyn_srcs}
        ${gc_win32_srcs})
elseif(APPLE)
    set(gc_srcs
        ${gc_core_srcs}
        ${gc_dyn_srcs}
        ${gc_pthread_srcs}
        ${gc_darwin_srcs})
else() 
    # pthreads
    set(gc_srcs
        ${gc_core_srcs}
        ${gc_dyn_srcs}
        ${gc_pthread_srcs})
endif()

# Onigruma things

set(MOSH_ONIG_DIR extlibs/onig-5.9.2)
set(onig_srcs
${MOSH_ONIG_DIR}/regint.h
${MOSH_ONIG_DIR}/regparse.h
${MOSH_ONIG_DIR}/regenc.h
${MOSH_ONIG_DIR}/st.h
${MOSH_ONIG_DIR}/regerror.c
${MOSH_ONIG_DIR}/regparse.c
${MOSH_ONIG_DIR}/regext.c
${MOSH_ONIG_DIR}/regcomp.c
${MOSH_ONIG_DIR}/regexec.c
${MOSH_ONIG_DIR}/reggnu.c
${MOSH_ONIG_DIR}/regenc.c
${MOSH_ONIG_DIR}/regsyntax.c
${MOSH_ONIG_DIR}/regtrav.c
${MOSH_ONIG_DIR}/regversion.c
${MOSH_ONIG_DIR}/st.c
${MOSH_ONIG_DIR}/regposix.c
${MOSH_ONIG_DIR}/regposerr.c
${MOSH_ONIG_DIR}/enc/unicode.c
${MOSH_ONIG_DIR}/enc/ascii.c
${MOSH_ONIG_DIR}/enc/utf8.c
${MOSH_ONIG_DIR}/enc/utf16_be.c
${MOSH_ONIG_DIR}/enc/utf16_le.c
${MOSH_ONIG_DIR}/enc/utf32_be.c
${MOSH_ONIG_DIR}/enc/utf32_le.c
${MOSH_ONIG_DIR}/enc/euc_jp.c
${MOSH_ONIG_DIR}/enc/sjis.c
${MOSH_ONIG_DIR}/enc/iso8859_1.c
${MOSH_ONIG_DIR}/enc/iso8859_2.c
${MOSH_ONIG_DIR}/enc/iso8859_3.c
${MOSH_ONIG_DIR}/enc/iso8859_4.c
${MOSH_ONIG_DIR}/enc/iso8859_5.c
${MOSH_ONIG_DIR}/enc/iso8859_6.c
${MOSH_ONIG_DIR}/enc/iso8859_7.c
${MOSH_ONIG_DIR}/enc/iso8859_8.c
${MOSH_ONIG_DIR}/enc/iso8859_9.c
${MOSH_ONIG_DIR}/enc/iso8859_10.c
${MOSH_ONIG_DIR}/enc/iso8859_11.c
${MOSH_ONIG_DIR}/enc/iso8859_13.c
${MOSH_ONIG_DIR}/enc/iso8859_14.c
${MOSH_ONIG_DIR}/enc/iso8859_15.c
${MOSH_ONIG_DIR}/enc/iso8859_16.c
${MOSH_ONIG_DIR}/enc/euc_tw.c
${MOSH_ONIG_DIR}/enc/euc_kr.c
${MOSH_ONIG_DIR}/enc/big5.c
${MOSH_ONIG_DIR}/enc/gb18030.c
${MOSH_ONIG_DIR}/enc/koi8_r.c
${MOSH_ONIG_DIR}/enc/cp1251.c
)

# mosh

include_directories(${PROJECT_BINARY_DIR}/cmake 
    extlibs/gc/include 
    extlibs/libatomic_ops/src src 
    ${GMP_INCLUDE_DIR} 
    ${MOSH_ONIG_DIR} 
    ${ARCH_INCLUDE}
    src/nmosh/include)

if(XCODE)
    add_definitions(-DWITHOUT_FFI_STUB)
elseif(WIN32)
    if(MOSH64)
        message(STATUS 
            "FFI stub was disabled (build without call-back support)")
        add_definitions(-DWITHOUT_FFI_STUB)
    elseif(MINGW)
        set(ffi_stub cmake/ffi_stub.S)
        set_source_files_properties(${ffi_stub} PROPERTIES LANGUAGE C)
    endif()
else()
    set(ffi_stub cmake/ffi_stub.S)
    set_source_files_properties(${ffi_stub} PROPERTIES LANGUAGE C)
endif()

set(mosh_core_hdrs
src/Code.h
src/SharedReference.h
src/NonGenerativeRTDs.h
src/HeapObject.h
src/Object-inl.h
src/Object-accessors.h
src/Pair-inl.h
src/VM-inl.h
src/match.h
src/baselib.h
src/CProcedure.h
src/Instruction.h
src/Callable.h
src/GenericMap.h
src/BinaryInputPort.h
src/BinaryOutputPort.h
src/scheme.h
src/ByteVector.h
src/HashTable.h
src/Pair.h
src/ExecutableMemory.h
src/LineBufferedFileBinaryOutputPort.h
src/Stack.h
src/Gloc.h
src/BinaryInputOutputPort.h
src/Box.h
src/Port.h
src/BinaryPort.h
src/OutputPort.h
src/Codec.h
src/Transcoder.h
src/ProcedureMacro.h
src/EqvHashTable.h
src/Compnum.h
src/Fixnum.h
src/Closure.h
src/UtilityProcedures.h
src/ProcessProcedures.h
src/Reader.h
src/NumberReader.h
src/scheme.h
src/Regexp.h
src/Symbol.h
src/VM.h
src/VM-Run.h
src/Continuation.h
src/Scanner.h
src/NumberScanner.h
src/ByteArrayBinaryInputPort.h
src/ByteArrayBinaryOutputPort.h
src/ScannerHelper.h
src/FileBinaryInputPort.h
src/BufferedFileBinaryInputPort.h
src/FileBinaryOutputPort.h
src/BufferedFileBinaryOutputPort.h
src/CustomBinaryInputPort.h
src/CustomTextualInputPort.h
src/CustomTextualOutputPort.h
src/CustomTextualInputOutputPort.h
src/CustomBinaryInputOutputPort.h
src/TextualOutputPort.h
src/TextualInputPort.h
src/TranscodedTextualInputPort.h
src/TranscodedTextualOutputPort.h
src/StringTextualOutputPort.h
src/StringTextualInputPort.h
src/Codec.h
src/UTF8Codec.h
src/UTF16Codec.h
src/UTF32Codec.h
src/EqHashTable.h
src/SString.h
src/GenericHashTable.h
src/Object.h
src/CompilerProcedures.h
src/HashTableProcedures.h
src/RegexpProcedures.h
src/StringProcedures.h
src/PortProcedures.h
src/ListProcedures.h
src/ErrorProcedures.h
src/ArithmeticProcedures.h
src/FlonumProcedures.h
src/BitwiseProcedures.h
src/ByteVectorProcedures.h
src/SocketProcedures.h
src/Equivalent.h
src/ucs4string.h
src/Vector.h
src/Fasl.h
src/FaslReader.h
src/FaslWriter.h
src/Arithmetic.h
src/Ratnum.h
src/Bignum.h
src/Flonum.h
src/CodeBuilder.h
src/FixnumProcedures.h
src/Latin1Codec.h
src/FFI.h
src/FFIProcedures.h
src/CustomBinaryOutputPort.h
src/FileBinaryInputOutputPort.h
src/SocketBinaryInputOutputPort.h
src/TranscodedTextualInputOutputPort.h
src/BufferedFileBinaryInputOutputPort.h
src/TestingFileBinaryOutputPort.h
src/TestingFileBinaryInputPort.h
src/TestingVM.h
src/OSConstants.h
src/getoptU.h
src/StandardOutputPort.h
src/StandardErrorPort.h
src/StandardInputPort.h
src/BlockBufferedFileBinaryOutputPort.h
src/BlockBufferedFileBinaryInputOutputPort.h
src/LineBufferedFileBinaryInputOutputPort.h
src/MultiVMProcedures.h
src/OSCompatSocket.h
src/OSCompatThread.h
src/VMFactory.h
src/SimpleStruct.h
)

set(mosh_core_cxxsrcs
src/OSCompat.h
src/Transcoder.cpp
src/EqvHashTable.cpp
src/Compnum.cpp
src/Fixnum.cpp
src/Closure.cpp
src/UtilityProcedures.cpp
src/ProcessProcedures.cpp
src/Reader.cpp
src/NumberReader.cpp
src/scheme.cpp
src/Regexp.cpp
src/Symbol.cpp
src/VM.cpp
src/VM-Profiler.cpp
src/VM-Run.cpp
src/Scanner.cpp
src/NumberScanner.cpp
src/ByteArrayBinaryInputPort.cpp
src/ByteArrayBinaryOutputPort.cpp
src/ScannerHelper.cpp
src/Reader.tab.cpp
src/NumberReader.tab.cpp
src/FileBinaryInputPort.cpp
src/BufferedFileBinaryInputPort.cpp
src/FileBinaryOutputPort.cpp
src/BufferedFileBinaryOutputPort.cpp
src/CustomBinaryInputPort.cpp
src/CustomTextualInputPort.cpp
src/CustomTextualOutputPort.cpp
src/CustomTextualInputOutputPort.cpp
src/CustomBinaryInputOutputPort.cpp
src/TextualOutputPort.cpp
src/TextualInputPort.cpp
src/TranscodedTextualInputPort.cpp
src/TranscodedTextualOutputPort.cpp
src/StringTextualOutputPort.cpp
src/StringTextualInputPort.cpp
src/Codec.cpp
src/UTF8Codec.cpp
src/UTF16Codec.cpp
src/UTF32Codec.cpp
src/EqHashTable.cpp
src/SString.cpp
src/GenericHashTable.cpp
src/Object.cpp
src/CompilerProcedures.cpp
src/HashTableProcedures.cpp
src/RegexpProcedures.cpp
src/StringProcedures.cpp
src/PortProcedures.cpp
src/ListProcedures.cpp
src/ErrorProcedures.cpp
src/ArithmeticProcedures.cpp
src/FlonumProcedures.cpp
src/BitwiseProcedures.cpp
src/ByteVectorProcedures.cpp
src/SocketProcedures.cpp
src/Equivalent.cpp
src/ucs4string.cpp
src/Vector.cpp
src/FaslReader.cpp
src/FaslWriter.cpp
src/Arithmetic.cpp
src/Ratnum.cpp
src/Bignum.cpp
src/Flonum.cpp
src/CodeBuilder.cpp
src/FixnumProcedures.cpp
src/Latin1Codec.cpp
src/FFI.cpp
src/FFIProcedures.cpp
src/CustomBinaryOutputPort.cpp
src/FileBinaryInputOutputPort.cpp
src/SocketBinaryInputOutputPort.cpp
src/TranscodedTextualInputOutputPort.cpp
src/BufferedFileBinaryInputOutputPort.cpp
src/getoptU.cpp
src/MultiVMProcedures.cpp
src/OSCompatSocket.cpp
src/OSCompatThread.cpp
src/VMFactory.cpp
src/OSCompat.cpp
src/nmosh/mosh-stubs.cpp
src/nmosh/generic/boehmgc-stubs/boehmgc-stubs.c
)

# FIXME: Move to NmoshPluginUtil.cmake
add_plugin(moshvm_helper src/nmosh/helper)
bless_nmosh_plugin_stub_source(src/nmosh/mosh-stubs.cpp)

# FIXME: Move them to plugin
set(mosh_posix_srcs
src/nmosh/posix/terminal/terminal.c
src/nmosh/posix/spawn/posixspawn.c
src/nmosh/posix/environ/mosh-environ.c
src/nmosh/posix/fd/posix_fd.c
src/nmosh/posix/poll/posix_poll.c
src/nmosh/posix/socket/posix_socket.c
src/nmosh/posix/sigchld_handler/sigchld_handler.c
src/nmosh/posix/debugee/posix_debugee.c
src/nmosh/bsd/kqueue/kqueue_stubs.c
src/nmosh/posix/wait3/wait3.c
src/nmosh/posix/pthread/mosh_pthread.c
)

set(mosh_core_srcs ${mosh_core_cxxsrcs} ${ffi_stub})
if(NOT WIN32)
    list(APPEND mosh_core_srcs ${mosh_posix_srcs})
endif()

if(WIN32)
    set(mosh_win32_srcs
        src/nmosh/win32/process.c)
    source_group(win32 FILES ${mosh_win32_srcs})
    set(mosh_core_srcs 
        ${mosh_core_srcs}
        ${mosh_win32_srcs}
        src/win/win.cpp
        )
    # FIXME: Exclude tests?
    set_source_files_properties(${mosh_core_cxxsrcs} src/main.cpp 
        src/TestingSignalHandler.cpp src/TestingVM.cpp
        PROPERTIES COMPILE_FLAGS "-D_UNICODE -DUNICODE -DWIN32_LEAN_AND_MEAN")
endif()

configure_file(cmake/config-cmake.h.in
    ${PROJECT_BINARY_DIR}/cmake/config.h)

source_group(gc FILES ${gc_srcs})
source_group(oniguruma FILES ${onig_srcs})

# Generate executable
if(WIN32)
    if(WIN32)
        # FIXME: We should use configure_file
        file(COPY ${PROJECT_SOURCE_DIR}/misc/logo/mosh.ico DESTINATION 
            ${PROJECT_BINARY_DIR}/misc/logo)
    endif()
    if(MOSH_BUILD_MODULE)
        add_library(${MOSH_EXECUTABLE_NAME} SHARED
            ${onig_srcs} 
            ${gc_srcs} 
            ${mosh_core_srcs} 
            ${mosh_core_hdrs}
            ${mosh_runtime_src}
            src/main.cpp)
        add_executable(${MOSH_EXECUTABLE_NAME}-run
            src/win/mosh.rc
            src/nmosh/win32/cuimain.cpp)
        set_target_properties(${MOSH_EXECUTABLE_NAME}-run
            PROPERTIES
            OUTPUT_NAME ${MOSH_EXECUTABLE_NAME})
        target_link_libraries(${MOSH_EXECUTABLE_NAME}-run
            ${MOSH_EXECUTABLE_NAME})
    else()
        add_executable(${MOSH_EXECUTABLE_NAME}
            ${onig_srcs} 
            ${gc_srcs} 
            ${mosh_core_srcs} 
            ${mosh_core_hdrs}
            ${mosh_runtime_src}
            src/win/mosh.rc
            src/nmosh/win32/cuimain.cpp
            src/main.cpp)
    endif()
elseif(MOSH_BUILD_OSX_BUNDLE)
    # Mac OSX Bundle
    add_executable(${MOSH_EXECUTABLE_NAME} MACOSX_BUNDLE
        ${onig_srcs} 
        ${gc_srcs} 
        ${mosh_core_srcs} 
        ${mosh_core_hdrs}
        ${mosh_runtime_src}
        src/main.cpp)
elseif(NOT ANDROID)
    # FIXME: Implement MOSH_BUILD_MODULE
    add_executable(${MOSH_EXECUTABLE_NAME}
        ${onig_srcs} 
        ${gc_srcs} 
        ${mosh_core_srcs} 
        ${mosh_core_hdrs}
        ${mosh_runtime_src}
        src/main.cpp)

else() # Android
    add_library(${MOSH_EXECUTABLE_NAME} SHARED
        ${onig_srcs} 
        ${gc_srcs} 
        ${mosh_core_srcs} 
        ${mosh_core_hdrs}
        ${mosh_runtime_src}
        src/main.cpp)
endif()
target_link_nmosh_plugin(${MOSH_EXECUTABLE_NAME})
target_link_libraries(${MOSH_EXECUTABLE_NAME} ${GMP_LIBRARY} ${arch_libs})

if(MOSH_PORTABLE) # FIXME: Bogus in ANDROID
    install(TARGETS ${MOSH_EXECUTABLE_NAME} DESTINATION .)
else()
    install(TARGETS ${MOSH_EXECUTABLE_NAME} DESTINATION bin)
endif()

if(MOSH_WITH_NMOSH_GUI)
    # FIXME: Implement MOSH_BUILD_MODULE
    add_executable(${MOSH_GUI_EXECUTABLE_NAME} WIN32
        ${onig_srcs} 
        ${gc_srcs} 
        ${mosh_core_srcs} 
        ${mosh_core_hdrs}
        ${mosh_runtime_src}
        src/main.cpp
        src/nmosh/win32/winmain.cpp)
    target_link_nmosh_plugin(${MOSH_GUI_EXECUTABLE_NAME})
    target_link_libraries(${MOSH_GUI_EXECUTABLE_NAME} 
        ${GMP_LIBRARY} ${arch_libs})
    if(MOSH_PORTABLE)
        install(TARGETS ${MOSH_GUI_EXECUTABLE_NAME} DESTINATION .)
    else()
        install(TARGETS ${MOSH_GUI_EXECUTABLE_NAME} DESTINATION bin)
    endif()
endif()

# Generate test script
if(WIN32)
    nmosh_config_uninstalled(cmake/testmosh.bat.in 
        testmosh.bat
        @ONLY
        NEWLINE_STYLE DOS)
    if(EXISTS ${MOSH_GMP_DIR}/mpir.dll)
        nmosh_dist_uninstalled(${MOSH_GMP_DIR}/mpir.dll)
    elseif(HAVE_GST_SDK) # GST SDK case
        set(_nam ${GMP_INCLUDE_DIR}/../bin/libgmp-10.dll)
        if(EXISTS ${_nam})
            nmosh_dist_uninstalled(${_nam})
        endif()
    endif()
else()
    nmosh_config_uninstalled(cmake/testmosh.sh.in 
        testmosh.sh
        @ONLY)
endif()

# Generate turbocharge
set(_turbocharge_output "output.fasl") # dummy name
if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${_turbocharge_output})
    # Create dummy data
    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${_turbocharge_output}.inc.c
        "")
endif()

if(NOT CMAKE_CROSSCOMPILING)
    set(_turbocharge_host "HOST")
elseif(ANDROID)
    set(_turbocharge_host "android")
    set(_turbocharge_output "${LIBRARY_OUTPUT_PATH_ROOT}/assets/archive.fasl")
endif()
set(_loadpath "")
set(NMOSH_PACKAGE_LIBPATH CACHE PATH "Path to additional library directory for packaging")
set(NMOSH_PACKAGE_HOSTNMOSH CACHE PATH "Path to host nmosh")
if(EXISTS "${NMOSH_PACKAGE_HOSTNMOSH}")
    set(_host_nmosh ${NMOSH_PACKAGE_HOSTNMOSH})
    set(_use_host_nmosh "")
else()
    set(_use_host_nmosh "nonono")
endif()
if(NMOSH_PACKAGE_LIBPATH)
    list(APPEND _loadpath ${NMOSH_PACKAGE_LIBPATH})
endif()
list(APPEND _loadpath ${CMAKE_CURRENT_SOURCE_DIR})
if(_turbocharge_host)
    add_custom_command(OUTPUT ${_turbocharge_output}
        COMMAND ${CMAKE_COMMAND} 
        -DNMOSH${_use_host_nmosh}="${_host_nmosh}"
        -DLOADPATH="${_loadpath}"
        -DSRCPATH="${CMAKE_CURRENT_SOURCE_DIR}"
        -DTARGET=${_turbocharge_host}
        -DOUTPUT="${_turbocharge_output}"
        -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/scripts/NmoshCompileBytecode.cmake
        )
    add_custom_target(turbocharge
        DEPENDS ${_turbocharge_output})
endif()

# tests
add_custom_target(TestTargets) 

add_library(testinggc EXCLUDE_FROM_ALL
    ${gc_srcs})
add_library(testingmosh EXCLUDE_FROM_ALL
    ${mosh_core_srcs}
    ${PROJECT_SOURCE_DIR}/gtest/gtest/gtest-all.cc
    ${mosh_runtime_src} ${onig_srcs} 
    src/TestingSignalHandler.cpp
    src/TestingVM.cpp)

set_target_properties(testinggc PROPERTIES FOLDER Test)
set_target_properties(testingmosh PROPERTIES FOLDER Test)

# GC tests
macro(add_gcguitest testname)
    if(MSVC)
        add_executable(gctest${testname} EXCLUDE_FROM_ALL 
            WIN32 ${PROJECT_SOURCE_DIR}/extlibs/gc/tests/${testname}.c)
    else()
        add_executable(gctest${testname} EXCLUDE_FROM_ALL 
            ${PROJECT_SOURCE_DIR}/extlibs/gc/tests/${testname}.c)
    endif()
    target_link_libraries(gctest${testname} testinggc ${arch_libs})
    set_target_properties(gctest${testname} PROPERTIES FOLDER Test)
    add_test(gctest-${testname} gctest${testname})
    add_dependencies(TestTargets gctest${testname})
endmacro(add_gcguitest)

macro(add_gctest testname)
    add_executable(gctest${testname} EXCLUDE_FROM_ALL 
        ${PROJECT_SOURCE_DIR}/extlibs/gc/tests/${testname}.c)
    target_link_libraries(gctest${testname} testinggc ${arch_libs})
    set_target_properties(gctest${testname} PROPERTIES FOLDER Test)
    add_test(gctest-${testname} gctest${testname})
    add_dependencies(TestTargets gctest${testname})
endmacro(add_gctest)

add_gcguitest(test) # gctest is GUI app in win32.. some historical reason
add_gctest(leak_test)
add_gctest(middle)
add_gctest(smash_test)
add_gctest(huge_test)
#add_gctest(trace_test)
if(NOT WIN32)
    add_gctest(thread_leak_test)
endif()
#add_gctest(test_cpp.cc)

# mosh base tests
set(gtest_srcs ${PROJECT_SOURCE_DIR}/gtest/gtest/gtest_main.cc)

macro(add_moshcoretest testname)
    if(WIN32)
        set_source_files_properties(src/${testname} PROPERTIES COMPILE_FLAGS 
            "-D_UNICODE -DUNICODE -DWIN32_LEAN_AND_MEAN")
    endif()
    add_executable(moshtest${testname} EXCLUDE_FROM_ALL 
        ${gtest_srcs} src/${testname}.cpp)
    target_link_libraries(moshtest${testname} testingmosh testinggc 
        ${GMP_LIBRARY} ${arch_libs})
    set_target_properties(moshtest${testname} PROPERTIES FOLDER Test)
    add_test(mosh-${testname} moshtest${testname})
    add_dependencies(TestTargets moshtest${testname})
endmacro(add_moshcoretest)

add_moshcoretest(ObjectTest)
if(MOSH_WITH_NMOSH)
    message(STATUS "VMTest is not ported to nmosh")
else()
    add_moshcoretest(VMTest)
endif()
add_moshcoretest(PortTest)
add_moshcoretest(FaslTest)
add_moshcoretest(FFITest)
add_moshcoretest(OSCompatTest)
if(MSVC)
    message(STATUS "getoptUTest is not compatible with MSVC(UTF-16) builds")
else()
    add_moshcoretest(getoptUTest)
endif()
add_moshcoretest(OSCompatThreadTest)

if(MOSH_PORTABLE)
    install(DIRECTORY ${PROJECT_SOURCE_DIR}/lib DESTINATION .)
    if(EXISTS ${MOSH_GMP_DIR}/mpir.dll)
        install(FILES ${MOSH_GMP_DIR}/mpir.dll DESTINATION .)
    endif()
else()
    install(DIRECTORY ${PROJECT_SOURCE_DIR}/lib 
        DESTINATION share/mosh/${MOSH_VERSION})
    if(EXISTS ${MOSH_GMP_DIR}/mpir.dll)
        install(FILES ${MOSH_GMP_DIR}/mpir.dll DESTINATION bin)
    endif()
endif()

SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Mosh")
SET(CPACK_PACKAGE_VENDOR "Mosh project")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
SET(CPACK_PACKAGE_VERSION_MAJOR ${MOSH_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${MOSH_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${MOSH_VERSION_MICRO})

if(WIN32 AND NOT UNIX)
    SET(CPACK_PACKAGE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/misc/logo/mosh.ico")
elseif(APPLE)
    SET(CPACK_BUNDLE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/misc/logo/mosh.icns")
    SET(CPACK_BUNDLE_STARTUP_COMMAND
        "${CMAKE_CURRENT_SOURCE_DIR}/misc/dist/mac/bundle-mosh-run.sh")
    SET(CPACK_BUNDLE_NAME "Mosh") # Match misc/dist/mac/bundle-mosh-run.sh
    SET(CPACK_BUNDLE_PLIST
        "${CMAKE_CURRENT_SOURCE_DIR}/misc/dist/mac/Info.plist")
    SET(CPACK_GENERATOR "Bundle")
endif()
SET(CPACK_PACKAGE_EXECUTABLES ${MOSH_EXECUTABLE_NAME})
INCLUDE(CPack)

