include(MoshGlobals)
include(NmoshPlugin)

set(_build OFF)

# libusb
if(PKG_CONFIG_FOUND AND NOT WIN32)
    # check module with pkgconfig
    pkg_check_modules(PKG_LIBUSB1 libusb-1.0)
endif()

if(PKG_LIBUSB1_FOUND)
    link_directories(${PKG_LIBUSB1_LIBRARY_DIRS})
    set(LIBUSB1_LIBRARY ${PKG_LIBUSB1_LIBRARIES})
    set(LIBUSB1_INCLUDE_DIR ${PKG_LIBUSB1_INCLUDE_DIRS})
elseif(WIN32)
    set(NMOSH_LIBUSB1_PREFIX CACHE PATH "libusbx-win package prefix")
    if(NMOSH_LIBUSB1_PREFIX)
        if(MSVC)
            set(LIBUSB1_LIBRARY
                ${MOSHWIN32_LIBUSB1_PREFIX}/MS32/static/libusb-1.0.lib)
        else()
            set(LIBUSB1_LIBRARY
                ${MOSHWIN32_LIBUSB1_PREFIX}/MinGW32/static/libusb-1.0.a)
        endif()
        set(LIBUSB1_INCLUDE_DIR
            ${MOSHWIN32_LIBUSB1_PREFIX}/include/libusbx-1.0)
    else()
        message(STATUS "libusb missing(NMOSH_LIBUSB1_PREFIX)")
    endif()
else()
    # Detect FreeBSD libusb
    find_library(LIBUSB1_LIBRARY usb)
endif()

if(LIBUSB1_LIBRARY)
    set(_build ON)
endif()

if(_build)
    add_nmosh_plugin(mosh_usb 
        LINK_LIBRARIES ${LIBUSB1_LIBRARY}
        C_SOURCES libusb1.c)
    include_directories(AFTER ${LIBUSB1_INCLUDE_DIR})
endif()

