include(MoshGlobals)
include(NmoshPlugin)

# Cairo

set(_build OFF)

# Don't use pkg-config to search cairo iff GStreamer SDK available
if(PKG_CONFIG_FOUND AND NOT GST_FRAMEWORK AND NOT WIN32)
    pkg_check_modules(PKG_CAIRO cairo)
endif()

if(PKG_CAIRO_FOUND)
    # Use pkg-config data
    link_directories(${PKG_CAIRO_LIBRARY_DIRS})
    set(CAIRO_LIBRARY ${PKG_CAIRO_LIBRARIES})
    set(CAIRO_INCLUDE_DIR ${PKG_CAIRO_INCLUDE_DIRS})
elseif(GST_FRAMEWORK)
    set(CAIRO_INCLUDE_DIR ${GST_FRAMEWORK}/Headers)
    set(CAIRO_LIBRARY ${GST_FRAMEWORK}/Libraries/libcairo.dylib)
    add_definitions(-DHAVE_CAIRO_H)
else()
    # handwritten..

    set(CAIRO_LIBS "" CACHE STRING "additional cairo libraries")

    if(MSVC)
        set(CAIROLIB "cairo.lib")
    else()
        if(APPLE)
            set(CAIROLIB "libcairo.dylib")
        else()
            set(CAIROLIB "libcairo.so")
        endif()
    endif()

    find_path(CAIRO_INCLUDE_DIR cairo/cairo.h)
    if(CAIRO_INCLUDE_DIR)
        set(HAVE_CAIRO_CAIRO_H 1)
        add_definitions(-DHAVE_CAIRO_CAIRO_H)
    else()
        find_path(CAIRO_INCLUDE_DIR cairo.h)
        if(CAIRO_INCLUDE_DIR)
            add_definitions(-DHAVE_CAIRO_H)
        endif()
    endif()

    find_library(CAIRO_LIBRARY ${CAIROLIB})

    if(NOT CAIRO_LIBRARY AND GTK2_CAIRO_LIBRARY)
        set(CAIRO_LIBRARY ${GTK2_CAIRO_LIBRARY})
    endif()
endif()

if(PKG_CAIRO_FOUND OR CAIRO_INCLUDE_DIR)
    set(_build ON)
endif()

if(_build)
    add_nmosh_plugin(mosh_cairo 
        LINK_LIBRARIES ${CAIRO_LIBS} ${CAIRO_LIBRARY}
        C_SOURCES moshcairo.c)
    include_directories(AFTER ${CAIRO_INCLUDE_DIR})
endif()

