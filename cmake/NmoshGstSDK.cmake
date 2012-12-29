# Pick-up GStreamer SDK from fluendo
#
# GStreamer SDK also provides: GTK/GObject/GMP.

# Win32, from environment variable GSTREAMER_SDK_ROOT_X86
if(WIN32)
    if(MOSH32)
        file(TO_CMAKE_PATH $ENV{GSTREAMER_SDK_ROOT_X86} _sdkroot)
    elseif(MOSH64)
        file(TO_CMAKE_PATH $ENV{GSTREAMER_SDK_ROOT_X86_64} _sdkroot)
    endif()
    if(EXISTS ${_sdkroot})
        set(HAVE_GST_SDK ON)
        message(STATUS "Using GStreamer SDK from ${_sdkroot}")
        list(APPEND CMAKE_FIND_ROOT_PATH ${_sdkroot})
        if(NOT MSVC)
            set(CAIRO_LIBRARY ${_sdkroot}/lib/libcairo.dll.a)
        endif()
        # FIXME: WHY?
        include_directories(${_sdkroot}/lib/glib-2.0/include)
    endif()
elseif(APPLE) # Apple , from GStreamer framework
    find_library(GST_FRAMEWORK GStreamer)
    if(GST_FRAMEWORK)
        set(HAVE_GST_SDK ON)
        include_directories(BEFORE ${GST_FRAMEWORK})
        set(GTK2_GTK_INCLUDE_DIR ${GST_FRAMEWORK}/Headers CACHE STRING
            "Workaround for FindGtk2 bug." FORCE)
        set(FREETYPE_LIBRARY
            ${GST_FRAMEWORK}/Libraries/libfreetype.dylib
            CACHE PATH
            "Using GStreamer Framework")
        set(GTK2_GLIB_LIBRARY
            ${GST_FRAMEWORK}/Libraries/libglib-2.0.dylib
            CACHE PATH
            "Using GStreamer Framework")
        set(GTK2_GOBJECT_LIBRARY
            ${GST_FRAMEWORK}/Libraries/libgobject-2.0.dylib
            CACHE PATH
            "Using GStreamer Framework")
        set(GTK2_GTHREAD_LIBRARY
            ${GST_FRAMEWORK}/Libraries/libgthread-2.0.dylib
            CACHE PATH
            "Using GStreamer Framework")
    endif()
endif()
