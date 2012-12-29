include(MoshGlobals)
include(NmoshPlugin)
include(NmoshGstSDK)


# GTK2
find_package(GTK)
find_package(GTK2)
set(_build OFF)
if(HAVE_GST_SDK OR GTK2_FOUND)
    set(_build ON)
endif()
if(_build)
    project(mosh_gobj)
    include_directories(AFTER ${GTK2_GLIB_INCLUDE_DIR})
    if(NOT GTK2_GTHREAD_LIBRARY)
        find_library(GTK2_GTHREAD_LIBRARY gthread-2.0)
    endif()
    add_nmosh_plugin(mosh_gobj 
        LINK_LIBRARIES 
        ${GTK2_GLIB_LIBRARY} ${GTK2_GOBJECT_LIBRARY} ${GTK2_GTHREAD_LIBRARY}
        C_SOURCES src/nmosh/ext/gobject/mgobj.c)
endif()

