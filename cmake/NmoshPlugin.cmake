# FIXME: Each plugin should handle it, not globally..
include(NmoshPluginUtil)
if(NOT WIN32)
    find_package(PkgConfig)
endif()

include(CMakeParseArguments)

macro(do_add_nmosh_plugin nam)
    set(_nulargs
        NODEFAULT)
    set(_multiargs
        C_SOURCES LINK_LIBRARIES LINK_DIRECTORIES)
    cmake_parse_arguments(NMOSH_PLUGIN
        "${_nulargs}"
        ""
        "${_multiargs}"
        ${ARGN})
    if(NMOSH_PLUGIN_NODEFAULT)
        set(_default OFF)
    else()
        set(_default ON)
    endif()
    option(NMOSHPLUGIN_${nam}_BUILD
        "Build nmosh plugin ${nam}" ${_default})
    if(NMOSHPLUGIN_${nam}_BUILD)
        if(NMOSHPLUGIN_${nam}_EMBED)
            add_definitions(-DNMOSHPLUGIN_EMBED)
            set(_disposition STATIC)
            set(_folder Libraries)
            set(_dirs "")
            set(_libs "")
            list(APPEND _libs ${ZZNMOSHPLUGIN_ADDLIBS}
                ${NMOSH_PLUGIN_LINK_LIBRARIES}
                ${nam})
            list(APPEND _dirs ${ZZNMOSHPLUGIN_ADDLIBDIR}
                ${NMOSH_PLUGIN_LINK_DIRECTORIES})
            set(ZZNMOSHPLUGIN_ADDLIBS ${_libs} 
                CACHE STRING "nmosh internal" FORCE)
            set(ZZNMOSHPLUGIN_ADDLIBDIR ${_dirs} 
                CACHE STRING "nmosh internal" FORCE)
        else()
            set(_disposition MODULE)
            set(_folder Plugins)
        endif()
        link_directories(${NMOSH_PLUGIN_LINK_DIRECTORIES})
        if(NMOSHPLUGIN_${nam}_EMBED)
            add_library(${nam} ${_disposition} ${NMOSH_PLUGIN_C_SOURCES})
        else()
            set(_or ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
            set(_ol ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${NMOSH_UNINSTALLED_PATH})
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${NMOSH_UNINSTALLED_PATH})
            add_library(${nam} ${_disposition} 
                ${NMOSH_PLUGIN_C_SOURCES})
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${_or})
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${_ol})
            set_target_properties(${nam} PROPERTIES
                FOLDER ${_folder}
                PREFIX ""
                SUFFIX ".mplg")
            # Move library destination to the side of main executable
            install(TARGETS ${nam} DESTINATION plugins)
        endif()
        target_link_libraries(${nam} ${NMOSH_PLUGIN_LINK_LIBRARIES})
    endif()
endmacro(do_add_nmosh_plugin)

macro(add_nmosh_plugin nam)
    do_add_nmosh_plugin(${nam} ${ARGN})
endmacro(add_nmosh_plugin)
