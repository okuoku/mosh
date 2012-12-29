include(CMakeParseArguments)
# FIXME: Legacy!
macro(add_mosh_plugin nam)
    add_library(${nam} MODULE ${ARGN})
    set_target_properties(${nam} PROPERTIES
        FOLDER Plugins
        PREFIX ""
        SUFFIX ".mplg")
    install(TARGETS ${nam} DESTINATION plugins)
endmacro(add_mosh_plugin)

macro(add_nmosh_plugin nam)
    set(_multiargs
        C_SOURCES LINK_LIBRARIES LINK_DIRECTORIES)
    cmake_parse_arguments(NMOSH_PLUGIN
        ""
        ""
        "${_multiargs}"
        ${ARGN})
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
    add_library(${nam} ${_disposition} ${NMOSH_PLUGIN_C_SOURCES})
    target_link_libraries(${nam} ${NMOSH_PLUGIN_LINK_LIBRARIES})
    set_target_properties(${nam} PROPERTIES
        FOLDER ${_folder}
        PREFIX ""
        SUFFIX ".mplg")
    if(NMOSHPLUGIN_${nam}_EMBED)
        # We have no file to install
    else()
        install(TARGETS ${nam} DESTINATION plugins)
    endif()
endmacro(add_nmosh_plugin)

