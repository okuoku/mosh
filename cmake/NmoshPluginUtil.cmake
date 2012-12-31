# Common Utilities for NmoshPlugin and others

if(__NMOSH_PLUGIN_UTIL_LOADED)
    return()
endif()

set(__NMOSH_PLUGIN_UTIL_LOADED ON)

macro(nmosh_dist_uninstalled0 fil suf)
    get_filename_component(_bn ${fil} NAME)
    configure_file(${fil}
        ${NMOSH_UNINSTALLED_PATH}${suf}/${_bn}
        COPYONLY)
endmacro(nmosh_dist_uninstalled0)

macro(nmosh_dist_uninstalled fil)
    if(CMAKE_CONFIGURATION_TYPES)
        foreach(e ${CMAKE_CONFIGURATION_TYPES})
            nmosh_dist_uninstalled0(${fil} "/${e}")
        endforeach()
    else()
        nmosh_dist_uninstalled0(${fil} "")
    endif()
endmacro(nmosh_dist_uninstalled)

macro(nmosh_config_uninstalled0 fil suf nam)
    configure_file(${fil}
        ${NMOSH_UNINSTALLED_PATH}${suf}/${nam}
        ${ARGN})
endmacro(nmosh_config_uninstalled0)

macro(nmosh_config_uninstalled fil nam)
    if(CMAKE_CONFIGURATION_TYPES)
        foreach(e ${CMAKE_CONFIGURATION_TYPES})
            nmosh_config_uninstalled0(${fil} "/${e}" ${nam} ${ARGN})
        endforeach()
    else()
        nmosh_config_uninstalled0(${fil} "" ${nam} ${ARGN})
    endif()
endmacro(nmosh_config_uninstalled)
