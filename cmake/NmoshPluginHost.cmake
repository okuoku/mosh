include(NmoshPluginUtil)
macro(nmosh_plugin_reset)
    set(ZZNMOSHPLUGIN_ADDLIBS "" CACHE STRING "nmosh internal" FORCE) 
    set(ZZNMOSHPLUGIN_ADDLIBDIR "" CACHE STRING "nmosh internal" FORCE) 
    set(ZZNMOSHPLUGIN_EMBED "" CACHE STRING "nmosh internal" FORCE) 
    mark_as_advanced(ZZNMOSHPLUGIN_ADDLIBS)
    mark_as_advanced(ZZNMOSHPLUGIN_ADDLIBDIR)
    mark_as_advanced(ZZNMOSHPLUGIN_EMBED)
endmacro(nmosh_plugin_reset)
macro(target_link_nmosh_plugin nam)
    # FIXME: Edit target properties instead
    link_directories(${ZZNMOSHPLUGIN_ADDLIBDIR})
    target_link_libraries(${nam} ${ZZNMOSHPLUGIN_ADDLIBS})
endmacro(target_link_nmosh_plugin)

macro(bless_nmosh_plugin_stub_source1 pth nam)
    set_property(SOURCE ${pth}
        APPEND
        PROPERTY COMPILE_DEFINITIONS
        "NMOSHPLUGIN_${nam}_EMBED")
endmacro(bless_nmosh_plugin_stub_source1)

macro(bless_nmosh_plugin_stub_source pth)
    foreach(e ${ZZNMOSHPLUGIN_EMBED})
        bless_nmosh_plugin_stub_source1(${pth} ${e})
    endforeach()
endmacro(bless_nmosh_plugin_stub_source)

macro(add_nmosh_plugin_directory0 default_p prefer_embded_p 
        nam dir)
    if(ANDROID)
        # Android requires every plugins embedded.
        # Currently, we do not configure BoehmGC as dl safe.
        option(NMOSHPLUGIN_${nam}_EMBED
            "Embed nmosh plugin ${nam}" ON)
        mark_as_advanced(NMOSHPLUGIN_${nam}_EMBED)
    else()
        option(NMOSHPLUGIN_${nam}_EMBED
            "Embed nmosh plugin ${nam}" OFF)
    endif()
    # FIXME: Handle default_p
    add_subdirectory(${dir} plugins/${nam})
endmacro(add_nmosh_plugin_directory0)

macro(add_nmosh_plugin_directory default_p nam dir)
    add_nmosh_plugin_directory0(${default_p} OFF
        ${nam} ${dir})
endmacro(add_nmosh_plugin_directory)

# Configurable options
set(NMOSH_UNINSTALLED_PATH ${CMAKE_CURRENT_BINARY_DIR}
    CACHE PATH "Build path prefix for debugging uninstalled build")
mark_as_advanced(NMOSH_UNINSTALLED_PATH)
