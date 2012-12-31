include(NmoshPluginUtil)
macro(nmosh_plugin_reset)
    set(ZZNMOSHPLUGIN_ADDLIBS "" CACHE STRING "nmosh internal" FORCE) 
    set(ZZNMOSHPLUGIN_ADDLIBDIR "" CACHE STRING "nmosh internal" FORCE) 
    mark_as_advanced(ZZNMOSHPLUGIN_ADDLIBS)
    mark_as_advanced(ZZNMOSHPLUGIN_ADDLIBDIR)
endmacro(nmosh_plugin_reset)
macro(target_link_nmosh_plugin nam)
    # FIXME: Edit target properties instead
    link_directories(${ZZNMOSHPLUGIN_ADDLIBDIR})
    target_link_libraries(${nam} ${ZZNMOSHPLUGIN_ADDLIBS})
endmacro(target_link_nmosh_plugin)

macro(add_nmosh_plugin_directory0 default_p prefer_embded_p 
        nam dir)
    # FIXME: Handle default_p
    add_subdirectory(${dir} plugins/${nam})
    option(NMOSHPLUGIN_${nam}_EMBED
        "Embed nmosh plugin ${nam}" OFF)
endmacro(add_nmosh_plugin_directory0)

macro(add_nmosh_plugin_directory default_p nam dir)
    add_nmosh_plugin_directory0(${default_p} OFF
        ${nam} ${dir})
endmacro(add_nmosh_plugin_directory)

# Configurable options
set(NMOSH_UNINSTALLED_PATH ${CMAKE_CURRENT_BINARY_DIR}
    CACHE PATH "Build path prefix for debugging uninstalled build")
mark_as_advanced(NMOSH_UNINSTALLED_PATH)
