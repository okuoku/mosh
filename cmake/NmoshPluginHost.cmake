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
    option(NMOSHPLUGIN_${nam}_BUILD
        "Build nmosh plugin ${nam}" ${default_p})
    option(NMOSHPLUGIN_${nam}_EMBED
        "Embed nmosh plugin ${nam}" ${prefer_embded_p})
    if(NMOSHPLUGIN_${nam}_BUILD)
        add_subdirectory(${dir} ${nam})
    endif()
endmacro(add_nmosh_plugin_directory0)

macro(add_nmosh_plugin_directory default_p nam dir)
    add_nmosh_plugin_directory0(OFF ${default_p}
        ${nam} ${dir})
endmacro(add_nmosh_plugin_directory)
