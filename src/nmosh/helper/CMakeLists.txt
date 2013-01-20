set(NMOSHPLUGIN_moshvm_helper_EMBED ON CACHE BOOL "Force Embedded" FORCE)
add_nmosh_plugin(moshvm_helper
    C_SOURCES nmosh-plugin-helper.cpp)
