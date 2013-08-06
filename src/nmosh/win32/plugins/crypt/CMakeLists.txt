include(MoshGlobals)
include(NmoshPlugin)

if(WIN32)
    add_nmosh_plugin(mosh_wincrypt
        LINK_LIBRARIES advapi32
        C_SOURCES mosh_wincrypt.c)
endif()
