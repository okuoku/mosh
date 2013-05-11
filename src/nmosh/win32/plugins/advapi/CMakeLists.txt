include(MoshGlobals)
include(NmoshPlugin)

if(WIN32)
    add_nmosh_plugin(mosh_advapi
        LINK_LIBRARIES advapi32
        C_SOURCES mosh_advapi.c)
endif()
