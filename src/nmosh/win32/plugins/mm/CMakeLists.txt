include(MoshGlobals)
include(NmoshPlugin)

if(WIN32)
    add_nmosh_plugin(mosh_winmm
        LINK_LIBRARIES winmm
        C_SOURCES moshmm.c)
endif()
