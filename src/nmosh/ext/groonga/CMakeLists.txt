include(MoshGlobals)
include(NmoshPlugin)

set(NMOSH_WITH_GROONGA CACHE PATH "Path to groonga source")
set(_build OFF)
if(EXISTS NMOSH_WITH_GROONGA)
    set(_build ON)
endif()
if(_build)
    add_nmosh_plugin(mosh_groonga 
        LINK_LIBRARIES libgroonga
        C_SOURCES mgrn.c)
    include_directories(AFTER ${NMOSH_WITH_GROONGA}/include)
    add_subdirectory(${NMOSH_WITH_GROONGA} 
        EXCLUDE_FROM_ALL nmosh-groonga)
endif()
