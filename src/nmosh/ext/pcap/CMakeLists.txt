include(MoshGlobals)
include(NmoshPlugin)

include(CheckIncludeFile)

set(_build OFF)

set(pcap_srcs
    mpcap.c
    )

if(WIN32)
    # Path to wpdpack
    set(NMOSH_WITH_WINPCAP_SDK CACHE PATH "Path to WpdPack directory")
    if(EXISTS ${NMOSH_WITH_WINPCAP_SDK})
        if(MOSH64)
            link_directories(${NMOSH_WITH_WINPCAP_SDK}/Lib/x64)
        else()
            link_directories(${NMOSH_WITH_WINPCAP_SDK}/Lib)
        endif()
        include_directories(${NMOSH_WITH_WINPCAP_SDK}/Include)
        set(_build ON)
    else()
        if(NMOSH_WITH_WINPCAP_SDK)
            message(WARNING 
                "WpdPack was not found on ${NMOSH_WITH_WINPCAP_SDK}")
        endif()
    endif()
    if(MSVC)
        set(pcap_libs Packet wpcap)
    else()
        set(pcap_libs packet wpcap)
    endif()
    add_definitions(-DWPCAP -DHAVE_REMOTE)
else()
    CHECK_INCLUDE_FILE(pcap.h HAVE_PCAP_H)
    set(pcap_libs pcap)
    if(HAVE_PCAP_H)
        set(_build ON)
    endif()
endif()


if(_build)
    add_nmosh_plugin(mosh_pcap
        C_SOURCES
        ${pcap_srcs}
        LINK_LIBRARIES
        ${pcap_libs})
endif()

