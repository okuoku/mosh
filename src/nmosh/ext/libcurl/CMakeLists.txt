include(MoshGlobals)
include(NmoshPlugin)

set(NMOSH_WITH_LIBCURL CACHE PATH "Path to curl directory")

if(NMOSH_WITH_LIBCURL)
    set(_build ON)
    set(CURL_STATICLIB ON CACHE BOOL "(configured by nmosh)" FORCE)
    if(MSVC)
        # Workaround for CURL_SIZEOF_CURL_OFF_T
        if(MOSH64)
            # Took from curl's CMakeLists.txt
            set(CURL_TYPEOF_CURL_OFF_T "long long")
            set(CURL_SIZEOF_CURL_OFF_T 8)
            set(CURL_FORMAT_CURL_OFF_T "lld")
            set(CURL_FORMAT_CURL_OFF_TU "llu")
            set(CURL_FORMAT_OFF_T "%lld")
            set(CURL_SUFFIX_CURL_OFF_T LL)
            set(CURL_SUFFIX_CURL_OFF_TU ULL)
        endif()
    endif()
    if(APPLE)
        set(_libcurl_disposition "")
    else()
        set(_libcurl_disposition EXCLUDE_FROM_ALL)
    endif()
    add_subdirectory(${NMOSH_WITH_LIBCURL} libcurl ${_libcurl_disposition})
    include_directories(${NMOSH_WITH_LIBCURL}/include
        # For curlbuild.h
        ${CMAKE_CURRENT_BINARY_DIR}/libcurl/include/curl)
    add_definitions(-DCURL_STATICLIB)
endif()

if(_build)
    add_nmosh_plugin(mosh_libcurl
        C_SOURCES mcurl_multi.c
        LINK_LIBRARIES libcurl)
endif()
