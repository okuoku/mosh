# Plugin mosh_sdl
cmake_minimum_required(VERSION 2.8)
include(MoshGlobals)
include(NmoshPlugin)

set(NMOSH_WITH_SDL CACHE PATH "Path to SDL2 directory")

# SDL2 Plugin
if(NMOSH_WITH_SDL)
    project(mosh_sdl)
    set(_linkdir "")
    if(WIN32) # Favor static library on Win32 to avoid pull-in of SDL1
        add_definitions(-DDECLSPEC=) # Magic. Not export SDL functions.
        set(SDL_STATIC ON CACHE BOOL "Forced by nmosh")
        set(SDL_LIBS SDL2-static)
        if(MOSH64)
            file(TO_CMAKE_PATH "$ENV{DXSDK_DIR}\\lib\\x64" _linkdir)
        else()
            file(TO_CMAKE_PATH "$ENV{DXSDK_DIR}\\lib\\x86" _linkdir)
        endif()
    else()
        set(SDL_SHARED ON CACHE BOOL "Forced by nmosh")
        set(SDL_LIBS SDL2)
    endif()
    add_subdirectory(${NMOSH_WITH_SDL} 
        SDL-nmosh EXCLUDE_FROM_ALL)
    set(SDL_INCLUDE_DIR 
        ${NMOSH_WITH_SDL}/include)
    add_nmosh_plugin(mosh_sdl 
        LINK_DIRECTORIES ${_linkdir}
        LINK_LIBRARIES ${SDL_LIBS}
        C_SOURCES mosh_sdl_core.c)
    include_directories(AFTER ${SDL_INCLUDE_DIR})
endif()
