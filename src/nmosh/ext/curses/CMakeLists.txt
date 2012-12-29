include(MoshGlobals)
include(NmoshPlugin)

set(NMOSH_WITH_CURSES CACHE 
    PATH "Path to nmosh-pdcurses directory (win32 only)")

# Curses
if(NOT NMOSH_WITH_CURSES)
    find_package( Curses )
endif()


if(CURSES_FOUND)
    set(_build ON)
else()
    set(_build OFF)
    if(WIN32 AND NMOSH_WITH_CURSES)
        set(_build ON)
    endif()
endif()

if(_build)
    set(_build_cui OFF)
    if(CURSES_INCLUDE_DIR)
        include_directories(AFTER ${CURSES_INCLUDE_DIR})
    elseif(NMOSH_WITH_CURSES)
        if(EXISTS ${NMOSH_WITH_CURSES})
            set(_build_cui ON)
            add_subdirectory(${NMOSH_WITH_CURSES} pdcurses) # => pdcurses_win32a
            set(CURSES_LIBRARY pdcurses_win32a)
            include_directories(AFTER ${NMOSH_WITH_CURSES})
        else()
            message(FATAL_ERROR 
                "Set NMOSH_WITH_CURSES to nmosh-pdcurses directory.")
        endif()
    else()
        include_directories(AFTER ${CURSES_INCLUDE_PATH})
    endif()
    add_nmosh_plugin(mosh_curses 
        LINK_LIBRARIES ${CURSES_LIBRARY}
        C_SOURCES mosh_curses.c)
    if(_build_cui)
        add_nmosh_plugin(mosh_curses_cui
            LINK_LIBRARIES pdcurses_win32
            C_SOURCES mosh_curses.c)
    endif()
endif()

