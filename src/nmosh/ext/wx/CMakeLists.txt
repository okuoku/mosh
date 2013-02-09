project(wx-plugin)

find_package(wxWidgets COMPONENTS gl adv core base)

include_directories(.)

set(srcs 
    controls/mwx_text.cpp
    controls/mwx_textattr.cpp
    colour/mwx_colour.cpp
    window/mwx_window.cpp
    window/mwx_sizer.cpp
    font/mwx_font.cpp
    frame/mwx_frame.cpp
    icon/mwx_icon.cpp
    taskbar/mwx_taskbar.cpp
    menu/mwx_menu.cpp
    notification/mwx_notification.cpp
    mwx_event.cpp
    mosh_wx.cpp)

if(wxWidgets_FOUND)
    include(${wxWidgets_USE_FILE})
    if(MSVC)
        add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    endif()
    add_nmosh_plugin(mosh_wx
        # FIXME: add CXX_SOURCES...
        C_SOURCES ${srcs}
        LINK_LIBRARIES ${wxWidgets_LIBRARIES}
        LINK_DIRECTORIES ${wxWidgets_LIBRARY_DIRS}
        )
    if(FALSE)
    # FIXME: Move this to add_nmosh_plugin
    set_property(
        SOURCE ${srcs}
        APPEND 
        PROPERTY COMPILE_DEFINITIONS
        ${wxWidgets_DEFINITIONS})
    set_property(
        SOURCE ${srcs}
        APPEND
        PROPERTY COMPILE_DEFINITIONS_DEBUG
        ${wxWidgets_DEFINITIONS_DEBUG})
    endif()
endif()
