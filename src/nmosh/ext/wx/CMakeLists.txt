project(wx-plugin)

find_package(wxWidgets COMPONENTS gl core base)

set(srcs 
    mosh_wx.cpp)
if(wxWidgets_FOUND)
    include(${wxWidgets_USE_FILE})
    # include_directories(${wxWidgets_INCLUDE_DIRS})
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
