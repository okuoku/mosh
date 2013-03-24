project(nmosh-sqlite3)

if(NMOSH_WITH_SQLITE3)
    add_definitions(-DNMOSH_WITH_SQLITE3)
    include_directories(${NMOSH_WITH_SQLITE3})
    set(NMOSH_HAS_SQLITE3 ON)
endif()

if(NMOSH_HAS_SQLITE3)
    add_nmosh_plugin(mosh_sqlite3
        # FIXME: add CXX_SOURCES...
        C_SOURCES msqlite3.c
        )
endif()
