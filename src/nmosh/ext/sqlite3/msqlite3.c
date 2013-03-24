#ifdef NMOSH_WITH_SQLITE3
#include <sqlite3.c>
#else
#include <sqlite3.h>
#endif
#include <nmosh/plugin-if.h>

MOSHEXPORT
void*
msqlite3_errstr(int err){
    return (void*)sqlite3_errstr(err);
}

MOSHEXPORT
int
msqlite3_open(const char* name, void** ret){
    int r;
    r = sqlite3_open(name, (sqlite3**)&ret);
    return r;
}

MOSHEXPORT
int
msqlite3_prepare_v2(sqlite3* db, const char* sql, int len, 
                sqlite3_stmt** out_stmt, const char** out_tail){
    /* NB: We stuck with v2 interface. v2 interface returns detailed error */
    return sqlite3_prepare(db, sql, len, out_stmt, out_tail);
}

#define MSQLITE_BIND_FUNC(type,name) \
    MOSHEXPORT int msqlite3_bind_ ## name (sqlite3_stmt* s, int idx, type t){\
        return sqlite3_bind_ ## name (s,idx,t); \
    }
#define MSQLITE_BIND_FUNC_BLOB(type,name) \
    MOSHEXPORT int msqlite3_bind_ ## name (sqlite3_stmt* s, int idx, type t, \
                                           int len){\
        return sqlite3_bind_ ## name (s,idx,t,len,SQLITE_TRANSIENT); \
    }

MSQLITE_BIND_FUNC_BLOB(void*,blob)
MSQLITE_BIND_FUNC(double,double)
MSQLITE_BIND_FUNC(int,int)
// FIXME: Implement 64bit type...
MSQLITE_BIND_FUNC_BLOB(const char*,text)
MSQLITE_BIND_FUNC_BLOB(const void*,text16)

MOSHEXPORT
int
msqlite3_bind_null(sqlite3_stmt* s,int idx){
    return sqlite3_bind_null(s,idx);
}


#define MSQLITE_COLUMN_FUNC(type,name) \
    MOSHEXPORT type msqlite3_column_ ## name (sqlite3_stmt* s, int col){\
        return (type)sqlite3_column_ ## name (s,col); \
    }

MSQLITE_COLUMN_FUNC(void*,blob)
MSQLITE_COLUMN_FUNC(int,bytes)
MSQLITE_COLUMN_FUNC(int,bytes16)
MSQLITE_COLUMN_FUNC(double,double)
MSQLITE_COLUMN_FUNC(int,int)
// FIXME: Implement 64bit type...
MSQLITE_COLUMN_FUNC(char*,text)
MSQLITE_COLUMN_FUNC(void*,text16)
MSQLITE_COLUMN_FUNC(int,type)

MOSHEXPORT
int /* 0 = no result */
msqlite3_column_count(sqlite3_stmt* s){
    return sqlite3_column_count(s);
}

MOSHEXPORT
int
msqlite3_step(sqlite3_stmt* s){
    return sqlite3_step(s);
}

MOSHEXPORT
int
msqlite3_finalize(sqlite3_stmt* s){
    return sqlite3_finalize(s);
}

MOSHEXPORT
int
msqlite3_close_v2(sqlite3* s){
    /* NB: we stuck with v2 interface. */
    return sqlite3_close_v2(s);
}

NMOSH_CONSTANT_BEGIN(mosh_sqlite3)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_OK)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_ERROR)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_INTERNAL)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_PERM)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_ABORT)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_BUSY)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_LOCKED)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_NOMEM)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_READONLY)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_INTERRUPT)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_IOERR)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_CORRUPT)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_NOTFOUND)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_FULL)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_CANTOPEN)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_PROTOCOL)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_EMPTY)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_SCHEMA)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_TOOBIG)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_CONSTRAINT)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_MISMATCH)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_MISUSE)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_NOLFS)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_AUTH)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_FORMAT)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_RANGE)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_NOTADB)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_ROW)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_DONE)
    /* Types */
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_INTEGER)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_FLOAT)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_BLOB)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_NULL)
    NMOSH_EXPORT_SYMBOL_INT(SQLITE_TEXT) /* Only in SQLite3 */
NMOSH_CONSTANT_END()
NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(mosh_sqlite3)
