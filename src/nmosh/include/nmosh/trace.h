/* FIXME: Stub stub stub ! */


/* nmosh trace */
#ifndef __NMOSH_TRACE_H
#define __NMOSH_TRACE_H

#include <nmosh/common.h>
NMOSH_COMMON_BEGIN

/* TRACE_MALLOC/FREE/COLLECT: */
#define NMOSH_TRACE_MALLOC_DEFINE(type, description)
#define NMOSH_TRACE_MALLOC(type, p)
#define NMOSH_TRACE_FREE(type)
#define NMOSH_TRACE_COLLECT_BEGIN(type)
#define NMOSH_TRACE_COLLECT_END(type)

/* TRACE_OBJECT: Object allocation trace */
#define NMOSH_TRACE_OBJECT_DEFINE(type, description)
#define NMOSH_TRACE_OBJECT_CREATE(type, p)
#define NMOSH_TRACE_OBJECT_DESTROY(type, p)
#define NMOSH_TRACE_OBJECT_RETAIN(type, p)
#define NMOSH_TRACE_OBJECT_RELEASE(type, p)

/* MALLOC: C runtime malloc() wrapper */
#define NMOSH_MALLOC_DEFINE(type, description)
#define NMOSH_MALLOC_DECLARE(type)
#define NMOSH_MALLOC(type, size)
#define NMOSH_FREE(type, p)
#define NMOSH_REALLOC(type, p, size)

NMOSH_COMMON_END
#endif /* __NMOSH_TRACE_H */
