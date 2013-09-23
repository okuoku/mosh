#if defined(HAVE_CL)||defined(HAVE_CL_CL_H)||defined(HAVE_OPENCL_CL_H)
#define __HAVE_CL 1
#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif
#endif

#include "mocl.gen-inc.c"
