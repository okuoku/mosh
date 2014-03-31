#ifndef SCHEME_BIGNUM_SHIM_H
#define SCHEME_BIGNUM_SHIM_H

#ifdef USE_MP_MINI_GMP
/* Statically link against mini-gmp */
#include <mini-gmp.h>
#include "bignum_shim_mini_gmp.h"
#else /* USE_MP_GMP */
/* By default, use full gmp */
#include <gmp.h>
#endif

#endif
