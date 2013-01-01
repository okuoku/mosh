#ifndef __NMOSH__PLUGIN_IF_H
#define __NMOSH__PLUGIN_IF_H

#include <nmosh/common.h>

/* Definitions */


#if !defined(NMOSHPLUGIN_EMBED)
#if defined(_WIN32)
#define MOSHEXPORT __declspec(dllexport)
#else
#define MOSHEXPORT
#endif
#else
#define MOSHEXPORT /* non export */
#endif /* !NMOSHPLUGIN_EMBED */

NMOSH_COMMON_BEGIN

typedef void (*nmosh_chime_callback_t)(void* chime_param);

struct {
    nmosh_chime_callback_t caller;
    void* state;
}nmosh_chime_call_t;

NMOSH_COMMON_END


#endif /* __NMOSH__PLUGIN_IF_H */
