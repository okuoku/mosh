#ifndef __NMOSH__PLUGIN_IF_H
#define __NMOSH__PLUGIN_IF_H

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


#ifdef __cplusplus
extern "C" {
#endif

#if 0
}
#endif

typedef void (*nmosh_chime_callback_t)(void* chime_param);

struct {
    nmosh_chime_callback_t caller;
    void* state;
}nmosh_chime_call_t;




#if 0
{
#endif

#ifdef __cplusplus
} /* Extern "C" */
#endif 

#endif /* __NMOSH__PLUGIN_IF_H */
