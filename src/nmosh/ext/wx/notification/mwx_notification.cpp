#include <nmosh/plugin-if.h>
#include <wx/wx.h>

#if wxCHECK_VERSION(2,9,0)
#include <wx/notifmsg.h>
#define HAVE_THIS
#else
typedef char* wxNotificationMessage;
#endif

extern "C"{
//}

MOSHEXPORT
void*
mwx_notification_create(const char* title, const char* message, 
                        int type){
#ifdef HAVE_THIS
    wxNotificationMessage* n;
    n = new wxNotificationMessage(wxString::FromUTF8(title),
                                  wxString::FromUTF8(message),
                                  NULL /* Parent */,
                                  type);
    return n;
#else
    return NULL;
#endif
}

MOSHEXPORT
void
mwx_notification_destroy(wxNotificationMessage* n){
#ifdef HAVE_THIS
    delete n;
#endif
}

MOSHEXPORT
void
mwx_notification_show(wxNotificationMessage* n,int seconds){
#ifdef HAVE_THIS
    /* -1 for auto, 0 without timeout */
    n->Show(seconds);
#endif
}

MOSHEXPORT
void
mwx_notification_close(wxNotificationMessage* n){
#ifdef HAVE_THIS
    /* FIXME: Return value(bool) */
    n->Close();
#endif
}

//{
}
