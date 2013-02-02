#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/notifmsg.h>

extern "C"{
//}

MOSHEXPORT
void*
mwx_notification_create(const char* title, const char* message, 
                        int type){
    wxNotificationMessage* n;
    n = new wxNotificationMessage(wxString::FromUTF8(title),
                                  wxString::FromUTF8(message),
                                  NULL /* Parent */,
                                  type);
    return n;
}

MOSHEXPORT
void
mwx_notification_destroy(wxNotificationMessage* n){
    delete n;
}

MOSHEXPORT
void
mwx_notification_show(wxNotificationMessage* n,int seconds){
    /* -1 for auto, 0 without timeout */
    n->Show(seconds);

}

//{
}
