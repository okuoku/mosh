#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/textctrl.h>
#include "mwx_event.h"

extern "C" {
// }

MOSHEXPORT
void*
mwx_textctrl_create(wxWindow* parent, void* handler, int id, int style){
    nmoshEventHandler* eh;
    wxTextCtrl* ret;
    eh = new nmoshEventHandler(handler);

    ret = new wxTextCtrl(parent, id, wxEmptyString,
                          wxDefaultPosition,
                          wxDefaultSize,
                          style);
    eh->Attach(ret);
    return ret;
}

MOSHEXPORT
void* /* Scheme object */
mwx_textctrl_getvalue(wxTextCtrl* ctl){
    const wxString& text = ctl->GetValue();
    const wxCharBuffer t = text.utf8_str();
    NMOSH_EXPORT_BEGIN(ret)
        NMOSH_EXPORT_BUFFER(NULL, t.data(), strlen(t.data()))
    NMOSH_EXPORT_END()
    return NMOSH_EXPORT(ret);
}

MOSHEXPORT
void
mwx_textctrl_setvalue(wxTextCtrl* ctl, const char* text){
    ctl->SetValue(wxString::FromUTF8(text));
}

MOSHEXPORT
void
mwx_textctrl_appendtext(wxTextCtrl* ctl, const char* text){
    ctl->AppendText(wxString::FromUTF8(text));
}

MOSHEXPORT
void
mwx_textctrl_showposition(wxTextCtrl* ctl, int pos){
    ctl->ShowPosition(pos);
}

MOSHEXPORT
int
mwx_textctrl_getlastposition(wxTextCtrl* ctl){
    return ctl->GetLastPosition();
}

MOSHEXPORT
void
mwx_textctrl_sethint(wxTextCtrl* ctl, const char* text){
#if wxCHECK_VERSION(2,9,0)
    ctl->SetHint(wxString::FromUTF8(text));
#endif
}

MOSHEXPORT
void
mwx_textctrl_setdefaultstyle(wxTextCtrl* ctl, const wxTextAttr* attr){
    ctl->SetDefaultStyle(*attr);
}

NMOSH_CONSTANT_BEGIN(mwx_text)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_PROCESS_ENTER)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_PROCESS_TAB)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_MULTILINE)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_PASSWORD)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_READONLY)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_RICH)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_RICH2)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_AUTO_URL)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_NOHIDESEL)
    NMOSH_EXPORT_SYMBOL_INT(wxHSCROLL)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_NO_VSCROLL)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_LEFT)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_CENTRE)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_RIGHT)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_DONTWRAP)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_CHARWRAP)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_WORDWRAP)
    NMOSH_EXPORT_SYMBOL_INT(wxTE_BESTWRAP)
NMOSH_CONSTANT_END()

// {
}
