#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/window.h>
#include "mwx_event.h"

class nmoshPaintableWindow : public wxWindow {
    public:
        nmoshPaintableWindow(wxWindow* parent, long style)
            : wxWindow(parent,wxID_ANY,wxDefaultPosition,wxDefaultSize,style)
        {};

};

extern "C" {
// }

MOSHEXPORT
void*
mwx_window_create_paintable(void* handler, wxWindow* parent, int style){
    nmoshEventHandler* eh;
    nmoshPaintableWindow* pw;
    pw = new nmoshPaintableWindow(parent, style);
    eh = new nmoshEventHandler(handler);
    eh->AttachWithPaintable(pw);
    pw->SetBackgroundStyle(wxBG_STYLE_CUSTOM); // Legacy.
    return pw;
}

MOSHEXPORT
void
mwx_window_refresh(wxWindow* wnd, int x, int y, int w, int h){
    wxRect r(x,y,w,h);
    wnd->Refresh(true, &r);
}

MOSHEXPORT
void
mwx_window_setfocus(wxWindow* wnd){
    wnd->SetFocus();
}

MOSHEXPORT
int
mwx_window_getid(wxWindow* wnd){
    return wnd->GetId();
}

MOSHEXPORT
void
mwx_window_getsize(wxWindow* wnd, int* out_width, int* out_height){
    wnd->GetSize(out_width, out_height);
}

MOSHEXPORT
void
mwx_window_setsizer(void* window, void* sizer){
    wxWindow* wnd = reinterpret_cast<wxWindow *>(window);
    wxSizer* siz = reinterpret_cast<wxSizer *>(sizer);
    wnd->SetSizer(siz);
}

MOSHEXPORT
void
mwx_window_show(void* window, int b){
    wxWindow* wnd = reinterpret_cast<wxWindow *>(window);
    wnd->Show(b);
}

MOSHEXPORT
void
mwx_window_destroy(wxWindow* window){
    window->Destroy();
}

NMOSH_CONSTANT_BEGIN(mwx_window)
    NMOSH_EXPORT_SYMBOL_INT(wxICON_INFORMATION)
    NMOSH_EXPORT_SYMBOL_INT(wxICON_WARNING)
    NMOSH_EXPORT_SYMBOL_INT(wxICON_ERROR)
NMOSH_CONSTANT_END()

// {
};
