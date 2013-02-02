#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/window.h>

extern "C" {
// }

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
