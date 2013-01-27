#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/window.h>

extern "C" {
// }

MOSHEXPORT
void
mwx_window_show(void* window, int b){
    wxWindow* wnd = reinterpret_cast<wxWindow *>(window);
    wnd->Show(b);
}

// {
};
