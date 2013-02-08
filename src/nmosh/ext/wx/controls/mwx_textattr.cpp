#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/textctrl.h>

extern "C" {
// }

MOSHEXPORT
void*
mwx_textattr_create(void){
    return new wxTextAttr();
}

MOSHEXPORT
void
mwx_textattr_setfont(wxTextAttr* ta, wxFont* fon){
    ta->SetFont(*fon);
}

MOSHEXPORT
void
mwx_textattr_destroy(wxTextAttr* ta){
    delete ta;
}

MOSHEXPORT
void
mwx_textattr_merge(wxTextAttr* ta, wxTextAttr* op){
    ta->Merge(*op);
}

// {
}
