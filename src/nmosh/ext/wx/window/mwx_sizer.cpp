#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/sizer.h>

extern "C"{
// }

MOSHEXPORT
void*
mwx_staticboxsizer_create(void* parent, int orient, const char* label){
    wxWindow* wnd = reinterpret_cast<wxWindow *>(parent);
    return new wxStaticBoxSizer(orient, wnd, wxString::FromUTF8(label));
}

MOSHEXPORT
void*
mwx_staticboxsizer_staticbox(void* staticboxsizer){
    wxStaticBoxSizer* sbs = reinterpret_cast<wxStaticBoxSizer*>(staticboxsizer);
    return sbs->GetStaticBox();
}

MOSHEXPORT
void*
mwx_boxsizer_create(int orient){
    return new wxBoxSizer(orient);
}

MOSHEXPORT
void
mwx_sizer_add_window(void* sizer, void* window,int proportion, int flags){
    wxWindow* wnd = reinterpret_cast<wxWindow *>(window);
    wxSizer* siz = reinterpret_cast<wxSizer *>(sizer);
    siz->Add(wnd, proportion, flags);
}

MOSHEXPORT
void
mwx_sizer_add_sizer(void* sizer, void* sizer2, int proportion, int flags){
    wxSizer* siz = reinterpret_cast<wxSizer *>(sizer);
    wxSizer* siz2 = reinterpret_cast<wxSizer *>(sizer2);
    siz->Add(siz2, proportion, flags);
}

NMOSH_CONSTANT_BEGIN(mwx_sizer)
NMOSH_EXPORT_SYMBOL_INT(wxVERTICAL)
NMOSH_EXPORT_SYMBOL_INT(wxHORIZONTAL)
NMOSH_EXPORT_SYMBOL_INT(wxTOP)
NMOSH_EXPORT_SYMBOL_INT(wxLEFT)
NMOSH_EXPORT_SYMBOL_INT(wxBOTTOM)
NMOSH_EXPORT_SYMBOL_INT(wxRIGHT)
NMOSH_EXPORT_SYMBOL_INT(wxALL)
NMOSH_EXPORT_SYMBOL_INT(wxSHAPED)
NMOSH_EXPORT_SYMBOL_INT(wxEXPAND)
NMOSH_EXPORT_SYMBOL_INT(wxFIXED_MINSIZE)
NMOSH_EXPORT_SYMBOL_INT(wxRESERVE_SPACE_EVEN_IF_HIDDEN)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_CENTER)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_LEFT)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_RIGHT)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_TOP)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_BOTTOM)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_CENTER_VERTICAL)
NMOSH_EXPORT_SYMBOL_INT(wxALIGN_CENTER_HORIZONTAL)
NMOSH_CONSTANT_END()

// {
}
