#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/aui/auibook.h>

extern "C" {
// }

MOSHEXPORT
void*
mwx_aui_notebook_create(wxWindow* wnd, int id, int style){
    return new wxAuiNotebook(wnd, id, wxDefaultPosition, wxDefaultSize,
                             style);
}

MOSHEXPORT
int
mwx_aui_notebook_addpage(wxAuiNotebook* book, wxWindow* wnd, const char* caption, int select_p){
    bool b;
    b = book->AddPage(wnd, wxString::FromUTF8(caption), select_p?true:false,
                      wxNullBitmap);
    return b?1:0;
}

MOSHEXPORT
void
mwx_aui_notebook_deletepage(wxAuiNotebook* book, int idx){
    book->DeletePage(idx);
}

MOSHEXPORT
void
mwx_aui_notebook_removepage(wxAuiNotebook* book, int idx){
    book->RemovePage(idx);
}

MOSHEXPORT
void
mwx_aui_notebook_setselection(wxAuiNotebook* book, int idx){
    book->SetSelection(idx);
}

MOSHEXPORT
int
mwx_aui_notebook_getpageindex(wxAuiNotebook* book, wxWindow* wnd){
    return book->GetPageIndex(wnd);
}

MOSHEXPORT
void
mwx_aui_notebook_setpagetext(wxAuiNotebook* book, int idx, const char* text){
    book->SetPageText(idx, wxString::FromUTF8(text));
}

NMOSH_CONSTANT_BEGIN(mwx_auinotebook)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_DEFAULT_STYLE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_TAB_SPLIT)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_TAB_MOVE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_TAB_EXTERNAL_MOVE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_TAB_FIXED_WIDTH)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_SCROLL_BUTTONS)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_WINDOWLIST_BUTTON)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_CLOSE_BUTTON)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_CLOSE_ON_ALL_TABS)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_TOP)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_NB_BOTTOM)
NMOSH_CONSTANT_END()

// {
};
