#include <nmosh/plugin-if.h>

#include <wx/wx.h>
#include <wx/artprov.h>

extern "C" {
//}

MOSHEXPORT
void*
mwx_icon_create_stock(void* id, int size){
    wxSize siz;
    if(size == 0){
        siz = wxDefaultSize;
    }else{
        siz = wxSize(size,size);
    }
    const wxIcon* ico = // FIXME: Assume const char* for asset ID...
        new wxIcon(wxArtProvider::GetIcon(reinterpret_cast<const char *>(id), 
                               wxART_BUTTON, siz));
    return (void*)ico;
}

NMOSH_CONSTANT_BEGIN(mwx_icon)
NMOSH_EXPORT_SYMBOL_POINTER(wxART_HELP)
NMOSH_CONSTANT_END()

//{
}
