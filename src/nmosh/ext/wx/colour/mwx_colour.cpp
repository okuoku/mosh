#include <nmosh/plugin-if.h>

#include <wx/wx.h>
#include <wx/colour.h>

extern "C" {
// }

MOSHEXPORT
void*
mwx_colour_null(void){
    return &wxNullColour;
}

MOSHEXPORT
void*
mwx_colour_create(int r, int g, int b, int a){
    return new wxColour(r,g,b,a);
}

MOSHEXPORT
void
mwx_colour_destroy(wxColour* col){
    delete col;
}

// {
};
