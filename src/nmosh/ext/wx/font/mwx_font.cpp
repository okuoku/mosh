#include <nmosh/plugin-if.h>

#include <wx/wx.h>

extern "C" {
// }


void*
mwx_font_create(int point, int family, int style, int weight, 
                int underline_p, const char* name, int encoding){
    return wxFont::New(point, (wxFontFamily)family, style,
                       (wxFontWeight)weight, (underline_p)?true:false,
                       name, (wxFontEncoding) encoding);
}

void
mwx_font_destroy(wxFont* fnt){
    delete fnt;
}

void*
mwx_font_copy(const wxFont* fnt){
    return new wxFont(*fnt);
}

int
mwx_font_is_fixedwith(const wxFont* fnt){
    return fnt->IsFixedWidth()?1:0;
}



NMOSH_CONSTANT_BEGIN(mwx_font)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_DEFAULT)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_DECORATIVE)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_ROMAN)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_SCRIPT)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_SWISS)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_MODERN)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFAMILY_TELETYPE)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTWEIGHT_NORMAL)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTWEIGHT_LIGHT)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTWEIGHT_BOLD)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_ITALIC)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_SLANT)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_LIGHT)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_BOLD)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_ANTIALIASED)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_NOT_ANTIALIASED)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_UNDERLINED)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTFLAG_STRIKETHROUGH)
    NMOSH_EXPORT_SYMBOL_INT(wxFONTENCODING_SYSTEM)
NMOSH_CONSTANT_END()

// {
};
