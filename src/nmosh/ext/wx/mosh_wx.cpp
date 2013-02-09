#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/app.h>
#include <wx/frame.h>

class nmoshApp : public wxApp {
    public:
        nmoshApp();
    private:
        virtual bool OnInit();
};

nmoshApp::nmoshApp() { 
    SetExitOnFrameDelete(false);
} ;

void* init_callback;
void* init_data;

bool
nmoshApp::OnInit(){
    uintptr_t c;
    c = NMOSH_APPLY(init_callback, init_data);
    if(c){
        /* Clear init data to make consevertive GC happy */
        init_callback = NULL;
        init_data = NULL;
        return true; /* Continue execution */
    }else{
        return false; /* Exit immediately */
    }
}

IMPLEMENT_APP_NO_MAIN(nmoshApp)


extern "C" {
// }

MOSHEXPORT
void
mwx_startapp(void* callback, void* data){
    char* bogus = NULL;
    int zero = 0;
    init_callback = callback;
    init_data = data;
    wxEntry(zero, &bogus);
}

MOSHEXPORT
void
mwx_app_exit(void){
    wxGetApp().ExitMainLoop();
}

/* Exported functions */
MOSHEXPORT
void
mwx_base_deinit(void){
    /* This is only for non-GUI apps */
    wxUninitialize();
}

MOSHEXPORT
int /* 0 = success */
mwx_base_init(void){
    /* This is only for non-GUI apps */
    bool b;
    b = wxInitialize();
    return (b)?0:-1;
}

NMOSH_PLUGIN_DECLARE(mwx_window);
NMOSH_PLUGIN_DECLARE(mwx_icon);
NMOSH_PLUGIN_DECLARE(mwx_frame);
NMOSH_PLUGIN_DECLARE(mwx_sizer);
NMOSH_PLUGIN_DECLARE(mwx_text);
NMOSH_PLUGIN_DECLARE(mwx_event);
NMOSH_PLUGIN_DECLARE(mwx_font);

NMOSH_CONSTANT_BEGIN(mosh_wx)
NMOSH_EXPORT_SYMBOL_INT(wxID_NEW)
NMOSH_EXPORT_SYMBOL_INT(wxID_EXIT)
NMOSH_EXPORT_SYMBOL_INT(wxITEM_NORMAL)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_icon)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_frame)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_sizer)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_text)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_event)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_window)
NMOSH_EXPORT_SYMBOL_PLUGIN(mwx_font)
NMOSH_CONSTANT_END()

NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(mosh_wx);

// {
}
