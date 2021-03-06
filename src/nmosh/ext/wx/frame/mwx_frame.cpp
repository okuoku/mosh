#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/frame.h>
#include "mwx_event.h"

class nmoshFrame : public wxFrame {
    public:
        nmoshFrame(wxWindow* parent, const wxString &title,
                   const wxPoint &pos, const wxSize &size,
                   long style, const wxString &name)
            : wxFrame(parent,wxID_ANY,title,pos,size,style,name){};
    private:
        void invokeCloseEvent(wxCloseEvent &e);
        void invokeIconizeEvent(wxIconizeEvent &e);
};

extern "C" {
// }

MOSHEXPORT
void*
mwx_frame_create_statusbar(void* frame){
    nmoshFrame* frm = reinterpret_cast<nmoshFrame *>(frame);
    return frm->CreateStatusBar();
}

MOSHEXPORT
void
mwx_frame_set_menubar(void* frame, void* menubar){
    nmoshFrame* frm = reinterpret_cast<nmoshFrame *>(frame);
    wxMenuBar* menu = reinterpret_cast<wxMenuBar *>(menubar);
    frm->SetMenuBar(menu);
}

MOSHEXPORT
void*
mwx_frame_create(void* handler, 
                 const char* title,
                 int xoff, int yoff, 
                 int xwidth, int ywidth,
                 const char* name, void* parent, int style){
    nmoshFrame* me;
    nmoshEventHandler* eh;
    wxPoint pos;
    wxSize siz;
    if(!xoff && !yoff){
        pos = wxDefaultPosition;
    }else{
        pos = wxPoint(xoff,yoff);
    }
    if(!xwidth && !ywidth){
        siz = wxDefaultSize;
    }else{
        siz = wxSize(xwidth, ywidth);
    }
    me = new nmoshFrame((wxWindow *)parent,
                        wxString::FromUTF8(title),
                        pos, siz, style, 
                        wxString::FromUTF8(name));
    eh = new nmoshEventHandler(handler);
    eh->Attach(me);
    return me;
}

NMOSH_CONSTANT_BEGIN(mwx_frame)
    NMOSH_EXPORT_SYMBOL_INT(wxDEFAULT_FRAME_STYLE)
    NMOSH_EXPORT_SYMBOL_INT(wxICONIZE)
    NMOSH_EXPORT_SYMBOL_INT(wxCAPTION)
    NMOSH_EXPORT_SYMBOL_INT(wxMINIMIZE)
    NMOSH_EXPORT_SYMBOL_INT(wxMINIMIZE_BOX)
    NMOSH_EXPORT_SYMBOL_INT(wxMAXIMIZE)
    NMOSH_EXPORT_SYMBOL_INT(wxMAXIMIZE_BOX)
    NMOSH_EXPORT_SYMBOL_INT(wxCLOSE_BOX)
    NMOSH_EXPORT_SYMBOL_INT(wxSTAY_ON_TOP)
    NMOSH_EXPORT_SYMBOL_INT(wxSYSTEM_MENU)
    NMOSH_EXPORT_SYMBOL_INT(wxRESIZE_BORDER)
    NMOSH_EXPORT_SYMBOL_INT(wxFRAME_TOOL_WINDOW)
    NMOSH_EXPORT_SYMBOL_INT(wxFRAME_NO_TASKBAR)
    NMOSH_EXPORT_SYMBOL_INT(wxFRAME_FLOAT_ON_PARENT)
NMOSH_CONSTANT_END()

// {
};
