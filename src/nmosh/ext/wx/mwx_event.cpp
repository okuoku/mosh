#include <nmosh/plugin-if.h>
#include "mwx_event.h"

BEGIN_EVENT_TABLE(nmoshEventHandler, wxEvtHandler)
    //EVT_TEXT(wxID_ANY, nmoshEventHandler::invokeCommandEvent)
    EVT_TEXT_ENTER(wxID_ANY, nmoshEventHandler::invokeCommandEvent)
    EVT_ICONIZE(nmoshEventHandler::invokeIconizeEvent)
    EVT_CLOSE(nmoshEventHandler::invokeCloseEvent)
    EVT_MENU(wxID_ANY, nmoshEventHandler::invokeCommandEvent)
    EVT_WINDOW_DESTROY(nmoshEventHandler::invokeDestroyEvent)
END_EVENT_TABLE()

// Constructor
nmoshEventHandler::nmoshEventHandler(void *handler){
    m_handler = handler;
}

void
nmoshEventHandler::invokeCommandEvent(wxCommandEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_INT(NULL, e.GetId())
        NMOSH_EXPORT_POINTER(NULL, &e)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

void
nmoshEventHandler::invokeCloseEvent(wxCloseEvent &e){
    void* obj;
    uintptr_t r;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_CSTRING(NULL, "close")
        NMOSH_EXPORT_INT(NULL, e.CanVeto()?1:0)
        //NMOSH_EXPORT_INT(NULL, e.GetLoggingOff()?1:0)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    r = NMOSH_APPLY(m_handler, obj); // 0 = Veto Quit event
    if(r == 0){
        e.Veto();
    }
}

void
nmoshEventHandler::invokeIconizeEvent(wxIconizeEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_CSTRING(NULL, "iconize")
        NMOSH_EXPORT_INT(NULL, e.IsIconized()?1:0)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

void
nmoshEventHandler::invokeDestroyEvent(wxWindowDestroyEvent &e){
    // FIXME: Free scheme closure here
    e.GetWindow()->SetEventHandler(e.GetWindow());
}

void
nmoshEventHandler::Attach(wxWindow* wnd){
    this->SetNextHandler(wnd->GetEventHandler());
    wnd->SetEventHandler(this);
}
