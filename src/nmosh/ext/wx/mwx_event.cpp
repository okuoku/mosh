#include <nmosh/plugin-if.h>
#include "mwx_event.h"

BEGIN_EVENT_TABLE(nmoshEventHandler, wxEvtHandler)
    EVT_ICONIZE(nmoshEventHandler::invokeIconizeEvent)
    EVT_CLOSE(nmoshEventHandler::invokeCloseEvent)
    EVT_MENU(wxID_ANY, nmoshEventHandler::invokeMenuEvent)
END_EVENT_TABLE()

// Constructor
nmoshEventHandler::nmoshEventHandler(void *handler){
    m_handler = handler;
}

void
nmoshEventHandler::invokeMenuEvent(wxCommandEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_INT(NULL, e.GetId())
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
        NMOSH_EXPORT_INT(NULL, e.GetLoggingOff()?1:0)
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

