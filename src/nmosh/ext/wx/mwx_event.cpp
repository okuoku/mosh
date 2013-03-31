#include <nmosh/plugin-if.h>
#include <wx/dcbuffer.h>
#include "mwx_event.h"

BEGIN_EVENT_TABLE(nmoshEventHandler, wxEvtHandler)
    //EVT_TEXT(wxID_ANY, nmoshEventHandler::invokeCommandEvent)
    EVT_TEXT_ENTER(wxID_ANY, nmoshEventHandler::invokeCommandEvent)
    EVT_ICONIZE(nmoshEventHandler::invokeIconizeEvent)
    EVT_CLOSE(nmoshEventHandler::invokeCloseEvent)
    EVT_MENU(wxID_ANY, nmoshEventHandler::invokeCommandEvent)
    EVT_WINDOW_DESTROY(nmoshEventHandler::invokeDestroyEvent)
    EVT_PAINT(nmoshEventHandler::invokePaintEvent)
    EVT_MOUSE_EVENTS(nmoshEventHandler::invokeMouseEvent)
END_EVENT_TABLE()

// Constructor
nmoshEventHandler::nmoshEventHandler(void *handler){
    m_handler = handler;
}

void
nmoshEventHandler::invokePaintEvent(wxPaintEvent &e){
    void* obj;
    if(m_paint_target){ // Speed hack.
        //wxAutoBufferedPaintDC dc(m_paint_target);
        wxPaintDC dc(m_paint_target);
        NMOSH_EXPORT_BEGIN(param)
            NMOSH_EXPORT_CSTRING(NULL, "paint")
            NMOSH_EXPORT_POINTER(NULL, &e)
            NMOSH_EXPORT_POINTER(NULL, &dc)
        NMOSH_EXPORT_END()
        obj = NMOSH_EXPORT(param);
        NMOSH_APPLY(m_handler, obj);
    }
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
nmoshEventHandler::invokeMouseEvent(wxMouseEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_CSTRING(NULL, "mouse")
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
    m_paint_target = NULL;
    this->SetNextHandler(wnd->GetEventHandler());
    wnd->SetEventHandler(this);
}

void
nmoshEventHandler::AttachWithPaintable(wxWindow* wnd){
    m_paint_target = wnd;
    this->SetNextHandler(wnd->GetEventHandler());
    wnd->SetEventHandler(this);
}

extern "C" {
// }

MOSHEXPORT
void
mwx_event_skip(wxEvent* e){
    e->Skip();
}

MOSHEXPORT
int
mwx_event_type(wxEvent* e){
    return e->GetEventType();
}

MOSHEXPORT
int
mwx_event_mouse_wheel_axis(wxMouseEvent* e){
    return e->GetWheelAxis();
}

MOSHEXPORT
int
mwx_event_mouse_wheel_delta(wxMouseEvent* e){
    return e->GetWheelDelta();
}

MOSHEXPORT
int
mwx_event_mouse_x(wxMouseEvent* e){
    return e->GetX();
}

MOSHEXPORT
int
mwx_event_mouse_y(wxMouseEvent* e){
    return e->GetY();
}

NMOSH_CONSTANT_BEGIN(mwx_event)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_COMMAND_TEXT_ENTER)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_ENTER_WINDOW)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_LEAVE_WINDOW)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_AUX2_UP)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_AUX2_DOWN)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_AUX2_DCLICK)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_AUX1_UP)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_AUX1_DOWN)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_AUX1_DCLICK)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_RIGHT_UP)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_RIGHT_DOWN)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_RIGHT_DCLICK)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_MIDDLE_UP)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_MIDDLE_DOWN)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_MIDDLE_DCLICK)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_LEFT_UP)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_LEFT_DOWN)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_LEFT_DCLICK)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_MOTION)
    NMOSH_EXPORT_SYMBOL_INT(wxEVT_MOUSEWHEEL)
    NMOSH_EXPORT_SYMBOL_INT(wxMOUSE_WHEEL_VERTICAL)
    NMOSH_EXPORT_SYMBOL_INT(wxMOUSE_WHEEL_HORIZONTAL)
NMOSH_CONSTANT_END()

// {
}
