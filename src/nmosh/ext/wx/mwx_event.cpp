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
    EVT_SET_FOCUS(nmoshEventHandler::invokeSetFocusEvent)
    EVT_KILL_FOCUS(nmoshEventHandler::invokeKillFocusEvent)
END_EVENT_TABLE()

// Common event header
#define EVENT_HEADER(e) \
    NMOSH_EXPORT_INT(NULL, e.GetEventType()) \
    NMOSH_EXPORT_POINTER(NULL, &e)


// Constructor
nmoshEventHandler::nmoshEventHandler(void *handler){
    m_handler = handler;
}

void
nmoshEventHandler::invokePaintEvent(wxPaintEvent &e){
    void* obj;
    if(m_paint_target){ // Speed hack.
        wxAutoBufferedPaintDC dc(m_paint_target);
        NMOSH_EXPORT_BEGIN(param)
            EVENT_HEADER(e)
            NMOSH_EXPORT_POINTER(NULL, &dc)
        NMOSH_EXPORT_END()
        obj = NMOSH_EXPORT(param);
        NMOSH_APPLY(m_handler, obj);
    }else{
		e.Skip();
    }
}

void
nmoshEventHandler::invokeCommandEvent(wxCommandEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        EVENT_HEADER(e)
        NMOSH_EXPORT_INT(NULL, e.GetId())
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

void
nmoshEventHandler::invokeMouseEvent(wxMouseEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        EVENT_HEADER(e)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

void
nmoshEventHandler::invokeCloseEvent(wxCloseEvent &e){
    void* obj;
    uintptr_t r;
    NMOSH_EXPORT_BEGIN(param)
        EVENT_HEADER(e)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

void
nmoshEventHandler::invokeIconizeEvent(wxIconizeEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        EVENT_HEADER(e)
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
nmoshEventHandler::invokeKillFocusEvent(wxFocusEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        EVENT_HEADER(e)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

void
nmoshEventHandler::invokeSetFocusEvent(wxFocusEvent &e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        EVENT_HEADER(e)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
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
void* /* Scheme object */
mwx_event_acquire_ids(void){
/* Hack nmosh-stub-gen.sps */
#define EXP(x) NMOSH_EXPORT_SYMBOL_INT(x)
    /* wx 2.9 dynamically(?) assigns event ids. get it runtime */
    NMOSH_EXPORT_BEGIN(ret)
        EXP(wxEVT_CLOSE_WINDOW)
#if wxCHECK_VERSION(2,9,0)
#else
#define wxEVT_TEXT_ENTER wxEVT_COMMAND_TEXT_ENTER
#endif
        EXP(wxEVT_TEXT_ENTER)
        EXP(wxEVT_ENTER_WINDOW)
        EXP(wxEVT_LEAVE_WINDOW)
        EXP(wxEVT_RIGHT_UP)
        EXP(wxEVT_RIGHT_DOWN)
        EXP(wxEVT_RIGHT_DCLICK)
        EXP(wxEVT_MIDDLE_UP)
        EXP(wxEVT_MIDDLE_DOWN)
        EXP(wxEVT_MIDDLE_DCLICK)
        EXP(wxEVT_LEFT_UP)
        EXP(wxEVT_LEFT_DOWN)
        EXP(wxEVT_LEFT_DCLICK)
        EXP(wxEVT_MOTION)
        EXP(wxEVT_MOUSEWHEEL)
#if wxCHECK_VERSION(2,9,0)
        EXP(wxEVT_AUX2_UP)
        EXP(wxEVT_AUX2_DOWN)
        EXP(wxEVT_AUX2_DCLICK)
        EXP(wxEVT_AUX1_UP)
        EXP(wxEVT_AUX1_DOWN)
        EXP(wxEVT_AUX1_DCLICK)
#endif
    NMOSH_EXPORT_END()
    return NMOSH_EXPORT(ret);
}

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
#if wxCHECK_VERSION(2,9,0)
    return e->GetWheelAxis();
#else
    return 0;
#endif
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
#if wxCHECK_VERSION(2,9,0)
    NMOSH_EXPORT_SYMBOL_INT(wxMOUSE_WHEEL_VERTICAL)
    NMOSH_EXPORT_SYMBOL_INT(wxMOUSE_WHEEL_HORIZONTAL)
#endif
NMOSH_CONSTANT_END()

// {
}
