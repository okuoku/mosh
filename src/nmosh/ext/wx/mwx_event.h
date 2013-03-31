#include <wx/wx.h>
#include <wx/frame.h>

class nmoshEventHandler : public wxEvtHandler {
    public:
        nmoshEventHandler(void* handler);
        void Attach(wxWindow* wnd);
        void AttachWithPaintable(wxWindow* wnd);
    private:
        wxWindow* m_paint_target;
        void* m_handler;
        void invokeCloseEvent(wxCloseEvent &e);
        void invokeIconizeEvent(wxIconizeEvent &e);
        void invokeCommandEvent(wxCommandEvent &e);
        void invokeDestroyEvent(wxWindowDestroyEvent &e);
        void invokePaintEvent(wxPaintEvent &e);
        DECLARE_EVENT_TABLE();
};
