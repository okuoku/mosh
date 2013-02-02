#include <wx/wx.h>
#include <wx/frame.h>

class nmoshEventHandler : public wxEvtHandler {
    public:
        nmoshEventHandler(void* handler);
        void Attach(wxWindow* wnd);
    private:
        void* m_handler;
        void invokeCloseEvent(wxCloseEvent &e);
        void invokeIconizeEvent(wxIconizeEvent &e);
        void invokeCommandEvent(wxCommandEvent &e);
        DECLARE_EVENT_TABLE();
};
