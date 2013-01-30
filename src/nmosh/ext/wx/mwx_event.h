#include <wx/wx.h>
#include <wx/frame.h>

class nmoshEventHandler : public wxEvtHandler {
    public:
        nmoshEventHandler(void* handler);
    private:
        void* m_handler;
        void invokeCloseEvent(wxCloseEvent &e);
        void invokeIconizeEvent(wxIconizeEvent &e);
        void invokeMenuEvent(wxCommandEvent &e);
        DECLARE_EVENT_TABLE();
};
