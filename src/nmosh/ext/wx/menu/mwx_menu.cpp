#include <nmosh/plugin-if.h>
#include <wx/wx.h>
#include <wx/event.h>
#include <wx/menu.h>

class nmoshMenu : public wxMenu {
    public:
        nmoshMenu(void* handler);
    private:
        void* m_handler;
        void invoke(wxCommandEvent& e);
        DECLARE_EVENT_TABLE();
};

nmoshMenu::nmoshMenu(void* handler){
    // FIXME: Call any constructor??
    m_handler = handler;
}

void
nmoshMenu::invoke(wxCommandEvent& e){
    void* obj;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_INT(NULL, e.GetId())
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    NMOSH_APPLY(m_handler, obj);
}

BEGIN_EVENT_TABLE(nmoshMenu, wxMenu)
    EVT_MENU(wxID_ANY, nmoshMenu::invoke)
END_EVENT_TABLE()

extern "C" {
// }

MOSHEXPORT
void*
mwx_menu_create(void* handler){
    return new nmoshMenu(handler);
}

// {
};
