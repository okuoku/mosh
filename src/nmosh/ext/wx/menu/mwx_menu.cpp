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


static void*
do_create_menu(int id, const char* text, const char* helpstring,
               int type, void* submenu){
    wxMenuItem* me;
    me = new wxMenuItem(NULL, id, text, helpstring, (wxItemKind)type, (wxMenu*)submenu);
	return me;
}

MOSHEXPORT
void*
mwx_menuitem_create_submenu(int id, const char* text, const char* helpstring,
                            void* menu){
    return do_create_menu(id,text,helpstring,wxITEM_NORMAL,menu);
}

MOSHEXPORT
void*
mwx_menuitem_create(int id, const char* text, const char* helpstring,
                    int type){
    return do_create_menu(id,text,helpstring,type,NULL);
}

MOSHEXPORT
void
mwx_menu_item_append(void* menu, void* item){
    wxMenu* me = reinterpret_cast<wxMenu *>(menu);
    wxMenuItem* i = reinterpret_cast<wxMenuItem*>(item);
    me->Append(i);
}

MOSHEXPORT
void
mwx_menu_item_append_separator(void* menu){
    wxMenu* me = reinterpret_cast<wxMenu *>(menu);
    me->AppendSeparator();
}

MOSHEXPORT
void
mwx_menu_item_delete(void* menu, int id){
    wxMenu* me = reinterpret_cast<wxMenu *>(menu);
    me->Delete(id);
}

MOSHEXPORT
void*
mwx_menu_create(void* handler){
    return new nmoshMenu(handler);
}

// {
};
