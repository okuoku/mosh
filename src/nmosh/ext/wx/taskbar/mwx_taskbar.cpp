#include <nmosh/plugin-if.h>

#include <wx/wx.h>
#include <wx/taskbar.h>

class nmoshTrayIcon : public wxTaskBarIcon {
    public:
        nmoshTrayIcon(void* handler);
    protected:
        virtual wxMenu* CreatePopupMenu();
    private:
        void* m_handler;
};

nmoshTrayIcon::nmoshTrayIcon(void* handler){
    m_handler = handler;
}

wxMenu*
nmoshTrayIcon::CreatePopupMenu(){
    void* obj;
    uintptr_t ret;
    NMOSH_EXPORT_BEGIN(param)
    NMOSH_EXPORT_END()
    obj = NMOSH_EXPORT(param);
    ret = NMOSH_APPLY(m_handler, obj);
    return reinterpret_cast<wxMenu*>(ret);
}

extern "C"{
// }

MOSHEXPORT
void
mwx_trayicon_seticon(void* me, void* icon, const char* tooltip){
    nmoshTrayIcon* ti = reinterpret_cast<nmoshTrayIcon *>(me);
    if(icon){
		// Create a copy for Icon.
        const wxIcon i = *reinterpret_cast<wxIcon *>(icon);
        const wxString tip = wxString::FromUTF8(tooltip);
        ti->SetIcon(i, tip);
    }else{
        ti->RemoveIcon();
    }
}

MOSHEXPORT
void*
mwx_trayicon_create(void*handler){
    return new nmoshTrayIcon(handler);
}

// {
}
