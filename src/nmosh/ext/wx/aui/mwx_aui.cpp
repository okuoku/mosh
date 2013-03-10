#include <nmosh/plugin-if.h>

#include <wx/wx.h>
#include <wx/aui/aui.h>

extern "C" {
// }

MOSHEXPORT
void*
mwx_aui_create(wxWindow* wnd, int flags){
    wxAuiManager* mgr;
    mgr = new wxAuiManager(wnd, flags);
    return mgr;
}

MOSHEXPORT
void
mwx_aui_paneinfo_destroy(wxAuiPaneInfo* info){
    delete info;
}

MOSHEXPORT
void*
mwx_aui_paneinfo_create(void){
    return new wxAuiPaneInfo();
}

MOSHEXPORT
void
mwx_aui_paneinfo_bestsize(wxAuiPaneInfo* info, int x, int y){
    info->BestSize(x,y);
}

MOSHEXPORT
void
mwx_aui_paneinfo_bottomdockable(wxAuiPaneInfo* info, int b){
    info->BottomDockable(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_caption(wxAuiPaneInfo* info,const char* str){
    info->Caption(wxString::FromUTF8(str));
}

MOSHEXPORT
void
mwx_aui_paneinfo_caption_visible(wxAuiPaneInfo* info, int b){
    info->CaptionVisible(b?true:false);
}

MOSHEXPORT
void /* Apply "main" pane settings */
mwx_aui_paneinfo_centrepane(wxAuiPaneInfo* info){
    info->CentrePane();
}

MOSHEXPORT
void
mwx_aui_paneinfo_closebutton(wxAuiPaneInfo* info, int b){
    info->CloseButton(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_defaultpane(wxAuiPaneInfo* info){
    info->DefaultPane();
}

MOSHEXPORT
void
mwx_aui_paneinfo_destroyonclose(wxAuiPaneInfo* info, int b){
    info->DestroyOnClose(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_direction(wxAuiPaneInfo* info, int dir){
    info->Direction(dir);
}

MOSHEXPORT
void
mwx_aui_paneinfo_dock(wxAuiPaneInfo* info){
    info->Dock();
}

MOSHEXPORT
void
mwx_aui_paneinfo_dockfixed(wxAuiPaneInfo* info, int b){
    info->DockFixed(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_dockable(wxAuiPaneInfo* info, int b){
    info->Dockable(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_fixed(wxAuiPaneInfo* info){
    info->Fixed();
}

MOSHEXPORT
void
mwx_aui_paneinfo_float(wxAuiPaneInfo* info){
    info->Float();
}

MOSHEXPORT
void
mwx_aui_paneinfo_floatable(wxAuiPaneInfo* info, int b){
    info->Floatable(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_floatingposition(wxAuiPaneInfo* info, int x, int y){
    info->FloatingPosition(x,y);
}

MOSHEXPORT
void
mwx_aui_paneinfo_floatingsize(wxAuiPaneInfo* info, int x, int y){
    info->FloatingSize(x,y);
}

MOSHEXPORT
void
mwx_aui_paneinfo_leftdockable(wxAuiPaneInfo* info, int b){
    info->LeftDockable(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_maxsize(wxAuiPaneInfo* info, int x, int y){
    info->MaxSize(x,y);
}

MOSHEXPORT
void
mwx_aui_paneinfo_maximizebutton(wxAuiPaneInfo* info, int b){
    info->MaximizeButton(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_minsize(wxAuiPaneInfo* info, int x, int y){
    info->MinSize(x,y);
}

MOSHEXPORT
void
mwx_aui_paneinfo_minimizebutton(wxAuiPaneInfo* info, int b){
    info->MinimizeButton(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_movable(wxAuiPaneInfo* info, int b){
    info->Movable(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_pinbutton(wxAuiPaneInfo* info, int b){
    info->PinButton(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_rightdockable(wxAuiPaneInfo* info, int b){
    info->RightDockable(b?true:false);
}

MOSHEXPORT
void
mwx_aui_paneinfo_show(wxAuiPaneInfo* info, int b){
    info->Show(b?true:false);
}

MOSHEXPORT
void /* Apply "toolbar" settings */
mwx_aui_paneinfo_toolbarpane(wxAuiPaneInfo* info){
    info->ToolbarPane();
}

MOSHEXPORT
void
mwx_aui_paneinfo_topdockable(wxAuiPaneInfo* info, int b){
    info->TopDockable(b?true:false);
}

MOSHEXPORT
int
mwx_aui_add(wxAuiManager* mgr, wxWindow* wnd, wxAuiPaneInfo* info){
    bool b;
    b = mgr->AddPane(wnd, *info);
    return b?1:0;
}

MOSHEXPORT
void
mwx_aui_update(wxAuiManager* mgr){
    mgr->Update();
}

NMOSH_CONSTANT_BEGIN(mwx_aui)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_NONE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_TOP)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_RIGHT)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_BOTTOM)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_LEFT)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_CENTER)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_DOCK_CENTRE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_ALLOW_FLOATING)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_ALLOW_ACTIVE_PANE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_TRANSPARENT_DRAG)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_VENETIAN_BLINDS_HINT)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_RECTANGLE_HINT)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_HINT_FADE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_NO_VENETIAN_BLINDS_FADE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_LIVE_RESIZE)
    NMOSH_EXPORT_SYMBOL_INT(wxAUI_MGR_DEFAULT)
NMOSH_CONSTANT_END()

// {
};

