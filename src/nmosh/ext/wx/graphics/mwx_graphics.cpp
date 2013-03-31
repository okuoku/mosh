#include <nmosh/plugin-if.h>

#include <wx/wx.h>
#include <wx/graphics.h>
#if wxUSE_GRAPHICS_CONTEXT

typedef enum {
    // Context OPs
    MWX_SET_FONT, // [O R G B A]
    MWX_SET_PEN, // [O]
    MWX_SET_BRUSH, // [O]
    //  Context transforms
    MWX_BITMAP, // [O X Y W H]
    MWX_ROTATE, // [Radian]
    MWX_SCALE, // [X Y]
    MWX_IDENTITY,
    //  Text and query
    MWX_TEXT_DRAW, // [O X Y]
    MWX_TEXT_QUERY_EXTENT, // [O] => [W H Descent externalLeading]
    MWX_TEXT_QUERY_CHAR_EXTENTS, // [O Count] => [W ...]
    // PATH OPs
    MWX_PATH_STROKE, 
    MWX_PATH_FILL_ODDEVEN,
    MWX_PATH_FILL_WINDING,
    MWX_PATH_BEGIN,
    MWX_PATH_END,
    MWX_PATH_CLOSE,
    MWX_PATH_MOVETO, // [X Y]
    MWX_PATH_ARC_CW, // [X Y R startAngle endAngle]
    MWX_PATH_ARC_CCW, // [X Y R startAngle endAngle]
    MWX_PATH_ARCTO, // [X1 Y1 X2 Y2 R]
    MWX_PATH_CIRCLE, // [X Y R]
    MWX_PATH_CURVETO, // [CX1 CY1 CX2 CY1 X Y]
    MWX_PATH_ELLIPSE, // [X Y W H]
    MWX_PATH_LINETO, // [X Y]
    MWX_PATH_RECT, // [X Y W H]
    MWX_PATH_RRECT, // [X Y W H R]
}mwx_graphics_drawop;


#define OP (op_ptr++,(mwx_graphics_drawop)op[op_ptr-1])
#define OBJINT (obj_ptr++,reinterpret_cast<intptr_t>((void*)obj[obj_ptr-1]))
#define OBJUINT (obj_ptr++,reinterpret_cast<uintptr_t>((void*)obj[obj_ptr-1]))
#define OBJFONT (obj_ptr++,reinterpret_cast<wxFont*>((void*)obj[obj_ptr-1]))
#define OBJBRUSH (obj_ptr++,reinterpret_cast<wxBrush*>((void*)obj[obj_ptr-1]))
#define OBJBITMAP (obj_ptr++,reinterpret_cast<wxBitmap*>((void*)obj[obj_ptr-1]))
#define OBJPEN (obj_ptr++,reinterpret_cast<wxPen*>((void*)obj[obj_ptr-1]))
#define OBJTEXT (obj_ptr++,reinterpret_cast<const char*>((void*)obj[obj_ptr-1]))
#define VTX (vtx_ptr++,vtx[vtx_ptr-1])
#define QRET *(query_ptr++,&query[query_ptr-1])

static int /* 0 for success */
kick_pathop(wxGraphicsContext* ctx, 
            unsigned char* op, int* inout_op_ptr,
            double* vtx, int* inout_vtx_ptr,
            uintptr_t* obj, int* inout_obj_ptr){
    int op_ptr = 0;
    int vtx_ptr = 0;
    int obj_ptr = 0;
    int query_ptr = 0;
    mwx_graphics_drawop dop;
    wxGraphicsPath pth = ctx->CreatePath();
    dop = OP;
    for(;;dop = OP){
        int r;
        switch(dop){
            case MWX_SET_PEN: // [O]
                ctx->SetPen(*OBJPEN);
                break;
            case MWX_SET_BRUSH: // [O]
                ctx->SetBrush(*OBJBRUSH);
                break;
            // PATH OPs
            case MWX_PATH_STROKE: 
                ctx->StrokePath(pth);
                break;
            case MWX_PATH_FILL_ODDEVEN:
                ctx->FillPath(pth, wxODDEVEN_RULE);
                break;
            case MWX_PATH_FILL_WINDING:
                ctx->FillPath(pth, wxWINDING_RULE);
                break;
            case MWX_PATH_END:
                *inout_op_ptr = op_ptr;
                *inout_vtx_ptr = vtx_ptr;
                *inout_obj_ptr = obj_ptr;
                return 0;
            case MWX_PATH_CLOSE:
                pth.CloseSubpath();
                break;
            case MWX_PATH_MOVETO: // [X Y]
                {
                    double x = VTX;
                    double y = VTX;
                    pth.MoveToPoint(x,y);
                }
                break;
            case MWX_PATH_ARC_CW: // [X Y R startAngle endAngle]
                {
                    double x = VTX;
                    double y = VTX;
                    double r = VTX;
                    double start = VTX;
                    double end = VTX;
                    pth.AddArc(x,y,r,start,end,true);
                }
                break;
            case MWX_PATH_ARC_CCW: // [X Y R startAngle endAngle]
                {
                    double x = VTX;
                    double y = VTX;
                    double r = VTX;
                    double start = VTX;
                    double end = VTX;
                    pth.AddArc(x,y,r,start,end,false);
                }
                break;
            case MWX_PATH_ARCTO: // [X1 Y1 X2 Y2 R]
                {
                    double x1 = VTX;
                    double y1 = VTX;
                    double x2 = VTX;
                    double y2 = VTX;
                    double r = VTX;
                    pth.AddArcToPoint(x1,y1,x2,y2,r);
                }
                break;
            case MWX_PATH_CIRCLE: // [X Y R]
                {
                    double x = VTX;
                    double y = VTX;
                    double r = VTX;
                    pth.AddCircle(x,y,r);
                }
                break;
            case MWX_PATH_CURVETO: // [CX1 CY1 CX2 CY1 X Y]
                {
                    double cx1 = VTX;
                    double cy1 = VTX;
                    double cx2 = VTX;
                    double cy2 = VTX;
                    double x = VTX;
                    double y = VTX;
                    pth.AddCurveToPoint(cx1,cy1,cx2,cy2,x,y);
                }
                break;
            case MWX_PATH_ELLIPSE: // [X Y W H]
                {
                    double x = VTX;
                    double y = VTX;
                    double w = VTX;
                    double h = VTX;
                    pth.AddEllipse(x,y,w,h);
                }
                break;
            case MWX_PATH_LINETO: // [X Y]
                {
                    double x = VTX;
                    double y = VTX;
                    pth.AddLineToPoint(x,y);
                }
                break;
            case MWX_PATH_RECT: // [X Y W H]
                {
                    double x = VTX;
                    double y = VTX;
                    double w = VTX;
                    double h = VTX;
                    pth.AddRectangle(x,y,w,h);
                }
                break;
            case MWX_PATH_RRECT: // [X Y W H R]
                {
                    double x = VTX;
                    double y = VTX;
                    double w = VTX;
                    double h = VTX;
                    double r = VTX;
                    pth.AddRoundedRectangle(x,y,w,h,r);
                }
                break;
            default:
                return -1;
        }
    }
}

static int /* 0 for success */
kick(wxGraphicsContext* ctx, 
     unsigned char* op, int count_op,
     double* vtx, int count_vtx,
     uintptr_t* obj, int count_obj,
     double* query, int count_query){
    int op_ptr = 0;
    int vtx_ptr = 0;
    int obj_ptr = 0;
    int query_ptr = 0;
    mwx_graphics_drawop dop;
    dop = OP;
    for(int i=0;i!=count_op;dop = OP){
        int r;
        int op_next_ptr;
        int vtx_next_ptr;
        int obj_next_ptr;
        switch(dop){
            case MWX_PATH_BEGIN:
                // Kick path ops
                op_next_ptr = op_ptr;
                obj_next_ptr = obj_ptr;
                vtx_next_ptr = vtx_ptr;
                r = kick_pathop(ctx, op, &op_next_ptr,
                                vtx, &vtx_next_ptr,
                                obj, &obj_next_ptr);
                if(r){
                    return r;
                }
                op_ptr = op_next_ptr;
                obj_ptr = obj_next_ptr;
                vtx_ptr = vtx_next_ptr;
                break;
            // Context OPs
            case MWX_SET_FONT: // [O R G B A]
                { 
                    const wxFont& font = *OBJFONT;
                    int r = OBJUINT;
                    int g = OBJUINT;
                    int b = OBJUINT;
                    int a = OBJUINT;
                    const wxColour col = wxColour(r,g,b,a);
                    ctx->SetFont(font,col);
                }
                break;
            case MWX_SET_PEN: // [O]
                ctx->SetPen(*OBJPEN);
                break;
            case MWX_SET_BRUSH: // [O]
                ctx->SetBrush(*OBJBRUSH);
                break;
            case MWX_BITMAP: // [O X Y W H]
                {
                    const wxBitmap& bmp = *OBJBITMAP;
                    double x = VTX;
                    double y = VTX;
                    double w = VTX;
                    double h = VTX;
                    ctx->DrawBitmap(bmp,x,y,w,h);
                }
                break;
            //  Context transforms
            case MWX_ROTATE: // [Radian]
                ctx->Rotate(VTX);
                break;
            case MWX_SCALE: // [X Y]
                {
                    double x = VTX;
                    double y = VTX;
                    ctx->Scale(x,y);
                }
                break;
            case MWX_IDENTITY:
                {
                    wxGraphicsMatrix mtx = ctx->GetTransform();
                    mtx.Set(); // Make identity mtx
                    ctx->SetTransform(mtx);
                }
                break;
                //  Text and query
            case MWX_TEXT_DRAW: // [O X Y]
                {
                    const char* text = OBJTEXT;
                    double x = VTX;
                    double y = VTX;
                    ctx->DrawText(wxString::FromUTF8(text),x,y);
                }
                break;
            case MWX_TEXT_QUERY_EXTENT: // [O] => [W H Descent externalLeading]
                {
                    const char* text = OBJTEXT;
                    double w,h,dec,ex;
                    ctx->GetTextExtent(wxString::FromUTF8(text),&w,&h,&dec,&ex);
                    QRET = w;
                    QRET = h;
                    QRET = dec;
                    QRET = ex;
                }
                break;
            case MWX_TEXT_QUERY_CHAR_EXTENTS: // [O Count] => [W ...]
                {
                    const char* text = OBJTEXT;
                    unsigned int count = OBJUINT;
                    wxArrayDouble ad;
                    ctx->GetPartialTextExtents(wxString::FromUTF8(text),ad);
                    for(int j = 0;j!=count;j++){
                        QRET = ad[j];
                    }
                }
                break;
            default: // INVALID
                return -1;
        }
    }
    // Sanity check
    if(count_query && (query_ptr > count_query)){
        return -2;
    }
    if(count_op && (op_ptr > count_op)){
        return -3;
    }
    if(count_vtx && (vtx_ptr > count_vtx)){
        return -4;
    }
    if(count_obj && (obj_ptr > count_obj)){
        return -5;
    }
    return 0;
}

static wxGraphicsRenderer*
me(){
    return wxGraphicsRenderer::GetDefaultRenderer();
}

extern "C" {
// }

MOSHEXPORT
int /* 0 for success */
mwx_graphics_kick_onpaint(wxWindowDC* dc,
                  unsigned char* op, int count_op,
                  double* vtx, int count_vtx,
                  uintptr_t* obj, int count_obj,
                  double* query, int count_query){
    return kick(me()->CreateContext(dc),op,count_op,
                vtx,count_vtx,obj,count_obj,query,count_query);
}

MOSHEXPORT
int /* 0 for success */
mwx_graphics_kick(wxWindow* wnd,
                  unsigned char* op, int count_op,
                  double* vtx, int count_vtx,
                  uintptr_t* obj, int count_obj,
                  double* query, int count_query){
    return kick(me()->CreateContext(wnd),op,count_op,
                vtx,count_vtx,obj,count_obj,query,count_query);
}

MOSHEXPORT
void
mwx_bitmap_destroy(wxBitmap* bm){
    delete bm;
}

MOSHEXPORT
void*
mwx_bitmap_create(int w, int h, int depth /* -1 for display depth */){
    return new wxBitmap(w,h,depth);
}

MOSHEXPORT
void*
mwx_bitmap_from_image(wxImage* im){
    return new wxBitmap(*im);
}

MOSHEXPORT
void
mwx_image_init_handlers(void){
    ::wxInitAllImageHandlers();
}

MOSHEXPORT
void*
mwx_image_load(const char* path){
    return new wxImage(wxString::FromUTF8(path));
}

MOSHEXPORT
void
mwx_image_destroy(wxImage* im){
    delete im;
}

MOSHEXPORT
void
mwx_brush_destroy(wxBrush* br){
    delete br;
}
MOSHEXPORT
void*
mwx_brush_create(int r, int g, int b, int a, int style){
    return new wxBrush(wxColour(r,g,b,a), style);
}

MOSHEXPORT
void*
mwx_pen_create(int r, int g, int b, int a, int width, int style){
    return new wxPen(wxColour(r,g,b,a), width, style);
}

MOSHEXPORT
void
mwx_pen_destroy(wxPen* pen){
    delete pen;
}

NMOSH_CONSTANT_BEGIN(mwx_graphics)
    // Context OPs
    NMOSH_EXPORT_SYMBOL_INT(MWX_SET_FONT) // [O R G B A]
    NMOSH_EXPORT_SYMBOL_INT(MWX_SET_PEN) // [O]
    NMOSH_EXPORT_SYMBOL_INT(MWX_SET_BRUSH) // [O]
    //  Context transforms
    NMOSH_EXPORT_SYMBOL_INT(MWX_BITMAP) // [O X Y W H]
    NMOSH_EXPORT_SYMBOL_INT(MWX_ROTATE) // [Radian]
    NMOSH_EXPORT_SYMBOL_INT(MWX_SCALE) // [X Y]
    NMOSH_EXPORT_SYMBOL_INT(MWX_IDENTITY)
    //  Text and query
    NMOSH_EXPORT_SYMBOL_INT(MWX_TEXT_DRAW) // [O X Y]
    NMOSH_EXPORT_SYMBOL_INT(MWX_TEXT_QUERY_EXTENT) // [O] => [W H Descent externalLeading]
    NMOSH_EXPORT_SYMBOL_INT(MWX_TEXT_QUERY_CHAR_EXTENTS) // [O Count] => [W ...]
    // PATH OPs
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_STROKE) 
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_FILL_ODDEVEN)
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_FILL_WINDING)
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_BEGIN)
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_END)
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_CLOSE)
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_MOVETO) // [X Y]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_ARC_CW) // [X Y R startAngle endAngle]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_ARC_CCW) // [X Y R startAngle endAngle]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_ARCTO) // [X1 Y1 X2 Y2 R]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_CIRCLE) // [X Y R]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_CURVETO) // [CX1 CY1 CX2 CY1 X Y]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_ELLIPSE) // [X Y W H]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_LINETO) // [X Y]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_RECT) // [X Y W H]
    NMOSH_EXPORT_SYMBOL_INT(MWX_PATH_RRECT) // [X Y W H R]
    NMOSH_EXPORT_SYMBOL_INT(wxSOLID)
    NMOSH_EXPORT_SYMBOL_INT(wxTRANSPARENT)
    NMOSH_EXPORT_SYMBOL_INT(wxDOT)
    NMOSH_EXPORT_SYMBOL_INT(wxLONG_DASH)
    NMOSH_EXPORT_SYMBOL_INT(wxSHORT_DASH)
    NMOSH_EXPORT_SYMBOL_INT(wxDOT_DASH)
    NMOSH_EXPORT_SYMBOL_INT(wxSTIPPLE)
    NMOSH_EXPORT_SYMBOL_INT(wxUSER_DASH)
    NMOSH_EXPORT_SYMBOL_INT(wxBDIAGONAL_HATCH)
    NMOSH_EXPORT_SYMBOL_INT(wxCROSSDIAG_HATCH)
    NMOSH_EXPORT_SYMBOL_INT(wxFDIAGONAL_HATCH)
    NMOSH_EXPORT_SYMBOL_INT(wxCROSS_HATCH)
    NMOSH_EXPORT_SYMBOL_INT(wxHORIZONTAL_HATCH)
    NMOSH_EXPORT_SYMBOL_INT(wxVERTICAL_HATCH)
NMOSH_CONSTANT_END()

// {
};
#else
#warning Graphics context feature was not found!
#endif
