#include <nmosh/plugin-if.h>

#include <wx/wx.h>
#include <wx/graphics.h>
#if wxUSE_GRAPHICS_CONTEXT

typedef enum {
    // Context OPs
    MWX_SET_FONT, // [O]
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
    MWX_TEXT_QUERY_CHAR_EXTENTS, // [O Count] => [Count W ...]
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
#define OBJ(type) (obj_ptr++,reinterpret_cast<type>((void*)obj[obj_ptr-1]))
#define VTX (vtx_ptr++,vtx[vtx_ptr-1])

static int /* 0 for success */
kick_pathop(wxGraphicsContext* ctx, 
            unsigned char* op, int* inout_op_ptr,
            double* vtx, int* inout_vtx_ptr,
            uintptr_t* obj, int* inout_obj_ptr){
    int op_ptr = 0;
    int vtx_ptr = 0;
    int obj_ptr = 0;
    mwx_graphics_drawop dop;
    dop = OP;
    for(;;dop = OP){
        int r;
        switch(dop){
            // PATH OPs
            case MWX_PATH_STROKE: 
                break;
            case MWX_PATH_FILL_ODDEVEN:
                break;
            case MWX_PATH_FILL_WINDING:
                break;
            case MWX_PATH_END:
                *inout_op_ptr = op_ptr;
                *inout_vtx_ptr = vtx_ptr;
                *inout_obj_ptr = obj_ptr;
                return 0;
            case MWX_PATH_CLOSE:
                break;
            case MWX_PATH_MOVETO: // [X Y]
                break;
            case MWX_PATH_ARC_CW: // [X Y R startAngle endAngle]
                break;
            case MWX_PATH_ARC_CCW: // [X Y R startAngle endAngle]
                break;
            case MWX_PATH_ARCTO: // [X1 Y1 X2 Y2 R]
                break;
            case MWX_PATH_CIRCLE: // [X Y R]
                break;
            case MWX_PATH_CURVETO: // [CX1 CY1 CX2 CY1 X Y]
                break;
            case MWX_PATH_ELLIPSE: // [X Y W H]
                break;
            case MWX_PATH_LINETO: // [X Y]
                break;
            case MWX_PATH_RECT: // [X Y W H]
                break;
            case MWX_PATH_RRECT: // [X Y W H R]
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
     uintptr_t* obj, int count_obj){
    int op_ptr = 0;
    int vtx_ptr = 0;
    int obj_ptr = 0;
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
            case MWX_SET_FONT: // [O]
                break;
            case MWX_SET_PEN: // [O]
                break;
            case MWX_SET_BRUSH: // [O]
                //  Context transforms
                break;
            case MWX_BITMAP: // [O X Y W H]
                break;
            case MWX_ROTATE: // [Radian]
                break;
            case MWX_SCALE: // [X Y]
                break;
            case MWX_IDENTITY:
                break;
                //  Text and query
            case MWX_TEXT_DRAW: // [O X Y]
                break;
            case MWX_TEXT_QUERY_EXTENT: // [O] => [W H Descent externalLeading]
                break;
            case MWX_TEXT_QUERY_CHAR_EXTENTS: // [O Count] => [Count W ...]
                break;
            default: // INVALID
                return -1;
        }
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
mwx_graphics_kick(wxWindow* wnd,
                  unsigned char* op, int count_op,
                  double* vtx, int count_vtx,
                  uintptr_t* obj, int count_obj){
    return kick(me()->CreateContext(wnd),op,count_op,vtx,count_vtx,obj,count_obj);
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
    NMOSH_EXPORT_SYMBOL_INT(MWX_SET_FONT) // [O]
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
    NMOSH_EXPORT_SYMBOL_INT(MWX_TEXT_QUERY_CHAR_EXTENTS) // [O Count] => [Count W ...]
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
