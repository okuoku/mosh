#include <nmosh/plugin-if.h>
#include "moshcairo.h"
#ifdef HAVE_CAIRO_H
#include <cairo.h> /* For GStreamer SDK */
#else
#include <cairo/cairo.h>
#endif

/* Generic */
MOSHEXPORT
cairo_t*
mc_context_create(cairo_surface_t* s){
    return cairo_create(s);
}

MOSHEXPORT
void
mc_context_destroy(cairo_t* cr){
    cairo_destroy(cr);
}

MOSHEXPORT
void
mc_context_disable_aa(cairo_t* cr){
    cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
}

MOSHEXPORT
void
mc_surface_destroy(cairo_surface_t* s){
    cairo_surface_destroy(s);
}

MOSHEXPORT
void
mc_pattern_destroy(cairo_pattern_t* p){
    cairo_pattern_destroy(p);
}

MOSHEXPORT
void
mc_pattern_disable_aa(cairo_pattern_t* p){
    cairo_pattern_set_filter(p, CAIRO_FILTER_NEAREST);
}

MOSHEXPORT
cairo_pattern_t*
mc_pattern_solid(double r,double g,double b,double a){
    return cairo_pattern_create_rgba(r,g,b,a);
}

MOSHEXPORT
cairo_pattern_t*
mc_pattern_surface(cairo_surface_t* s){
    return cairo_pattern_create_for_surface(s);
}

/* Font related */
/* Currently, we use "toy" API. */
MOSHEXPORT
cairo_font_face_t*
mc_font_create(char *name,int bold_p,int italic_p){
    cairo_font_weight_t we;
    cairo_font_slant_t sl;
    sl = italic_p ? CAIRO_FONT_SLANT_ITALIC : CAIRO_FONT_SLANT_NORMAL;
    we = bold_p ? CAIRO_FONT_WEIGHT_BOLD : CAIRO_FONT_WEIGHT_NORMAL;
    return cairo_toy_font_face_create(name,sl,we);
}

MOSHEXPORT
void
mc_font_destroy(void* p){
    cairo_font_face_destroy((cairo_font_face_t *)p);
}

MOSHEXPORT
void
mc_font_extent_text(cairo_t* cr, char* text, cairo_font_face_t* font,
                    double* ret){
    cairo_text_extents_t ex;
    /* Select font */
    cairo_set_font_face(cr, font);
    /* Query extents */
    cairo_text_extents(cr, text, &ex);
    ret[0] = ex.x_bearing;
    ret[1] = ex.y_bearing;
    ret[2] = ex.width;
    ret[3] = ex.height;
    ret[4] = ex.x_advance;
    ret[5] = ex.y_advance;
}



/* Kick */

MOSHEXPORT
void
mc_kick(cairo_t* cr,const unsigned char* ops,int count_ops,void* objs[],int count_objs,
        double* vtxs, int count_vtxs,
        cairo_matrix_t mtxs[], int count_mtxs){
    int p_op = 0;
    int p_vtx = 0;

    cairo_pattern_t* pat;
    cairo_matrix_t *m0,*m1,*m;
    cairo_font_face_t* font;
    char* text;

    int r0,r;
    double x,y,x0,y0,x1,y1;

#define V(v) do{ v = vtxs[p_vtx]; p_vtx++; } while(0)
#define _ID do{ r = 0; do { p_op++; r0 = ops[p_op]; r <<= 7; r += r0; } \
    while(r0>=128); } while(0)
#define O(v) do{ _ID; v = objs[r]; } while(0)
#define M(v) do{ _ID; v = &mtxs[r]; } while(0)

    for(p_op = 0;p_op<count_ops;p_op++){
        switch(ops[p_op]){
            case Path_MoveTo:
                V(x);
                V(y);
                cairo_move_to(cr,x,y);
                break;
            case Path_CurveTo:
                V(x0);
                V(y0);
                V(x1);
                V(y1);
                V(x);
                V(y);
                cairo_curve_to(cr,x0,y0,x1,y1,x,y);
                break;
            case Path_LineTo:
                V(x);
                V(y);
                cairo_line_to(cr,x,y);
                break;
            case Path_Close:
                cairo_close_path(cr);
                break;

            case Draw_SelectContext:
                O(cr);
                break;
            case Draw_Source:
                O(pat);
                cairo_set_source(cr,pat);
                break;
            case Draw_WindingEvenOdd:
                cairo_set_fill_rule(cr, CAIRO_FILL_RULE_EVEN_ODD);
                break;
            case Draw_WindingNonZero:
                cairo_set_fill_rule(cr, CAIRO_FILL_RULE_WINDING);
                break;
            case Draw_Stroke:
                cairo_stroke(cr);
                break;
            case Draw_StrokePreserved:
                cairo_stroke_preserve(cr);
                break;
            case Draw_Fill:
                cairo_fill(cr);
                break;
            case Draw_FillPreserved:
                cairo_fill_preserve(cr);
                break;
            case Draw_StrokeWidth:
		V(x);
                cairo_set_line_width(cr,x);
                break;
            case Draw_SetTransform:
                M(m);
                cairo_set_matrix(cr,(const cairo_matrix_t *)m);
                break;
            case Draw_LoadTransform:
                M(m);
                cairo_get_matrix(cr,m);
                break;
            case Draw_Paint:
                cairo_paint(cr);
                break;
            case Draw_PaintWithAlpha:
                V(x);
                cairo_paint_with_alpha(cr,x);
                break;
            case Draw_SetOperator:
                _ID;
                cairo_set_operator(cr, r);
                break;
            case Draw_Clip:
                cairo_clip(cr);
                break;
            case Draw_ClipPreserved:
                cairo_clip_preserve(cr);
                break;
            case Draw_ClearClip:
                cairo_reset_clip(cr);
                break;
            case Draw_FontFace:
                O(font);
                cairo_set_font_face(cr,font);
                break;
            case Draw_FontSize:
                V(x);
                cairo_set_font_size(cr,x);
                break;
            case Draw_Text:
                O(text);
                cairo_show_text(cr,text);
                break;
            case Source_SetTransform:
                O(pat);
                M(m);
                cairo_pattern_set_matrix(pat, (const cairo_matrix_t *)m);
                break;

            case Matrix_Identity:
                M(m);
                cairo_matrix_init_identity(m);
                break;
            case Matrix_Load:
                M(m);
                V(x0);
                V(y0);
                V(x1);
                V(y1);
                V(x);
                V(y);
                cairo_matrix_init(m,x0,y0,x1,y1,x,y);
                break;
            case Matrix_Translate:
                M(m);
                V(x);
                V(y);
                cairo_matrix_translate(m,x,y);
                break;
            case Matrix_Scale:
                M(m);
                V(x);
                V(y);
                cairo_matrix_scale(m,x,y);
                break;
            case Matrix_Rotate:
                M(m);
                V(x);
                cairo_matrix_rotate(m,x);
                break;
            case Matrix_Invert:
                M(m);
                cairo_matrix_invert(m);
                break;
            case Matrix_Mul:
                M(m);
                M(m0);
                M(m1);
                cairo_matrix_multiply(m,m0,m1);
                break;
            case Matrix_Copy:
                M(m);
                M(m0);
                *m = *m0;
                break;
        }
    }
}

/* Memory surface */
#ifdef CAIRO_HAS_IMAGE_SURFACE
MOSHEXPORT
cairo_surface_t*
mc_mem_create(int x,int y){
    return cairo_image_surface_create( CAIRO_FORMAT_RGB24,
                                       x,
                                       y);
}

MOSHEXPORT
cairo_surface_t*
mc_mem_create_alpha(int x,int y){
    return cairo_image_surface_create( CAIRO_FORMAT_ARGB32,
                                       x,
                                       y);
}

MOSHEXPORT
cairo_surface_t*
mc_mem_create_for(void* p, int x, int y, int pitch){
    return cairo_image_surface_create_for_data(p,
                                               CAIRO_FORMAT_RGB24,
                                               x,
                                               y,
                                               pitch);

}

#else
#error It's strange. Your Cairo doesn't have image surface support ... (or too old < 1.8)
#endif

#ifdef CAIRO_HAS_PNG_FUNCTIONS
MOSHEXPORT
cairo_surface_t*
mc_mem_png_load(const char* fn){
    return cairo_image_surface_create_from_png(fn);
}

MOSHEXPORT
int
mc_mem_png_save(cairo_surface_t* s, const char* fn){
    return cairo_surface_write_to_png(s, fn);
}
#endif /* PNG */

#ifdef CAIRO_HAS_WIN32_SURFACE
#include <cairo/cairo-win32.h>
MOSHEXPORT
cairo_surface_t*
mc_win32_create(void* hDC){
    return cairo_win32_surface_create((HDC)hDC);
}

MOSHEXPORT
cairo_surface_t*
mc_win32_create_alpha(void* hDC, int x,int y){
    return cairo_win32_surface_create_with_ddb((HDC)hDC,
                                               CAIRO_FORMAT_RGB24,
                                               x,
                                               y);
}

MOSHEXPORT
cairo_font_face_t*
mc_win32_create_font(void* hFont){
    return cairo_win32_font_face_create_for_hfont((HFONT) hFont);
}
#endif /* WIN32 */
