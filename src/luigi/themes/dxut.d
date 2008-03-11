//---------------------------------------------------------------------
/*
  luigi/themes/dxut.d -- A clone of the DXUT texture-based theme.

  Copyright (C) 2006 William V. Baxter III

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  William Baxter wbaxter@gmail.com
*/


module luigi.themes.dxut;

import luigi.base;
import luigi.theme;
import luigi.opengl;
import luigi.gldraw;
import luigi.font;
import luigi.event;
import luigi.gui;

import std.math : pow;
import std.perf;

version (useDerelict) {
import derelict.opengl.extension.arb.texture_compression;
} else {
    private
    {
        PFNGLCOMPRESSEDTEXIMAGE3DARBPROC         glCompressedTexImage3DARB;
        PFNGLCOMPRESSEDTEXIMAGE2DARBPROC         glCompressedTexImage2DARB;
        PFNGLCOMPRESSEDTEXIMAGE1DARBPROC         glCompressedTexImage1DARB;
        PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC      glCompressedTexSubImage3DARB;
        PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC      glCompressedTexSubImage2DARB;
        PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC      glCompressedTexSubImage1DARB;
        PFNGLGETCOMPRESSEDTEXIMAGEARBPROC        glGetCompressedTexImageARB;

        version(Windows)
            import win32.wingdi; // for wgl funcs
        else version(linux)
            version = UsingGLX;
        version(UsingGLX)
            import x11.glx;
        import string=std.string;

        bool glBindExtFunc(void **ptr, char[] funcName)
        {
            version(Windows)
                *ptr = wglGetProcAddress(string.toStringz(funcName));
            else version(UsingGLX)
                *ptr = glXGetProcAddress(string.toStringz(funcName));
            return (*ptr !is null);
        }
    }
}

static const GLint GL_COMPRESSED_RGB_S3TC_DXT1_EXT  = 0x83F0;
static const GLint GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = 0x83F1;
static const GLint GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = 0x83F2;
static const GLint GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = 0x83F3;



void initialize_compressed_texture_extension()
{
    bool allOk = true;
    version(useDerelict) {
        if (!ARBTextureCompression.load("GL_ARB_texture_compression")) 
            allOk=false;
    }
    else {
        if(!glBindExtFunc(cast(void**)&glCompressedTexImage3DARB, "glCompressedTexImage3DARB"))
            allOk = false;
        if(!glBindExtFunc(cast(void**)&glCompressedTexImage2DARB, "glCompressedTexImage2DARB"))
            allOk = false;
        if(!glBindExtFunc(cast(void**)&glCompressedTexImage1DARB, "glCompressedTexImage1DARB"))
            allOk = false;
        if(!glBindExtFunc(cast(void**)&glCompressedTexSubImage3DARB, "glCompressedTexSubImage3DARB"))
            allOk = false;
        if(!glBindExtFunc(cast(void**)&glCompressedTexSubImage2DARB, "glCompressedTexSubImage2DARB"))
            allOk = false;
        if(!glBindExtFunc(cast(void**)&glCompressedTexSubImage1DARB, "glCompressedTexSubImage1DARB"))
            allOk = false;
        if(!glBindExtFunc(cast(void**)&glGetCompressedTexImageARB, "glGetCompressedTexImageARB"))
            allOk = false;
    }
    if (!allOk) {
        throw new Exception("Unable to initialize texture compression extension");
    }
}

alias DXUTTheme Theme;

class DXUTTheme : ThemeBase
{
    this() {
        // Load texture first
        try {
            initialize_compressed_texture_extension();

            char[] filepath = Luigi().find_resource_location(
                "dxutcontrols.dds",
                "themes/dxutcontrols.dds",
                "resource/dxutcontrols.dds"
                );
            if (filepath) {
                loadCompressedTexture(filepath, m_texid);
            } else {
                report_error("Couldn't find, or failed to load dxutcontrols.dds");
                return;
            }

            glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &m_texw);
            glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &m_texh);
        }
        catch (Exception e) {
            report_error(std.string.format("Couldn't load texture: ", e.toString));
        }

        m_default_font = new ASCIIBitmapFont();
        m_timer = new PerformanceCounter;

        // Register all known size and draw functions.
        label_register();
        button_register();
        checkbox_register();
        radiobutton_register();
        textfield_register();
        optionmenu_register();
        slider_register();
        scrollbar_register();
        panel_register();
        spinner_register();

        sel_text_color.set(255,255,255);
        sel_text_bkg_color.set(10,36,106);
        sel_text_unfocus_bkg_color.set(176,176,176);

    }
    ~this() {
        if (m_texid) 
            glDeleteTextures(1, &m_texid);
    }

    //----THEME DATA--------------------------------------------------------
    class ThemeData {
        // per-widget elements needed so we can blend objects independently
        // a little wasteful, though, since most info does not change per-elem
        Element[] elements;
    }
    ThemeData get_theme_data(Widget w) {
        Object td;
        if (w && (td=w.theme_instance_data)!is null) {
            return cast(ThemeData)td;
        }
        else {
            // This means we haven't made it yet
            ThemeData td2 = new ThemeData;
            w.theme_instance_data = td2;
            auto pElements = w.classinfo in m_elements;
            if (pElements!=null) {
                td2.elements = (*pElements).dup;
            }
            else {
                // this is an unknown class 
                // -- find best match up class hierarchy
                auto cinfo = w.classinfo;
                while (cinfo != Widget.classinfo) {
                    pElements = cinfo in m_elements;
                    if (pElements) {
                        td2.elements = (*pElements).dup;
                        break;
                    }
                    cinfo = cinfo.base;
                }
                if (cinfo == Widget.classinfo) {
                    // didn't find it. w is not a subclass of anything
                    // we know how to draw.
                    Element[] empty;
                    m_elements[w.classinfo] = empty;
                }
            }
            return td2;
        }
    }
    Element[] get_widget_elements(Widget w) {
        return get_theme_data(w).elements;
    }

    //----GRPAHICS STATE------------------------------------------------------
    override void begin_drawing(Rect r) {
        m_timer.stop();
        m_elapsedTime = m_timer.milliseconds / 1000.0;
        m_timer.start();
        
        push_graphics_state(r);
        glPushAttrib(GL_TEXTURE_BIT);

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glBindTexture(GL_TEXTURE_2D,m_texid);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);

        glMatrixMode(GL_TEXTURE);
        glPushMatrix();
        glLoadIdentity();
        glScalef(1./m_texw, 1./m_texh,1);
        glMatrixMode(GL_MODELVIEW);

    }
    override void end_drawing() {
        glPopAttrib(); // TEXTURE_BIT
        pop_graphics_state();
        glMatrixMode(GL_TEXTURE);
        glPopMatrix();
        glMatrixMode(GL_MODELVIEW);
    }

    //============================================================================
    // Widget drawing and size routines

    //----HELPERS-----------------------------------------------------------------
    Font get_font() { return m_default_font; }
    bool set_color(float g) 
    {
        glColor3f(g,g,g);
        return true;
    }
    bool set_color(Color c) 
    {
        glColor4ubv(c.ptr);
        return c.a!=0;
    }

    enum TextAlign {
        Center = 0x0,
        Top = 0x1,
        Bottom = 0x2,
        Left = 0x4,
        Right = 0x8,
        TopLeft = Top|Left,
        TopRight = Top|Right,
        BottomLeft = Bottom|Left,
        BottomRight = Bottom|Right,
        LRMask = Right | Left,
        TBMask = Top | Bottom,
    }
    /** Draw text so the upper left corner is at (x,y) */
    void draw_text(Font font, char[] label, float x, float y)
    {
        Point trans = font.origin; trans.x+=x; trans.y+=y;
        translate(trans);
        font.draw_string(label);
        untranslate(trans);
    }
    /** Draw text inside the rect with the indicated alignment */
    void draw_text_rect(Font font, char[] txt, Rect r, TextAlign ta)
    {
        Point trans = font.origin; trans.x+=r.x; trans.y+=r.y;
        TextAlign tbflags = ta & TextAlign.TBMask;
        TextAlign lrflags = ta & TextAlign.LRMask;
        if (lrflags != TextAlign.Left) {
            Rect sr = font.string_rect(txt);
            if (lrflags == TextAlign.Right) {
                trans.x += r.w - sr.w;
            } 
            else if (lrflags == TextAlign.Center) {
                trans.x += (r.w - sr.w)/2;
            }
        }
        if (lrflags != TextAlign.Top) {
            float txth = font.height;
            if (lrflags == TextAlign.Bottom) {
                trans.y += r.h - txth;
            } 
            else if (lrflags == TextAlign.Center) {
                trans.y += (r.h - txth)/2;
            }
        }
        translate(trans);
        font.draw_string(txt);
        untranslate(trans);
    }
    /** Draw text centered in the rectangle */
    void draw_text_centered(Font font, char[] label, Rect rect)
    {
        draw_text_rect(font,label,rect,TextAlign.Center);
    }

    void draw_elem_texture(inout Element elem, inout Rect rect) {
        if (elem.texColor.alpha == 0) 
            return;
        glEnable(GL_TEXTURE_2D);
        set_color(elem.texColor.current);
        fill_rect(rect, elem.texRect);
    }
    void draw_elem_texture(inout Element elem, inout Rect rect, int[4] texRect) {
        if (elem.texColor.alpha == 0) 
            return;
        glEnable(GL_TEXTURE_2D);
        set_color(elem.texColor.current);
        fill_rect(rect, texRect);
    }
    void draw_elem_text(inout Element elem, char[] text, Rect rect, 
                        TextAlign al = TextAlign.Center, bool shadow=false)
    {
        if (elem.fontColor.alpha == 0)
            return;
        glDisable(GL_TEXTURE_2D);
        if (shadow) {
            set_color(0);
            rect.x+=1; rect.y+=1;
            draw_text_rect(elem.font, text, rect, al);
            rect.x-=1; rect.y-=1;
        }
        set_color(elem.fontColor.current);
        draw_text_rect(elem.font, text, rect, al);
    }
    //----LABEL---------------------------------------------------------------
    void label_register() {
        addHandlers!(Label)(&label_draw, &label_best_size);

        Element[] elems = new Element[1];
        Element e;
        e.set_font( m_default_font );
        e.fontColor.states[ WidgetState.Disabled ] = Color( 200, 200, 200, 200 );
        elems[0] = e;
        m_elements[Label.classinfo] = elems;
    }
    Size label_best_size(Widget widget, Size bounds) {
        auto w = cast(Label)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        Rect r = elem[0].font.string_rect(w.label);
        return Size(r.width+2,r.height+2);
    }
    void label_draw(Widget widget) {
        auto w = cast(Label)widget; assert(w);
        WidgetState state = WidgetState.Normal;
        if( !w.enabled)
            state = WidgetState.Disabled;
        Element[] elem = get_widget_elements(w);
        elem[0].fontColor.blend( state, m_elapsedTime );
        draw_elem_text( elem[0], w.label, w.rect, TextAlign.Center, true );
    }

    //----BUTTON--------------------------------------------------------------
    void button_register() {
        addHandlers!(Button)(&button_draw, &button_best_size, &button_min_size);
        Element[] elems = new Element[2];
        Element e;
        // button layer
        e.set_texture( m_texid, [0,0,136,54] );
        e.fontColor.states[ WidgetState.Disabled ] = Color(200,200,200,200);
        e.set_font( m_default_font );
        e.texColor.states[ WidgetState.Normal ] = Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Depressed ] = Color(255, 255, 255, 200);
        e.fontColor.states[ WidgetState.Rollover ] = Color(0, 0, 0, 255);
        elems[0] = e;
    
        // fill layer
        e.set_texture( m_texid, [136, 0, 272, 54], Color(255, 255, 255, 0) );
        e.texColor.states[ WidgetState.Rollover ] = Color(255, 255, 255, 160);
        e.texColor.states[ WidgetState.Depressed ] = Color(0, 0, 0, 60);
        e.texColor.states[ WidgetState.Focus ] = Color(255, 255, 255, 30);
        elems[1] = e;
    
        m_elements[Button.classinfo] = elems;
    }
    Size button_min_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        Rect r = elem[0].font.string_rect(w.label);
        return Size(r.width+20,r.height+8);
    }
    Size button_best_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Size minsz = button_min_size(w,bounds);
        return Size(max(50.,minsz.w), max(22.,minsz.h));
    }
    void button_draw(Widget widget) {
        auto w = cast(Button)widget; assert(w);
        with (w) {
            int dx = 0;
            int dy = 0;
            WidgetState iState = WidgetState.Normal;
            float blendRate = 0.8f;
            bool chk = is_toggle && checked;
            if( !shown ) {
                iState = WidgetState.Hidden; 
            }
            else if( !enabled ) {
                iState = WidgetState.Disabled;
            }
            else if( depressed )
            {
                iState = WidgetState.Depressed;
                blendRate = 0.6;
            }
            else if( rollover )
            {
                iState = WidgetState.Rollover;
            }
            else if( focused )
            {
                iState = WidgetState.Focus;
            }

            if ( depressed ^ chk ) {
                iState = WidgetState.Depressed;
                dx = dy = 1;
            }
            if (!depressed && chk && rollover) {
                iState = WidgetState.Rollover;
            }
            if ( rollover ) {
                dx -= 1; dy -= 1;
            }

            Element[] elem = get_widget_elements(w);

            Rect r = rect;
            r.inset(1);
            r.x += dx;
            r.y += dy;

            foreach(inout e; elem) {
                e.texColor.blend( iState, m_elapsedTime, blendRate);
                e.fontColor.blend( iState, m_elapsedTime, blendRate);
                draw_elem_texture(e, r);
                draw_elem_text(e, label, r);
            }
        }
    }
    //----CHECKBOX----------------------------------------------------------
    void checkbox_register() {
        addHandlers!(Checkbox)(&checkbox_draw, &checkbox_best_size, &checkbox_min_size);
        Element[] elems = new Element[2];
        Element e;

        e.fontColor.states[ WidgetState.Rollover ] = Color(0, 0, 0, 255);
        e.texColor.states[ WidgetState.Rollover ] = Color(255, 255, 255, 160);
    
        // check box
        e.set_texture( m_texid, [0, 54, 27, 81] );
        e.set_font( m_default_font, Color(255, 255, 255, 255) );
        e.fontColor.states[ WidgetState.Disabled ] = Color( 200, 200, 200, 200 );
        e.texColor.states[ WidgetState.Normal ] = Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Focus ] = Color(255, 255, 255, 200);
        e.texColor.states[ WidgetState.Depressed ] = Color(255, 255, 255, 255);
        elems[0] = e;

        // Check
        e.set_texture( m_texid, [27, 54, 54, 81] );
        elems[1] = e;

        m_elements[Checkbox.classinfo] = elems;
    }
    Size checkbox_min_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        Point p0 = Point(elem[0].texRect[0], elem[0].texRect[1]);
        Point p1 = Point(elem[0].texRect[2], elem[0].texRect[3]);
        p1 -= p0;
        Size btn = Size(lrint(p1.x/2),lrint(p1.y/2));
        Size txt = elem[0].font.string_rect(w.label).size;

        return Size(btn.width+3+txt.width+4,max(btn.height,txt.height));
    }
    Size checkbox_best_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Size minsz = checkbox_min_size(w,bounds);
        return Size(max(50.,minsz.w), max(22.,minsz.h));
    }
    void checkbox_draw(Widget widget) {
        auto w = cast(Button)widget; assert(w);
        with (w) {
            WidgetState iState = WidgetState.Normal;
            if( !shown )
                iState = WidgetState.Hidden;
            else if( !enabled )
                iState = WidgetState.Disabled;
            else if( depressed )
                iState = WidgetState.Depressed;
            else if( rollover )
                iState = WidgetState.Rollover;
            else if( focused )
                iState = WidgetState.Focus;

            float fBlendRate = ( iState == WidgetState.Depressed ) ? 0.0f : 0.8f;

            Element[] elem = get_widget_elements(w);
            elem[0].texColor.blend( iState, m_elapsedTime, fBlendRate );
            elem[0].fontColor.blend( iState, m_elapsedTime, fBlendRate );

            Rect btn_rect = rect;
            btn_rect.w = btn_rect.h;

            Rect txt_rect = rect;
            txt_rect.x += btn_rect.w + 2;
            txt_rect.w = elem[0].font.string_rect(label).w;
            
            draw_elem_texture( elem[0], btn_rect );
            draw_elem_text( elem[0], label, txt_rect, TextAlign.Center, true );

            if( !checked )
                iState = WidgetState.Hidden;

            elem[1].texColor.blend( iState, m_elapsedTime, fBlendRate );
            btn_rect.inset(1);
            draw_elem_texture( elem[1], btn_rect );
        }
    }
    //----RADIOBUTTON-------------------------------------------------------
    void radiobutton_register() {
        // radiobutton is identical to checkbox, except the textures it uses to draw.
        addHandlers!(RadioButton)(&checkbox_draw, &checkbox_best_size, &checkbox_min_size);
        Element[] elems = new Element[2];
        Element e;

        e.fontColor.states[ WidgetState.Rollover ] = Color(0, 0, 0, 255);
        e.texColor.states[ WidgetState.Rollover ] = Color(255, 255, 255, 160);
    
        // radio button border
        e.set_texture( m_texid, [54, 54, 81, 81] );
        e.set_font( m_default_font, Color(255, 255, 255, 255) );
        e.fontColor.states[ WidgetState.Disabled ] = Color( 200, 200, 200, 200 );
        e.texColor.states[ WidgetState.Normal ] = Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Focus ] = Color(255, 255, 255, 200);
        e.texColor.states[ WidgetState.Depressed ] = Color(255, 255, 255, 255);
        elems[0] = e;

        // selection indicator
        e.set_texture( m_texid, [81, 54, 108, 81] );
        elems[1] = e;

        m_elements[RadioButton.classinfo] = elems;
    }
    //----TEXTFIELD---------------------------------------------------------
    enum TextElem {
        Text,
        TL, // top left
        TC, // top center
        TR, // top right
        L,  // left
        R,  // right
        BL, // bottom left
        BC, // bottom center
        BR  // bottom right
    }
    void textfield_register() {
        addHandlers!(TextField)(&textfield_draw, &textfield_best_size, &textfield_min_size);
        addEventHandlers!(TextField)(&textfield_mouse_button, &textfield_mouse_move);

        Element[] elems = new Element[TextElem.max+1];
        Element e;

        e.fontColor.states[ WidgetState.Rollover ] = Color(0, 0, 0, 255);
        e.fontColor.states[ WidgetState.Depressed ] = Color(0, 0, 0, 255);
        e.fontColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 200);
        e.set_font( m_default_font, Color(255, 255, 255, 255));

        e.set_texture( m_texid, [1, 290, 280, 331] );
        e.texColor.states[ WidgetState.Rollover ] = Color(255, 255, 255, 160);
        e.texColor.states[ WidgetState.Depressed ] = Color(150, 150, 150, 255);
        e.texColor.states[ WidgetState.Normal ]   = Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Focus ]    = Color(255, 255, 255, 200);
        e.texColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 255);

        e.set_font(m_default_font, Color( 0, 0, 0 ));
        int[4][] rects = [
            [14, 90, 241, 113],
            [8, 82, 14, 90],
            [14, 82, 241, 90],
            [241, 82, 246, 90],
            [8, 90, 14, 113],
            [241, 90, 246, 113],
            [8, 113, 14, 121],
            [14, 113, 241, 121],
            [241, 113, 246, 121],
            ];
        foreach(i,r; rects) {
            e.set_texture( m_texid, r);
            elems[i] = e;
        }
        m_elements[TextField.classinfo] = elems;
    }
    const int textfield_borderw = 5; 
    const int textfield_paddingw = 4;
    float textfield_get_char_pos(TextField txtf, float clickx)
    {
        Element[] elem = get_widget_elements(txtf);
        Rect entry_rect,inside_rect;
        Rect r = txtf.rect;
        r.inset(textfield_borderw + textfield_paddingw);
        r.x += txtf.xscroll_offset;
        r.w -= txtf.xscroll_offset;
        Font font = elem[0].font;
        float basex = r.x;
        float x = r.x;
        float lastx = x-10;
        int imax = txtf.text.length;
        // binary search would be better here theoretically
        int i;
        for(i=0; x<clickx && i<imax; x=basex+font.string_rect(txtf.text[0..++i]).w)
        {
            lastx = x;
        }
        if (i==0) return 0;
        else if (i>=imax) return imax;
        
        float frac = (clickx-lastx)/(x-lastx);
        return i-1.0+frac;
    }
    void textfield_update_rects(TextField txtf, Element[] elem)
    {
        Rect rbase = txtf.rect;
        Rect rtext = rbase;
        rtext.inset( textfield_borderw );
        alias TextElem T;
        elem[T.Text].elemRect = rtext;
        elem[T.TL].elemRect.setLTRB( rbase.x,  rbase.y,  rtext.x,  rtext.y );
        elem[T.TC].elemRect.setLTRB( rtext.x,  rbase.y,  rtext.x2, rtext.y );
        elem[T.TR].elemRect.setLTRB( rtext.x2, rbase.y,  rbase.x2, rtext.y );
        elem[T.L ].elemRect.setLTRB( rbase.x,  rtext.y,  rtext.x,  rtext.y2 );
        elem[T.R ].elemRect.setLTRB( rtext.x2, rtext.y,  rbase.x2, rtext.y2 );
        elem[T.BL].elemRect.setLTRB( rbase.x,  rtext.y2, rtext.x,  rbase.y2 );
        elem[T.BC].elemRect.setLTRB( rtext.x,  rtext.y2, rtext.x2, rbase.y2 );
        elem[T.BR].elemRect.setLTRB( rtext.x2, rtext.y2, rbase.x2, rbase.y2 );
    }
    Size textfield_min_size(Widget widget, Size bounds) {
        Size sz = textfield_best_size(widget,bounds);
        sz.w = min(100.0,sz.w);
        return sz;
    }
    Size textfield_best_size(Widget widget, Size bounds) {
        auto w = cast(TextField)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        Size sz = elem[TextElem.Text].font.string_rect(w.text).size;
        sz.w = max(100.0,sz.w);
        float pad = 2*(textfield_borderw+textfield_paddingw);
        sz.w += pad;
        sz.h += pad;
        return sz;
    }
    void textfield_draw(Widget widget)
    {
        auto w = cast(TextField)widget; assert(w);
        with(w) {
            Element[] elem = get_widget_elements(w);
            textfield_update_rects(w, elem);

            //foreach(i,e; elem)
            for (int i=0; i<elem.length; i++)
            {
                Element *e = &elem[i];
                e.texColor.blend( WidgetState.Normal, m_elapsedTime );
                e.fontColor.blend( WidgetState.Normal, m_elapsedTime );
                draw_elem_texture( *e, e.elemRect );
            }

            Rect inside_rect = elem[0].elemRect;
            inside_rect.inset(textfield_paddingw);

            // Clip to insides of entry rect
            push_clip_rect( inside_rect ); scope(exit) pop_clip_rect();

            // Scrolled rect is the entire text rect, possibly scrolled out of view
            // (--actually only x and y matter)
            Rect scrolled_rect = inside_rect;
            scrolled_rect.x += xscroll_offset;

            float sel_offset = elem[0].font.string_rect(text[0..sel_begin]).width;
            Size sel_size = elem[0].font.string_rect(text[sel_begin..sel_end]).size;

            // make sure active end of selection is visible
            {
                float sel_x = scrolled_rect.x + sel_offset;
                if (sel_active==1) sel_x += sel_size.width;
                float dx;
                if ((dx=sel_x-inside_rect.x)<0) {
                    scrolled_rect.x-=dx-2; xscroll_offset-=dx-2;
                }
                else if ((dx=sel_x-inside_rect.x2)>0) {
                    scrolled_rect.x-=dx+1; xscroll_offset-=dx+1;
                }
            }

            if (selection_length)
            {
                // draw unselected first part of text
                if (sel_begin!=0) {
                    draw_elem_text(elem[0], text[0..sel_begin], scrolled_rect,
                                   TextAlign.TopLeft);
                }
                
                Rect rsel;
                rsel.x = scrolled_rect.x+sel_offset;
                rsel.y = scrolled_rect.y;
                rsel.size = sel_size;

                // Draw selection bkg
                if (focused) {
                    set_color(sel_text_bkg_color);
                } else {
                    set_color(sel_text_unfocus_bkg_color);
                }
                glDisable(GL_TEXTURE_2D);
                fill_rect( rsel );

                // draw selected text
                set_color(sel_text_color);
                draw_text_rect(elem[0].font, text[sel_begin..sel_end], rsel, 
                               TextAlign.TopLeft);
                rsel.x += rsel.width;

                // draw rest of unselected text
                if (sel_end!=text.length) {
                    draw_elem_text(elem[0], text[sel_end..$], rsel, TextAlign.TopLeft);
                }
            }
            else {
                // Draw all the text in one go
                draw_elem_text(elem[0], text, scrolled_rect, TextAlign.TopLeft);
            }

            // Caret
            if ( enabled && selection_length == 0 ) {
                if (focused && selection_length==0)
                {
                    float h = inside_rect.height;
                    float x = scrolled_rect.x + sel_offset;
                    set_color(elem[0].fontColor.current);
                    glBegin( GL_LINE_LOOP );
                    glVertex2f( x, inside_rect.y  );
                    glVertex2f( x, inside_rect.y  );
                    glVertex2f( x, inside_rect.y2 );
                    glVertex2f( x, inside_rect.y2 );
                    glEnd();
                }
            }

        }
    }
    void textfield_mouse_button(Widget widget, MouseButtonEvent ev)
    {
        if (ev.is_left) {
            auto txtf = cast(TextField)widget; assert(txtf);
            if (ev.is_press) {
                float fselpos = textfield_get_char_pos(txtf, ev.x);
                int pos = lrint(fselpos);
                if (ev.shift_down) {
                    int[2] sel=txtf.sel_range[];
                    sel[txtf.sel_active] = pos;
                    txtf.select_range(sel[0],sel[1]);
                }
                else {
                    txtf.select_range(pos,pos);       
                }
                txtf.grab_mouse();
            }
            else if (ev.is_release) {
                if (txtf.is_grabbing_mouse()) txtf.release_mouse(); 
            }
        }
    }
    void textfield_mouse_move(Widget widget, MouseMoveEvent ev) 
    {
        if (ev.left_down) {
            auto txtf = cast(TextField)widget; assert(txtf);
            float fselpos = textfield_get_char_pos(txtf, ev.x);
            int pos = lrint(fselpos);
            int[2] sel=txtf.sel_range[];
            sel[txtf.sel_active] = pos;
            txtf.select_range(sel[0],sel[1]);
        }
    }
    
    //----SLIDER------------------------------------------------------------
    void slider_register() {
        addHandlers!(Slider)(&slider_draw, &slider_best_size, &slider_min_size);
        addEventHandlers!(Slider)(&slider_mouse_button, &slider_mouse_move);

        Element[] elems = new Element[2];
        Element e;

        e.fontColor.states[ WidgetState.Rollover ] = Color(0, 0, 0, 255);
        e.fontColor.states[ WidgetState.Depressed ] = Color(0, 0, 0, 255);
        e.fontColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 200);
        e.set_font( m_default_font, Color(255, 255, 255, 255));

        e.set_texture( m_texid, [1, 290, 280, 331] );
        e.texColor.states[ WidgetState.Rollover ] = Color(255, 255, 255, 160);
        e.texColor.states[ WidgetState.Depressed ] = Color(150, 150, 150, 255);
        e.texColor.states[ WidgetState.Normal ]   = Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Focus ]    = Color(255, 255, 255, 200);
        e.texColor.states[ WidgetState.Disabled ] = Color(255, 255, 255, 70);
        elems[0] = e;

        // Slider thumb
        e.set_texture( m_texid, [248, 55, 289, 96] );
        elems[1] = e;

        m_elements[Slider.classinfo] = elems;
        
    }
    Size slider_min_size(Widget widget, Size bounds) {
        auto w = cast(Slider)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        
        int scroll_thumb_w = (elem[1].texRect[2] - elem[1].texRect[0])/2;
        int scroll_thumb_h = (elem[1].texRect[3] - elem[1].texRect[1])/2;
        Size sz = Size(scroll_thumb_w+20, scroll_thumb_h+5);
        if (w.vertical) {
            return Size(sz.h,sz.w);
        }
        return sz;
    }
    Size slider_best_size(Widget widget, Size bounds) {
        auto w = cast(Slider)widget; assert(w);
        Size sz = slider_min_size(w,bounds);
        if (w.vertical) {
            sz.h = max(sz.h, 100);
            //sz.h = max(sz.h, bounds.h);
        }
        else {
            sz.w = max(sz.w, 100);
            //sz.w = max(sz.w, bounds.w);
        }
        return sz;
    }
    void slider_draw(Widget widget) {
        auto w = cast(Slider)widget; assert(w);
        with(w) {
            int dx = 0;
            int dy = 0;

            WidgetState iState = WidgetState.Normal;
            if( !shown )
            {
                iState = WidgetState.Hidden;
            }
            else if( disabled )
            {
                iState = WidgetState.Disabled;
            }
            else if( is_grabbing_mouse )
            {
                iState = WidgetState.Depressed;
                dx = dy = 1;
            }
            else if( rollover )
            {
                iState = WidgetState.Rollover;
                dx = dy = -1;
            }
            else if( focused )
            {
                iState = WidgetState.Focus;
            }

            float fBlendRate = ( iState == WidgetState.Depressed ) ? 0.0f : 0.8f;

            Element[] elem = get_widget_elements(w);
    
            int track_height = (elem[0].texRect[3] - elem[0].texRect[1])/2;
            Rect track_rect = rect;
            if (horizontal) {
                track_rect.h = track_height;
                track_rect.y = rect.y + (rect.h-track_rect.h)/2;
            } else {
                track_rect.w = track_height;
                track_rect.x = rect.x + (rect.w-track_rect.w)/2;
            }
            elem[0].texColor.blend( iState, m_elapsedTime, fBlendRate );
            if (horizontal) {
                draw_elem_texture( elem[0], track_rect );
            }
            else {
                glPushMatrix();
                // Rotate rectangle
                Rect r = Rect(0,0,track_rect.h,track_rect.w);
                r.x = -r.w/2;
                r.y = -r.h/2;
                glTranslatef(rect.x+rect.w/2, rect.y+rect.h/2, 0);
                glRotatef(90,0,0,1);
                draw_elem_texture( elem[0], r );
                glPopMatrix();
            }

            Rect thumb_rect = rect;
            int thumb_w = (elem[1].texRect[2] - elem[1].texRect[0])/2;
            int thumb_h = (elem[1].texRect[3] - elem[1].texRect[1])/2;
            thumb_rect.size = Size(thumb_w,thumb_h);
            if (thumb_rect.w > rect.w) {
                float shrinkby = rect.w/thumb_rect.w;
                thumb_rect.h *= shrinkby;
                thumb_rect.w = rect.w;
            }
            if (thumb_rect.h > rect.h) {
                float shrinkby = rect.h/thumb_rect.h;
                thumb_rect.w *= shrinkby;
                thumb_rect.h = rect.h;
            }
            elem[1].texColor.blend( iState, m_elapsedTime, fBlendRate );
            if ( horizontal ) {
                float t = (value-value_min)/(value_max-value_min);
                float v = rect.x + thumb_rect.w/2 + t*(rect.w-thumb_rect.w);
                thumb_rect.x = v-thumb_rect.w/2;
                thumb_rect.y = track_rect.y + (track_rect.h-thumb_rect.h)/2;
                draw_elem_texture( elem[1], thumb_rect );
            } else {
                float t = (value-value_min)/(value_max-value_min);
                float v = rect.y2 - thumb_rect.w/2 - t*(rect.h-thumb_rect.h);
                thumb_rect = Rect(-thumb_rect.h/2,-thumb_rect.w/2, 
                                   thumb_rect.h,   thumb_rect.w);
                glPushMatrix();
                glTranslatef(rect.x+rect.w/2, v, 0);
                glRotatef(90,0,0,1);
                draw_elem_texture( elem[1], thumb_rect );
                glPopMatrix();
            }
        }
    }
    double slider_map_mouse_to_value(Slider w, Point p)
    {
        double t;
        Element[] elem = get_widget_elements(w);
        int tw = 
            (elem[1].texRect[2] - elem[1].texRect[0])/2;
        tw *= 0.5;
        if (w.horizontal) {
            double trackmin = w.rect.x + tw;
            double trackmax = w.rect.x2 - tw;
            t = (p.x-trackmin)/(trackmax-trackmin);
        } else {
            double trackmin = w.rect.y + tw;
            double trackmax = w.rect.y2 - tw;
            t = 1 - (p.y-trackmin)/(trackmax-trackmin);
        }
        double v = w.value_min + t*(w.value_max - w.value_min);
        return v;
    }
    void slider_mouse_button(Widget widget, MouseButtonEvent ev)
    {
        auto w = cast(Slider)widget; assert(w);
        if (ev.is_left && ev.is_press) { 
            w.grab_mouse(); 
        }
        else if (ev.is_left && ev.is_release) { w.release_mouse(); }
        double v = slider_map_mouse_to_value(w, ev.p);
        w.m_saveValue = w.value;
        w.value = v;
        ev.alive = false;
    }
    void slider_mouse_move(Widget widget, MouseMoveEvent ev) 
    {
        if (!widget.is_grabbing_mouse) {
            return;
        }
        auto w = cast(Slider)widget; assert(w);
        if (ev.left_down) {
            double v = slider_map_mouse_to_value(w, ev.p);
            w.value = v;
            ev.alive = false;
        }
    }
    //----SCROLLBAR---------------------------------------------------------
    enum ScrollArrow{ Up, Down, Right, Left }

    void scrollbar_register() {
        addHandlers!(ScrollBar.ScrollButton)(
            &scrollbutton_draw, &scrollbutton_best_size, &scrollbutton_min_size);
        //addEventHandlers!(ScrollBar.ScrollButton)(
        //    &scrollbutton_mouse_button, &scrollbutton_mouse_move);
        addHandlers!(ScrollBar.ScrollTrack)(
            &scrolltrack_draw, &scrolltrack_best_size, &scrolltrack_min_size);
        //addEventHandlers!(ScrollBar.ScrollTrack)(
        //    &scrolltrack_mouse_button, &scrolltrack_mouse_move);
        
        Element[] telems = new Element[2];
        Element e;
        

        e.texColor.states[ WidgetState.Rollover ] =  Color(255, 255, 255, 160);
        e.texColor.states[ WidgetState.Normal ] =    Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Depressed ] = Color(150, 150, 150, 255);
        e.texColor.states[ WidgetState.Normal ] =    Color(255, 255, 255, 150);
        e.texColor.states[ WidgetState.Focus ] =     Color(255, 255, 255, 200);

        // track
        e.set_texture( m_texid, [243, 144, 265, 155]);
        e.texColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 255);
        telems[0] = e;
    
        // thumb
        e.set_texture( 0, [266, 123, 286, 167] );
        telems[1] = e;

        m_elements[ScrollBar.ScrollTrack.classinfo] = telems;

        Element[] belems = new Element[4];
        // uparrow
        e.set_texture( m_texid, [243, 124, 265, 144] );
        e.texColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 255);
        belems[ScrollArrow.Up] = e;
    
        // downarrow
        e.set_texture( m_texid, [243, 155, 265, 176] );
        e.texColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 255);
        belems[ScrollArrow.Down] = e;

        // rightarrow
        e.set_texture( m_texid, [243, 180, 265, 200] );
        e.texColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 255);
        belems[ScrollArrow.Right] = e;
    
        // leftarrow
        e.set_texture( m_texid, [243, 211, 265, 232] );
        e.texColor.states[ WidgetState.Disabled ] = Color(200, 200, 200, 255);
        belems[ScrollArrow.Left] = e;

        m_elements[ScrollBar.ScrollButton.classinfo] = belems;

    }
    Size scrollbutton_min_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        Size sz = Size(elem[0].texRect[2]-elem[0].texRect[0],
                       elem[0].texRect[3]-elem[0].texRect[1]);
        sz *= 0.5;
        return sz;
    }
    Size scrollbutton_best_size(Widget widget, Size bounds) {
        return scrollbutton_min_size(widget,bounds);
    }
    Size scrolltrack_min_size(Widget widget, Size bounds) {
        return slider_min_size(widget,bounds);
    }
    Size scrolltrack_best_size(Widget widget, Size bounds) {
        return slider_best_size(widget,bounds);
    }
    void scrollbutton_draw(Widget widget) {
        auto w = cast(Button)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        with(w) {
            WidgetState iState = WidgetState.Normal;
            if( !enabled ) {
                iState = WidgetState.Disabled;
            }
            Rect r = rect;
            ScrollArrow dirn;
            switch(w.label) {
            case "up":     dirn = ScrollArrow.Up; break;
            case "down":   dirn = ScrollArrow.Down; break;
            case "right":  dirn = ScrollArrow.Right; break;
            default:       dirn = ScrollArrow.Left;
            }
            elem[dirn].texColor.blend( iState, m_elapsedTime, 0.8);
            draw_elem_texture(elem[dirn], r);
        }
    }
    void scrolltrack_draw(Widget widget) {
        auto w = cast(ScrollBar.ScrollTrack)widget; assert(w);
        Element[] elem = get_widget_elements(w);
        with(w) {
            WidgetState iState = WidgetState.Normal;
            if( !enabled ) {
                iState = WidgetState.Disabled;
            }

            Rect track_rect = rect;
            elem[0].texColor.blend( iState, m_elapsedTime, 0.8 );
            if (horizontal) {
                draw_elem_texture( elem[0], track_rect );
            }
            else {
                glPushMatrix();
                // Rotate rectangle
                Rect r = Rect(0,0,track_rect.h,track_rect.w);
                r.x = -r.w/2;
                r.y = -r.h/2;
                glTranslatef(rect.x+rect.w/2, rect.y+rect.h/2, 0);
                glRotatef(90,0,0,1);
                draw_elem_texture( elem[0], r );
                glPopMatrix();
            }

            Rect thumb_rect = rect;
            int thumb_w = (elem[1].texRect[2] - elem[1].texRect[0])/2;
            int thumb_h = (elem[1].texRect[3] - elem[1].texRect[1])/2;
            thumb_rect.size = Size(thumb_w,thumb_h);
            if (thumb_rect.w > rect.w) {
                float shrinkby = rect.w/thumb_rect.w;
                thumb_rect.h *= shrinkby;
                thumb_rect.w = rect.w;
            }
            if (thumb_rect.h > rect.h) {
                float shrinkby = rect.h/thumb_rect.h;
                thumb_rect.w *= shrinkby;
                thumb_rect.h = rect.h;
            }
            elem[1].texColor.blend( iState, m_elapsedTime, 0.8 );
            if ( horizontal ) {
                float t = (value-value_min)/(value_max-value_min);
                float v = rect.x + thumb_rect.w/2 + t*(rect.w-thumb_rect.w);
                thumb_rect.x = v-thumb_rect.w/2;
                thumb_rect.y = track_rect.y + (track_rect.h-thumb_rect.h)/2;
                draw_elem_texture( elem[1], thumb_rect );
            } else {
                float t = (value-value_min)/(value_max-value_min);
                float v = rect.y2 - thumb_rect.w/2 - t*(rect.h-thumb_rect.h);
                thumb_rect = Rect(-thumb_rect.h/2,-thumb_rect.w/2, 
                                  thumb_rect.h,   thumb_rect.w);
                glPushMatrix();
                glTranslatef(rect.x+rect.w/2, v, 0);
                glRotatef(90,0,0,1);
                draw_elem_texture( elem[1], thumb_rect );
                glPopMatrix();
            }
        }
    }
    //----PANEL-------------------------------------------------------------
    void panel_register() {
    }
    //----SPINNER-----------------------------------------------------------
    void spinner_register() {
    }
    //----OPTIONMENU--------------------------------------------------------
    void optionmenu_register() {
    }

    private:
    Font m_default_font;
    GLuint m_texid = 0;
    int m_texw = 0;
    int m_texh = 0;
    Element[][ClassInfo] m_elements;

    Color sel_text_color;
    Color sel_text_bkg_color;
    Color sel_text_unfocus_bkg_color;
        
    PerformanceCounter m_timer;
    float m_elapsedTime = 0;
}




//============================================================================
enum WidgetState
{
    Normal = 0,
    Disabled,
    Hidden,
    Focus,
    Rollover,
    Depressed
}

struct BlendColor
{
    static BlendColor opCall(
        Color defaultColor, 
        Color disabledColor = Color(128, 128, 128, 200), 
        Color hiddenColor = Color(0,0,0,0) ) 
    {
        BlendColor bc; bc.set(defaultColor,disabledColor,hiddenColor);
        return bc;
    }
    void set( Color defaultColor, 
              Color disabledColor = Color(128, 128, 128, 200), 
              Color hiddenColor = Color(0,0,0,0) )
    {
        for( int i=0; i <= WidgetState.max; i++ ) {
            states[ i ] = defaultColor;
        }
        states[ WidgetState.Disabled ] = disabledColor;
        states[ WidgetState.Hidden ] = hiddenColor;
        current = hiddenColor;
    }
    void blend( WidgetState iState, float fElapsedTime, float fRate = 0.7f )
    {
        current = Color.lerp(current, states[iState], 1.0f - pow( fRate, 30. * fElapsedTime ) );
    }

    ubyte alpha() { return current.a; }
    
    Color states[ WidgetState.max+1 ]; // Modulate colors for all possible control states
    Color current;
}

struct Element
{
    void set_texture(uint iTexID, int[4] textureRect, 
                     Color defaultTexColor = Color(255,255,255,255))
    {
        iTexture = iTexID;
        texRect[] = textureRect[];
    
        texColor.set( defaultTexColor );
    }
    void set_font( Font fnt, Color defaultFontColor = Color(255, 255, 255, 255) ) 
    {
        font = fnt;
        fontColor.set(defaultFontColor);
    }

    void refresh() {
        texColor.current = texColor.states[ WidgetState.Hidden ];
        fontColor.current = fontColor.states[ WidgetState.Hidden ];
    }

    uint iTexture;          // Index of the texture for this Element 
    Font font;              // Font object for this Element

    int[4] texRect;       // Bounding rect of this element on the composite texture
    Rect elemRect;        // where this element will be drawn
    
    BlendColor texColor;
    BlendColor fontColor;
}


//============================================================================
// DDS TEXTURE UTILITIES

alias writefln report_error;

// Struct & defines modified from directx sdk's ddraw.h
const uint DDS_CAPS               = 0x00000001L;
const uint DDS_HEIGHT             = 0x00000002L;
const uint DDS_WIDTH              = 0x00000004L;
const uint DDS_RGB                = 0x00000040L;
const uint DDS_PIXELFORMAT        = 0x00001000L;
const uint DDS_LUMINANCE          = 0x00020000L;

const uint DDS_ALPHAPIXELS        = 0x00000001L;
const uint DDS_ALPHA              = 0x00000002L;
const uint DDS_FOURCC             = 0x00000004L;
const uint DDS_PITCH              = 0x00000008L;
const uint DDS_COMPLEX            = 0x00000008L;
const uint DDS_TEXTURE            = 0x00001000L;
const uint DDS_MIPMAPCOUNT        = 0x00020000L;
const uint DDS_LINEARSIZE         = 0x00080000L;
const uint DDS_VOLUME             = 0x00200000L;
const uint DDS_MIPMAP             = 0x00400000L;
const uint DDS_DEPTH              = 0x00800000L;

const uint DDS_CUBEMAP            = 0x00000200L;
const uint DDS_CUBEMAP_POSITIVEX  = 0x00000400L;
const uint DDS_CUBEMAP_NEGATIVEX  = 0x00000800L;
const uint DDS_CUBEMAP_POSITIVEY  = 0x00001000L;
const uint DDS_CUBEMAP_NEGATIVEY  = 0x00002000L;
const uint DDS_CUBEMAP_POSITIVEZ  = 0x00004000L;
const uint DDS_CUBEMAP_NEGATIVEZ  = 0x00008000L;

align(1) struct DDSInfo
{
    uint       size;                 // size of the structure
    uint       flags;                // determines what fields are valid
    uint       height;               // height of surface to be created
    uint       width;                // width of input surface
    uint       linearSize;           // Formless late-allocated optimized surface size
    uint       depth;                // Depth for volume textures
    uint       mipMapCount;          // number of mip-map levels requestde
    uint       alphaBitDepth;        // depth of alpha buffer requested
    uint[10]   unused;

    uint       pixFmtSize;           // size of pixelformat structure
    uint       pixFmtFlags;          // pixel format flags
    char[4]    fourCC;               // (FOURCC code)
    uint       RGBBitCount;          // how many bits per pixel
    uint       RBitMask;             // mask for red bit
    uint       GBitMask;             // mask for green bits
    uint       BBitMask;             // mask for blue bits
    uint       RGBAlphaBitMask;      // mask for alpha channel

    uint       caps;         // capabilities of surface wanted
    uint       caps2;
    uint       caps3;
    uint       caps4;

    uint       textureStage;         // stage in multitexture cascade
}

struct DDSImageData
{
    GLsizei  width;
    GLsizei  height;
    GLint    components;
    GLenum   format;
    int      numMipMaps;
    GLubyte[] pixels;

    char[] toString() { 
        return std.string.format(
"{
    width = %s;
    height = %s;
    components = %s;
    format = %s;
    numMipMaps = %s;
    pixels.sizeof = %s;
}", width,height,components,format,numMipMaps,pixels.length);
    }
}

//-----------------------------------------------------------------------------
// Name: loadCompressedTexture()
// Desc: 
//-----------------------------------------------------------------------------
void loadCompressedTexture(char[] filename, inout GLuint texid)
{
    DDSImageData *pDDSdata = loadDDSTextureFile( filename );

    if( pDDSdata )
    {
        int nHeight     = pDDSdata.height;
        int nWidth      = pDDSdata.width;
        int nNumMipMaps = pDDSdata.numMipMaps;

        int nBlockSize;

        if( pDDSdata.format == GL_COMPRESSED_RGBA_S3TC_DXT1_EXT )
            nBlockSize = 8;
        else
            nBlockSize = 16;

        glGenTextures( 1, &texid );
        glBindTexture( GL_TEXTURE_2D, texid );

        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );

        int nSize;
        int nOffset = 0;

        // Load the mip-map levels

        for( int i = 0; i < nNumMipMaps; ++i )
        {
            if( nWidth  == 0 ) nWidth  = 1;
            if( nHeight == 0 ) nHeight = 1;

            nSize = ((nWidth+3)/4) * ((nHeight+3)/4) * nBlockSize;

            glCompressedTexImage2DARB( GL_TEXTURE_2D,
                                       i,
                                       pDDSdata.format,
                                       nWidth,
                                       nHeight,
                                       0,
                                       nSize,
                                       &pDDSdata.pixels[0] + nOffset );

            nOffset += nSize;

            // Half the image size for the next mip-map level...
            nWidth  = (nWidth  / 2);
            nHeight = (nHeight / 2);
        }
    }

    if(pDDSdata)
    {
        delete pDDSdata.pixels;
        delete pDDSdata;
    }
}

import std.c.stdio;
import std.c.string;
import std.intrinsic : bswap;

//-----------------------------------------------------------------------------
// Name: loadDDSTextureFile()
// Desc: 
//-----------------------------------------------------------------------------
DDSImageData* loadDDSTextureFile( char[] filename )
{
    uint bswapLE(inout uint v) {
        version(BigEndian) v = bswap(v);
        return v;
    }

    char[4] filecode;
    FILE *pFile;
    int factor;
    int bufferSize;

    // Open the file
    pFile = fopen( std.string.toStringz(filename), "rb" );

    if( !pFile )
    {
        char[] str = std.string.format(
            "loadDDSTextureFile couldn't find, or failed to load \"%s\"", filename);
        report_error(str);
        return null;
    }
    scope(exit) fclose(pFile);

    // Verify the file is a true .dds file
    uint nread = fread( filecode.ptr, 1, 4, pFile );
    assert(nread == 4);

    if( filecode !=  "DDS " )
    {
        char[] str = std.string.format(
            "The file \"%s\" doesn't appear to be a valid .dds file!", filename );
        report_error(str);
        return null;
    }

    // Get the surface descriptor
    DDSInfo ddsinfo;
    nread = fread( &ddsinfo, ddsinfo.sizeof, 1, pFile );

    nread *= ddsinfo.sizeof;
    if (nread < bswapLE(ddsinfo.size)) {
        fseek( pFile, ddsinfo.size-nread, 1 );
    }
    
    auto pDDSdata = new DDSImageData;

    //
    // This .dds loader supports the loading of compressed formats DXT1, DXT3 
    // and DXT5.
    //

    bswapLE(ddsinfo.width);
    bswapLE(ddsinfo.height);
    bswapLE(ddsinfo.depth);
    if (ddsinfo.depth == 0) ddsinfo.depth = 1;
    uint block_size = ((ddsinfo.width + 3)/4) * ((ddsinfo.height + 3)/4) * ddsinfo.depth;

    switch( ddsinfo.fourCC )
    {
        case "DXT1":
            pDDSdata.format = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
            factor = 2;
            block_size *= 8;
            break;

/*
        case "DXT2":
            pDDSdata.format = GL_COMPRESSED_RGBA_S3TC_DXT2_EXT;
            factor = 4;
            block_size *= 16;
            break;
*/
        case "DXT3":
            pDDSdata.format = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
            factor = 4;
            block_size *= 16;
            break;

/*
        case "DXT4":
            pDDSdata.format = GL_COMPRESSED_RGBA_S3TC_DXT4_EXT;
            factor = 4;
            block_size *= 16;
            break;
*/
        case "DXT5":
            pDDSdata.format = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
            factor = 4;
            block_size *= 16;
            break;

        default:
            char[] str = std.string.format(
                "The file \"%s\" doesn't appear to be compressed "
                "using DXT1, DXT3, or DXT5!", filename );
            report_error(str);
            return null;
    }

    //
    // How big will the buffer need to be to load all of the pixel data 
    // including mip-maps?
    //

    bswapLE(ddsinfo.flags);
    bswapLE(ddsinfo.linearSize);
    bswapLE(ddsinfo.mipMapCount);

	if (!(ddsinfo.flags & (DDS_LINEARSIZE | DDS_PITCH))
		|| ddsinfo.linearSize == 0) 
    {
		ddsinfo.flags |= DDS_LINEARSIZE;
		ddsinfo.linearSize = block_size;
	}

    if( ddsinfo.linearSize == 0 )
    {
        report_error("linearSize is 0!");
    }

    if( ddsinfo.mipMapCount > 1 )
        bufferSize = ddsinfo.linearSize * factor;
    else
        bufferSize = ddsinfo.linearSize;

    pDDSdata.pixels.length = bufferSize;

    fread( pDDSdata.pixels.ptr, 1, bufferSize, pFile );
    // need to do an endian swap on pixels for big-endian systems?

    pDDSdata.width      = ddsinfo.width;
    pDDSdata.height     = ddsinfo.height;
    pDDSdata.numMipMaps = ddsinfo.mipMapCount;
    if (pDDSdata.numMipMaps == 0) pDDSdata.numMipMaps = 1;

    if( ddsinfo.fourCC == "DXT1" )
        pDDSdata.components = 3;
    else
        pDDSdata.components = 4;

    return pDDSdata;
}


void dump_ddsinfo(inout DDSInfo d)
{
    writefln("dwSize = %s", d.size);
    writefln("dwFlags = %s", d.flags);
    writefln("dwHeight = %s", d.height);
    writefln("dwWidth = %s", d.width);
    writefln("dwLinearSize = %s", d.linearSize);
    writefln("dwDepth = %s", d.depth);
    writefln("dwMipMapCount = %s", d.mipMapCount);
    writefln("dwAlphaBitDepth = %s", d.alphaBitDepth);
    

}
