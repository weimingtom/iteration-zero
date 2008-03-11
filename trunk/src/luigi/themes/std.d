//---------------------------------------------------------------------
/*
  luigi/themes/std.d -- A standard Windows-classic like theme.

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
module luigi.themes.std;

import luigi.base;
import luigi.theme;
import luigi.opengl;
import luigi.themes.bitmaps;
import luigi.gldraw;
import gldraw = luigi.gldraw;
import luigi.font;
import luigi.event;
import luigi.gui;

alias StdTheme Theme;
class StdTheme : ThemeBase
{
    this() {
        // Register all known size and draw functions.
        label_register();
        button_register();
        checkbox_register();
        textfield_register();
        slider_register();
        panel_register();
        spinner_register();
        /*radiogroup_register();*/
        radiobutton_register();

        m_default_font = new ASCIIBitmapFont();

        bkg_color.set(212,208,200);
        text_color.set(0,0,0);

        sel_text_color.set(255,255,255);
        sel_text_bkg_color.set(10,36,106);
        sel_text_unfocus_bkg_color.set(176,176,176);

        hilite_color[0].set(64,64,64);
        hilite_color[1].set(128,128,128);
        hilite_color[2].set(255,255,255);

        m_bitmaps = new StdBitmapSet;

        // Some default colors
        set_background_color(Overlay.classinfo, Color(0,0,0,0));
    }

    class ThemeData {
        Color[char[]] colors;
    }

    // Public interface functions for users
    Color get_theme_color(Widget w, char[] key, inout Color default_) {
        ThemeData td = get_theme_data(w);
        if (td && key in td.colors) {  return td.colors[key]; }
        return get_theme_color(w.classinfo, key, default_);
    }
    Color get_theme_color(ClassInfo klass, char[] key, inout Color default_) {
        // Search parents too?
        ThemeData td = get_theme_data(klass);
        if (td && key in td.colors) {  return td.colors[key]; }
        return default_;
    }

    Color get_background_color(Widget w) {
        return get_theme_color(w, "background", bkg_color);
    }
    Color get_background_color(ClassInfo klass) {
        return get_theme_color(klass, "background", bkg_color);
    }
    void set_background_color(Widget w, Color c) {
        getadd_theme_data(w).colors["background"] = c;
    }
    void set_background_color(ClassInfo klass, Color c) {
        getadd_theme_data(klass).colors["background"] = c;
    }

    //============================================================================
    // Built-in Drawing and size routines

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

    //----------------------------------------------------------------------
    ThemeData get_theme_data(Widget w) {
        Object td=null;
        if (w && (td=w.theme_instance_data)!is null) {
            return cast(ThemeData)td;
        }
        return null;
    }
    ThemeData getadd_theme_data(Widget w) {
        Object td;
        if (w && (td=w.theme_instance_data)!is null) {
            return cast(ThemeData)td;
        }
        else {
            ThemeData td2 = new ThemeData;
            w.theme_instance_data = td2;
            return td2;
        }
    }
    ThemeData add_theme_data(Widget w) {
        ThemeData td = new ThemeData;
        w.theme_instance_data = td;
        return td;
    }

    ThemeData get_theme_data(ClassInfo klass) {
        if (klass in m_classThemeData) {
            return m_classThemeData[klass];
        }
        return null;
    }
    ThemeData getadd_theme_data(ClassInfo klass) {
        if (klass in m_classThemeData) {
            return m_classThemeData[klass];
        }
        else {
            ThemeData td = new ThemeData;
            m_classThemeData[klass] = td;
            return td;
        }
    }
    ThemeData add_theme_data(ClassInfo klass) {
        ThemeData td = new ThemeData;
        m_classThemeData[klass] = td;
        return td;
    }
    //----------------------------------------------------------------------


    void draw_text_plain(char[] label, float x, float y)
    {
        alias m_default_font font;
        Point trans = font.origin; trans.x+=x; trans.y+=y;
        translate(trans);
        font.draw_string(label);
        untranslate(trans);
    }

    /** Draw text so the upper left corner is at (x,y) */
    void draw_text(char[] label, float x, float y, bool enabled=true)
    {
        alias m_default_font font;
        Point trans = font.origin; trans.x+=x; trans.y+=y;
        translate(trans);
        if (!enabled) {
            set_color( hilite_color[2] );
            translate(1,1);
              font.draw_string(label);
            translate(-1,-1);
        }
        set_color( enabled ? text_color : hilite_color[0] );
        font.draw_string(label);
        untranslate(trans);
    }

    /** Draw text so the first character's origin is at x,y.  Not so useful. */
    void draw_label_text(char[] label, float x, float y, bool enabled=true)
    {
        alias m_default_font font;
        translate(x,y);
        if (!enabled) {
            set_color( hilite_color[2] );
            translate(1,1);
            font.draw_string(label);
            translate(-1,-1);
        }
        set_color( enabled ? text_color : hilite_color[0] );
        font.draw_string(label);
        translate(-x,-y);
    }


    //----OVERLAY-----------------------------------------------------------------
    void begin_drawing(Rect r) {
        gldraw.push_graphics_state(r);
    }
    void end_drawing() {
        gldraw.pop_graphics_state();
    }
    //----PANEL-------------------------------------------------------------------
    void panel_register() {
        m_drawFuncs.add(Panel.classinfo, &panel_draw);
    }

    void panel_draw(Widget widget) {
        auto w = cast(Panel)widget; assert(w);
        if (set_color(get_background_color(widget)))
            fill_rect(w.rect);
    }

    //----LABEL-------------------------------------------------------------------
    void label_register() {
        addHandlers!(Label)(&label_draw, &label_best_size);
    }
    Size label_best_size(Widget widget, Size bounds) {
        auto w = cast(Label)widget; assert(w);
        Rect r = m_default_font.string_rect(w.label);
        return Size(r.width+2,r.height+2);
    }
    void label_draw(Widget widget) {
        auto w = cast(Label)widget; assert(w);
        set_color(get_background_color(widget));
        fill_rect(w.rect);
        with (w) {
            alias m_default_font f;
            
            Rect sr = f.string_rect(label);

            float x = w.rect.x + min(0.0,(w.rect.w-sr.w)/2);
            float y = w.rect.y + min(0.0,(w.rect.h-sr.h)/2);
            draw_text(label,x,y,enabled);
        }
    }

    //----BUTTON------------------------------------------------------------------
    void button_register() {
        addHandlers!(Button)(&button_draw, &button_best_size, &button_min_size);
    }
    Size button_min_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Rect r = m_default_font.string_rect(w.label);
        return Size(r.width+5,r.height+5);
    }
    Size button_best_size(Widget widget, Size bounds) {
        auto w = cast(Button)widget; assert(w);
        Size minsz = button_min_size(w,bounds);
        return Size(max(50.,minsz.w), max(22.,minsz.h));
    }
    void button_draw(Widget widget) {
        auto w = cast(Button)widget; assert(w);
        with (w) {
            alias m_default_font f;
            
            set_color(get_background_color(widget));
            fill_rect(rect);
            if (w.depressed ^ w.checked) {
                stroke_sunken_rect(
                    rect,bkg_color,hilite_color[0],hilite_color[1],hilite_color[2]
                    );
            } else {
                stroke_raised_rect(
                    rect,bkg_color,hilite_color[0],hilite_color[1],hilite_color[2]
                    );
            }

            Rect sr = f.string_rect(label);

            float shift = (w.depressed^w.checked)?1:0;
            float x = w.rect.x+(w.rect.w-sr.w)/2 + shift;
            float y = w.rect.y+(w.rect.h-sr.h)/2 + shift;
            draw_text(label, x, y, enabled);

            if (w.focused()) {
                set_color(hilite_color[0]);
                Rect r = w.rect();
                r.inset(3);
                glEnable(GL_LINE_STIPPLE);
                glLineStipple( 1, 0x5555 );
                stroke_rect(r);
                glDisable(GL_LINE_STIPPLE);
            }
        }
    }

    //----CHECKBOX----------------------------------------------------------------
    void checkbox_register() {
        addHandlers!(Checkbox)(&checkbox_draw, &checkbox_best_size);
    }
    //Size checkbox_min_size(Widget widget, Size bounds) {
    //    auto w = cast(Checkbox)widget; assert(w);
    //    return Size(0,0);
    //}
    Size checkbox_best_size(Widget widget, Size bounds) {
        // Drawing basics
        //    [x] the label string
        // we want 2 pixel clearance around string, 1 pixel around box

        auto w = cast(Checkbox)widget; assert(w);
        Size cbsize = m_bitmaps[StdBitmaps.Checkbox_On].size;
        Size text_size = m_default_font.string_rect( w.label ).size;
        Size sz;
        sz.width = (cbsize.width+2) + (text_size.width+4);
        sz.height = max(cbsize.height+2, text_size.height+4);
        return sz;
    }
    void checkbox_draw(Widget widget) {
        alias m_default_font font;
        auto w = cast(Checkbox)widget; assert(w);
        set_color(get_background_color(widget));
        fill_rect(w.rect);
        with (w)
        {
            Size cbsize = m_bitmaps[StdBitmaps.Checkbox_On].size;
            Point cbpos = rect.pos;
            cbpos.x += 1;
            cbpos.y = rect.y+(rect.h-cbsize.h)/2;
            if (value != 0 ^ depressed) {
                if ( enabled ) 
                    m_bitmaps.draw( StdBitmaps.Checkbox_On, cbpos );
                else
                    m_bitmaps.draw( StdBitmaps.Checkbox_On_Dis, cbpos );
            }
            else {
                if ( enabled )
                    m_bitmaps.draw( StdBitmaps.Checkbox_Off, cbpos );
                else
                    m_bitmaps.draw( StdBitmaps.Checkbox_Off_Dis, cbpos );      
            }

            Rect text_rect = font.string_rect( label );
            text_rect.x = rect.x + (cbsize.w+2)+2;
            text_rect.y = rect.y + 2;
            {
                if ( focused ) {
                    glEnable( GL_LINE_STIPPLE );
                    glLineStipple( 1, 0x5555 );
                    set_color( hilite_color[0] );

                    Rect focus_rect = text_rect;
                    focus_rect.inset(-1);
                    stroke_rect(focus_rect);
  
                    glDisable( GL_LINE_STIPPLE );
                }
            }
            
            draw_text( label, text_rect.x, text_rect.y, enabled);
        }
    }
    
    //----TEXTFIELD-------------------------------------------------------------
    void textfield_register() {
        addHandlers!(TextField)(&textfield_draw, &textfield_best_size, &textfield_min_size);
        addEventHandlers!(TextField)(&textfield_mouse_button, &textfield_mouse_move);
    }
    const int textfield_base_scroll = 3;

    /** Returns a double indicating the position of the x value.
        If exactly on a char boundary then the return value will be
        integral, if right in the middle of a character it will be 
        0.5 offset from an integer. 
    */
    void textfield_get_rects(TextField w, inout Rect entry, inout Rect txt)
    {
        entry = w.rect;
        entry.y+=1;
        entry.h-=2;
        txt = entry;
        txt.inset(1); txt.w-=1; txt.h-=1;
    }
    float textfield_get_char_pos(TextField txtf, float clickx)
    {
        Rect entry_rect,inside_rect;
        textfield_get_rects(txtf, entry_rect, inside_rect);
        alias m_default_font font;
        float offset = inside_rect.x + textfield_base_scroll + txtf.xscroll_offset;
        float x = offset;
        float lastx = x-10;
        int imax = txtf.text.length;
        // binary search would of course be better here
        int i;
        for(i=0; x<clickx && i<imax; x=offset+font.string_rect(txtf.text[0..++i]).w)
        {
            lastx = x;
        }
        if (i==0) return 0;
        else if (i>=imax) return imax;
        
        float frac = (clickx-lastx)/(x-lastx);
        return i-1.0+frac;

    }
    Size textfield_min_size(Widget widget, Size bounds) {
        auto w = cast(TextField)widget; assert(w);
        Size sz = Size(20,2);
        return sz;
    }
    Size textfield_best_size(Widget widget, Size bounds) {
        auto w = cast(TextField)widget; assert(w);
        Size sz = textfield_min_size(w,bounds);
        return Size(max(130.,sz.x),max(20.,sz.h));
    }
    void textfield_draw(Widget widget) 
    {
        auto txtf = cast(TextField)widget; assert(txtf);
        set_color(get_background_color(widget));
        fill_rect(txtf.rect);
        translate(txtf.rect.x, txtf.rect.y);

        alias m_default_font font;
        with (txtf) {
            Rect entry_rect,inside_rect;
            textfield_get_rects(txtf, entry_rect, inside_rect);
            entry_rect.pos -= rect.pos;
            inside_rect.pos -= rect.pos;

            set_color(hilite_color[2]);
            fill_rect( entry_rect );

            // Find where to draw the text

            // Clip to insides of entry rect
            push_clip_rect( inside_rect );

            float offset = inside_rect.x + textfield_base_scroll + xscroll_offset;
            float sel_offset = font.string_rect(text[0..sel_begin]).width;
            Size sel_size = font.string_rect(text[sel_begin..sel_end]).size;

            // make sure active end of selection is visible
            {
                float sel_x = offset + sel_offset;
                if (sel_active==1) sel_x += sel_size.width;
                float dx;
                if ((dx=sel_x-inside_rect.x)<0) {
                    offset-=dx-2; xscroll_offset-=dx-2;
                }
                else if ((dx=sel_x-inside_rect.x2)>0) {
                    offset-=dx+1; xscroll_offset-=dx+1;
                }
            }

            if (selection_length)
            {
                Rect rsel;
                rsel.x = offset + sel_offset;
                rsel.y = 4;
                rsel.size = sel_size;

                // draw unselected first part of text
                if (sel_begin!=0) {
                    draw_text(text[0..sel_begin], offset, inside_rect.y);
                    offset += sel_offset;
                }
                
                // Draw selection bkg
                if (focused) {
                    set_color(sel_text_bkg_color);
                } else {
                    set_color(sel_text_unfocus_bkg_color);
                }
                fill_rect( rsel );

                // draw selected text
                set_color(sel_text_color);
                draw_text_plain(text[sel_begin..sel_end],
                                offset,
                                inside_rect.y);
                offset += rsel.width;

                // draw rest of unselected text
                if (sel_end!=text.length) {
                    draw_text(text[sel_end..$], offset, inside_rect.y);
                }
            }
            else {
                // Draw all the text in one go
                draw_text(text, offset, inside_rect.y);
            }
  
            // Draw some outline rects;
            float w = rect.width-1;
            float h = rect.height-1;

            // Caret
            if ( enabled && selection_length == 0 ) {
                if (focused && selection_length==0)
                {
                    float x = offset + sel_offset;
                    set_color(hilite_color[0]);
                    glBegin( GL_LINE_LOOP );
                    glVertex2f( x, 0 + 4 );
                    glVertex2f( x, 0 + 4 );
                    glVertex2f( x, 0 + h - 3 );
                    glVertex2f( x, 0 + h - 3 );
                    glEnd();
                }
            }
            pop_clip_rect();

            translate(0.5,0.5);
            glBegin( GL_LINES );
            set_color(hilite_color[1]);
            glVertex2f( entry_rect.x, 0 );     glVertex2f( w, 0 );
            glVertex2f( entry_rect.x, 0 );     glVertex2f( entry_rect.x, h );     

            set_color(hilite_color[2]);
            glVertex2f( entry_rect.x, h );      glVertex2f( w, h );
            glVertex2f( w, h );                 glVertex2f( w, 0 );

            if ( enabled )
                set_color( hilite_color[0] );
            else
                set_color( .25 );

            glVertex2f( entry_rect.x+1, 1 );     glVertex2f( w-1, 1 );
            glVertex2f( entry_rect.x+1, 1 );     glVertex2f( entry_rect.x+1, h-2 );

            set_color( .75 );
            glVertex2f( entry_rect.x+1, h-1 );     glVertex2f( w-1, h-1 );
            glVertex2f( w-1, h-1 );                glVertex2f( w-1, 1 );
            glEnd();
            translate(-0.5,-0.5);
        }
        translate(-widget.rect.x, -widget.rect.y);
    }
    void textfield_mouse_button(Widget widget, MouseButtonEvent ev)
    {
        if (ev.is_left) 
        {
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
                if (txtf.is_grabbing_mouse) txtf.release_mouse();
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


    //----SLIDER----------------------------------------------------------------
    void slider_register() {
        addHandlers!(Slider)(&slider_draw, &slider_best_size, &slider_min_size);
        addEventHandlers!(Slider)(&slider_mouse_button, &slider_mouse_move);
    }

    static const int scroll_thumb_w =  8;
    static const int scroll_thumb_h = 18;
    static const int slider_track_inset = 2;
    Size slider_min_size(Widget widget, Size bounds) {
        auto w = cast(Slider)widget; assert(w);
        Size sz = Size(scroll_thumb_w+20, scroll_thumb_h+5);
        if (w.vertical) {
            return Size(sz.h,sz.w);
        }
        return sz;
    }
    Size slider_best_size(Widget widget, Size bounds) {
        auto w = cast(Slider)widget; assert(w);
        Size sz = Size(100,scroll_thumb_h+5);
        if (w.vertical) {
            return Size(sz.h,sz.w);
        }
        return sz;
    }
    void slider_draw(Widget widget) {
        auto w = cast(Slider)widget; assert(w);
        set_color(get_background_color(widget));
        fill_rect(w.rect);
        translate(widget.rect.pos); scope(exit) untranslate(widget.rect.pos);
        {
            Rect rect = w.rect;

            const int tw = slider_track_inset;
            Rect rtrack;
            if (w.horizontal) {
                float ctr = 0.5*(rect.h);
                rtrack.set(tw, ctr-tw, rect.w-tw-tw, tw+tw);
            } else {
                float ctr = 0.5*(rect.w);
                rtrack.set(ctr-tw, tw, tw+tw, rect.h-tw-tw);
            }
            stroke_sunken_rect(
                rtrack, bkg_color,hilite_color[0],hilite_color[1],hilite_color[2]);

            Rect rthumb; float vpos;
            float t = w.value/(w.value_max-w.value_min);
            if (w.horizontal) { 
                rthumb.size = Size(scroll_thumb_w,scroll_thumb_h); 
                vpos = tw + t*(rtrack.w-rthumb.w);
                rthumb.x = lrint(vpos);
                rthumb.y = lrint(rect.h-rthumb.h)/2;
            }
            else { 
                rthumb.size = Size(scroll_thumb_h,scroll_thumb_w);
                vpos = tw+(1-t)*(rtrack.h-rthumb.h);
                rthumb.x = lrint(rect.w-rthumb.w)/2;
                rthumb.y = lrint(vpos);
            }
            set_color(bkg_color);
            fill_rect(rthumb);
            stroke_raised_rect(
                rthumb, bkg_color,hilite_color[0],hilite_color[1],hilite_color[2]);

            if (w.focused()) {
                set_color(hilite_color[0]);
                Rect r = rtrack;
                r.enclose(rthumb);
                r.inset(-2);
                glEnable(GL_LINE_STIPPLE);
                glLineStipple( 1, 0x5555 );
                stroke_rect(r);
                glDisable(GL_LINE_STIPPLE);
            }
        }
    }

    double slider_map_mouse_to_value(Slider w, Point p)
    {
        double t;
        const int tw = slider_track_inset;
        if (w.horizontal) {
            double trackmin = w.rect.x + tw;
            double trackmax = w.rect.x2 - tw*2;
            t = (p.x-trackmin)/(trackmax-trackmin);
        } else {
            double trackmin = w.rect.y + tw*2;
            double trackmax = w.rect.y2 - tw*2;
            t = 1 - (p.y-trackmin)/(trackmax-trackmin);
        }
        double v = w.value_min + t*(w.value_max - w.value_min);
        return v;
    }

    void slider_mouse_button(Widget widget, MouseButtonEvent ev)
    {
        auto w = cast(Slider)widget; assert(w);
        if (ev.is_left && ev.is_press) { w.grab_mouse(); }
        else if (ev.is_left && ev.is_release) { w.release_mouse(); }
        double v = slider_map_mouse_to_value(w, ev.p);
        w.m_saveValue = w.value;
        w.value = v;
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
        }
    }
    //----SPINNER----------------------------------------------------------------
    void spinner_register() { 
        //addHandlers!(Spinner)(&spinner_draw, &spinner_best_size);
    }
    /*
    Size spinner_min_size(Widget widget, Size bounds) {
        auto w = cast(Spinner)widget; assert(w);
        return Size(0,0);
    }
    Size spinner_best_size(Widget widget, Size bounds) {
        auto w = cast(Spinner)widget; assert(w);
        return Size(0,0);
    }
    void spinner_draw(Widget widget) {
        auto w = cast(Spinner)widget; assert(w);
        set_color(get_background_color(widget));
        fill_rect(w.rect);
    }
    */
    //----RADIOGROUP----------------------------------------------------------------
/*
    void radiogroup_register() {
        //addHandlers!(RadioGroup)(&radiogroup_draw, &radiogroup_best_size);
    }
    Size radiogroup_min_size(Widget widget, Size bounds) {
        auto w = cast(RadioGroup)widget; assert(w);
        return Size(0,0);
    }
    Size radiogroup_best_size(Widget widget, Size bounds) {
        auto w = cast(RadioGroup)widget; assert(w);
        return Size(0,0);
    }
    void radiogroup_draw(Widget widget) {
        auto w = cast(RadioGroup)widget; assert(w);
        set_color(get_background_color(widget));
        fill_rect(w.rect);
    }
*/
    //----RADIOBUTTON----------------------------------------------------------------
    void radiobutton_register() {
        //addHandlers!(RadioButton)(&radiobutton_draw, &radiobutton_best_size);
    }
    Size radiobutton_min_size(Widget widget, Size bounds) {
        auto w = cast(RadioButton)widget; assert(w);
        return Size(0,0);
    }
    Size radiobutton_best_size(Widget widget, Size bounds) {
        auto w = cast(RadioButton)widget; assert(w);
        return Size(0,0);
    }
    void radiobutton_draw(Widget widget) {
        auto w = cast(RadioButton)widget; assert(w);
        set_color(get_background_color(widget));
        fill_rect(w.rect);
    }
    //----------------------------------------------------------------------------

    Color bkg_color;
    Color text_color;
    Color sel_text_color;
    Color sel_text_bkg_color;
    Color sel_text_unfocus_bkg_color;
    Color hilite_color[3]; // dark medium light

    private:

    Font m_default_font;
    StdBitmapSet m_bitmaps;

    ThemeData[ClassInfo] m_classThemeData;
}
