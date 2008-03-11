//---------------------------------------------------------------------
/*
 Copyright:

  luigi/theme.d -- theme support for 'luigi' user interface library.

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
module luigi.theme;


import luigi.base;
import luigi.opengl;
import luigi.gldraw;
import luigi.font;
import luigi.event;

import luigi.gui;
//import luigi.widgets.widget;

//--------------------------------------------------------------------------
/** An abstract interface for themes used by Luigi */
interface Theme
{
    /** Called before and after refreshing the gui by Overlay.
     *  This is where all common 2D GL state can be set up and restored by 
     *  the theme.
     */
    void begin_drawing(Rect r);
    void end_drawing();

    /** Draws the given item */
    void draw(Widget item);

    /** Sizes used for layout */
    Size minimum_size(Widget item, Size bounds);
    Size preferred_size(Widget item, Size bounds);

    /** Event handling */
    void on_key(Widget item, KeyEvent ev);
    void on_mouse_button(Widget item, MouseButtonEvent ev);
    void on_mouse_move(Widget item, MouseMoveEvent ev);
    void on_mouse_wheel(Widget item, MouseWheelEvent ev);
    void on_focus(Widget item, FocusEvent ev);

    // need sizes
    // need states? naw can get it from control
}

class AbstractTheme : Theme
{
    abstract override void draw(Widget item);
    abstract override Size minimum_size(Widget item, Size bounds);
    abstract override Size preferred_size(Widget item, Size bounds);

    void begin_drawing(Rect r) {}
    void end_drawing() {}

    //============================================================================
    // Event functions
    override void on_key(Widget item, KeyEvent ev) {
        // this really isn't that theme specific...
        item.on_key(ev);
    }
    override void on_mouse_button(Widget item, MouseButtonEvent ev) {
        item.on_mouse_button(ev);
    }
    override void on_mouse_move(Widget item, MouseMoveEvent ev) {
        item.on_mouse_move(ev);
    }
    override void on_mouse_wheel(Widget item, MouseWheelEvent ev) {
        item.on_mouse_wheel(ev);
    }
    override void on_focus(Widget item, FocusEvent ev) {
        item.on_focus(ev);
    }
}

/** This template/mixin provides helper functions used by themes to 
 *  manage per-class instance information.
 */
template WidgetClassInfoDict(FuncT)
{
    /** Lookup Widget class-specific data in a table.
     *  
     * This is like an ordinary AA lookup keyed on ClassInfo, 
     * but if a key is missing it recursively tries all the ClassInfo
     * objects for the base classes as well.  
     * If it finds a match, that match is copied to 
     * table[item.classinfo] for faster future lookups.
     */
    FuncT lookup(inout FuncT[ClassInfo] table, Widget item) {
        // Search for func in the table, going up class hierarchy
        ClassInfo start = item.classinfo;
        if (start in table) return table[start];
        ClassInfo stop = Widget.classinfo;
        for(auto cinfo=start; cinfo!=stop; cinfo=cinfo.base) 
        {
            if (cinfo in table) {
                FuncT f = table[cinfo];
                table[start] = f; // cache result for later
                return f;
            }
        }
        return null;
    }    
    /** Same as lookup(table,item) but use the mixin's builtin m_table */
    FuncT lookup(Widget item) {
        return lookup(m_table,item); 
    }

    /** Add an element to the ClassInfo-keyed table */
    void add(inout FuncT[ClassInfo] table, ClassInfo c, FuncT f) {
        table[c] = f;
    }
    /** Same as add(table,c,f) but use the mixin's builtin m_table */
    void add(ClassInfo c, FuncT f) {
        add(m_table, c, f);
    }
    FuncT[ClassInfo] m_table;
}


class ThemeBase : AbstractTheme
{
    alias Size delegate(Widget item, Size bounds) SizeFunc;
    alias void delegate(Widget item) DrawFunc;

    alias void delegate(Widget item, MouseButtonEvent ev) ButtonFunc;
    alias void delegate(Widget item, MouseMoveEvent ev) MoveFunc;
    alias void delegate(Widget item, KeyEvent ev) KeyFunc;

    void addHandlers(alias Class)(DrawFunc draw, SizeFunc best_sz, SizeFunc min_sz=null )
    {
        m_drawFuncs.add(Class.classinfo, draw);
        m_bestsizeFuncs.add(Class.classinfo, best_sz);
        m_minsizeFuncs.add(Class.classinfo, (min_sz is null)?best_sz : min_sz);
    }
    void addEventHandlers(alias Class)(ButtonFunc btn, MoveFunc move=null, KeyFunc key=null )
    {
        m_btnFuncs.add(Class.classinfo, btn);
        if (move !is null) 
            m_moveFuncs.add(Class.classinfo, move);
        if (key !is null)
            m_moveFuncs.add(Class.classinfo, move);
    }

    protected:
    mixin WidgetClassInfoDict!(DrawFunc)   m_drawFuncs;
    mixin WidgetClassInfoDict!(SizeFunc)   m_minsizeFuncs;
    mixin WidgetClassInfoDict!(SizeFunc)   m_bestsizeFuncs;
    mixin WidgetClassInfoDict!(ButtonFunc) m_btnFuncs;
    mixin WidgetClassInfoDict!(MoveFunc)   m_moveFuncs;
    mixin WidgetClassInfoDict!(KeyFunc)    m_keyFuncs;

    public:

    override void draw(Widget item) {
        DrawFunc f = m_drawFuncs.lookup(item);
        if (f) f(item);
    }

    override Size minimum_size(Widget item, Size bounds) { 
        SizeFunc f = m_minsizeFuncs.lookup(item);
        if (f) return f(item,bounds);
        return Size(0,0);
    }

    override Size preferred_size(Widget item, Size bounds) { 
        SizeFunc f = m_bestsizeFuncs.lookup(item);
        if (f) { 
            return f(item,bounds);
        }
        return Size(50,22); 
    }

    //============================================================================
    // Event functions
    override void on_key(Widget item, KeyEvent ev) 
    {
        KeyFunc f = m_keyFuncs.lookup(item);
        if (f) f(item, ev);
        if (!f || ev.alive) { item.on_key(ev); }
    }
    override void on_mouse_button(Widget item, MouseButtonEvent ev) 
    {
        ButtonFunc f = m_btnFuncs.lookup(item);
        if (f) f(item, ev);
        if (!f || ev.alive) { item.on_mouse_button(ev); }
    }
    override void on_mouse_move(Widget item, MouseMoveEvent ev) 
    {
        MoveFunc f = m_moveFuncs.lookup(item);
        if (f) 
            f(item, ev);
        if (!f || ev.alive) { item.on_mouse_move(ev); }
    }
}



