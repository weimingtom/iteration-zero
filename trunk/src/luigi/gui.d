//---------------------------------------------------------------------
/**
   Luigi is an OpenGL based GUI library.
   
   The goal is to provide a simple but flexible way to add a simple
   GUI to an OpenGL program.

   Luigi supports themes and input adaptors.  Themes give you a way
   to customize the look of GUI.  Input adaptors do the work of
   taking events from the OS or from a windowing toolkit like GLD, and
   turning them into something Luigi can use.
*/ 
//---------------------------------------------------------------------
/*
  luigi/gui.d -- main import file for 'luigi' user interface library.
  version 0.5, December 3, 2006

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
//----------------------------------------------------------------------------
/*
  WARNING: Because of how overloading works in D (at least as of DMD 0.176),
           *******************************************
           ** ALL SETTERS MUST COME BEFORE GETTERS  **
           *******************************************
  I.e.

          int value(int newval)

   should always appear before 

          int value()

   Otherwise, signal.connect(value) will silently connect to the
   getter rather than the setter.
 */
module luigi.gui;

import luigi.opengl;
import luigi.base;
import luigi.event;
import luigi.font;
import luigi.theme;
static import luigi.themes.std;
import luigi.arranger;
import luigi.adapter;
//import sslot.signal;
import luigi.signalobj;

//import adapter = luigi.adapter.glfw;

import drawsys = luigi.gldraw;

import math = std.math;
import string = std.string;
import std.ctype : isprint;
import std.uni : isUniAlpha;
static import std.path;
static import std.file;

// Just for debug?
import std.stdio : writefln;


/** 
 * The singleton master object.  
 * Holds GUI globals including the theme, the input adapter, and keeps track
 * of all the top-level Frames and Overlays.
 */
class Luigi
{
    /// Return the singleton instance
    static Luigi opCall() 
    {
        static Luigi instance = null;
        if (!instance) instance = new Luigi;
        return instance;
    }

    /// Return the singleton instance
    static Luigi inst() {
        return Luigi();
    }

    /// Set the theme
    Theme theme(Theme th) { return m_theme=th; }
    /// Get the current theme
    Theme theme() { 
        if (!m_theme) {
            m_theme = new luigi.themes.std.StdTheme();
        }
        return m_theme; 
    }

    /// Add a location where themes should look for resource files
    /// (e.g. images, textures, etc.  The built-in themes look for some files 
    /// relative to the luigi base directory, so you may need to add it to
    /// your path.)  The current directory '.' is the only thing on the resource path
    /// by default.
    void add_resource_location(char[] path) {
        char[] workp = path.dup;
        if (workp[$-1..$] != std.path.sep) { workp ~= std.path.sep; }
        m_resource_path ~= workp;
    }
    /// Add a location where themes should look for resource files to the beginning of
    /// the resource path.
    /// (e.g. images, textures, etc.  The built-in themes look for some files 
    /// relative to the luigi base directory, so you may need to add it to
    /// your path.)  The current directory '.' is the only thing on the resource path
    /// by default.
    void prepend_resource_location(char[] path) {
        char[] workp = path.dup;
        if (workp[$-1..$] != std.path.sep) { workp ~= std.path.sep; }
        m_resource_path = workp ~ m_resource_path;
    }
    /// Get the current resource path.  This is a list of paths where Luigi 
    /// should look for resource files like images.
    char[][] resource_path() {
        return m_resource_path;
    }
    /// Finds the specified resource file on the resource path.  This could be 
    /// an image or anything else.  Used to find theme textures internally.
    /// A list of names passed in will treated as alternative names for the same
    /// resource file and searched for in the order provided.
    /// Returns null if the resource could not be located on the resource path.
    char[] find_resource_location(char[][] name...) {
        foreach(char[] aname; name) {
            foreach(char[] p; m_resource_path) {
                char[] fullpath = p ~ aname;
                if (std.file.exists(fullpath)) {
                    return fullpath;
                }
            }
        }
        return null;
    }

    /** Add a top level overlay.  
        This is called automatically by Overlay, so typically there is no 
        reason for users to call it themselves.
    */
    void add_overlay(Overlay ov) {
        m_guis ~= ov;

        if (!m_inputsys) {
            throw new GUIException("Must set Luigi().adapter before creating Overlays");
        }

        // add in top-level input hooks
        with (m_inputsys) {
            addSysKeyCallback(&ov.on_sys_key);
            addSysMouseButtonCallback(&ov.on_sys_mouse_button) ;
            addMouseMoveCallback(&ov.on_sys_mouse_move);
            addMouseWheelCallback(&ov.on_sys_mouse_wheel);
            addWindowSizeCallback(&ov.on_sys_window_size);
            addWindowCloseCallback(&ov.on_sys_window_close);
        }
    }

    /** Returns the size of the specified window */
    Size get_window_size(WindowHandle win) {
        return m_inputsys.get_window_size(win);
    }

    /** Set the current input adapter */
    void adapter(InputAdapter inputsys) {
        m_inputsys = inputsys;
    }
    /** Get the current input adapter */
    InputAdapter adapter() {
        return m_inputsys;
    }



private:

    this() {
        add_resource_location(".");
    }

    ~this() {
    }

    Overlay[] m_guis;
    InputAdapter m_inputsys;
    Theme m_theme;
    char[][] m_resource_path;
}


/// Add a user-level input callback functions and delegates
void add_key_callback(KeyEventFn cb)                  {Luigi().adapter.addKeyCallback(cb);}
void add_key_callback(KeyEventDg cb)                  {Luigi().adapter.addKeyCallback(cb);}
void add_mouse_button_callback(MouseButtonEventFn cb) {Luigi().adapter.addMouseButtonCallback(cb); }
void add_mouse_button_callback(MouseButtonEventDg cb) {Luigi().adapter.addMouseButtonCallback(cb);}
void add_mouse_move_callback(MouseMoveEventFn cb)     {Luigi().adapter.addMouseMoveCallback(cb);}
void add_mouse_move_callback(MouseMoveEventDg cb)     {Luigi().adapter.addMouseMoveCallback(cb);}
void add_mouse_wheel_callback(MouseWheelEventFn cb)   {Luigi().adapter.addMouseWheelCallback(cb);}
void add_mouse_wheel_callback(MouseWheelEventDg cb)   {Luigi().adapter.addMouseWheelCallback(cb);}
void add_window_size_callback(WindowSizeEventFn cb)   {Luigi().adapter.addWindowSizeCallback(cb);}
void add_window_size_callback(WindowSizeEventDg cb)   {Luigi().adapter.addWindowSizeCallback(cb);}
void add_window_close_callback(WindowCloseEventFn cb) {Luigi().adapter.addWindowCloseCallback(cb);}
void add_window_close_callback(WindowCloseEventDg cb) {Luigi().adapter.addWindowCloseCallback(cb);}
/// Add a system-level input callback function or delegate -- Use with caution!
void add_sys_key_callback(KeyEventFn cb)                  {Luigi().adapter.addSysKeyCallback(cb);}
void add_sys_key_callback(KeyEventDg cb)                  {Luigi().adapter.addSysKeyCallback(cb);}
void add_sys_mouse_button_callback(MouseButtonEventFn cb) {Luigi().adapter.addSysMouseButtonCallback(cb); }
void add_sys_mouse_button_callback(MouseButtonEventDg cb) {Luigi().adapter.addSysMouseButtonCallback(cb); }


/// Remove installed callbacks
void remove_key_callback(KeyEventFn cb)                  {Luigi().adapter.removeKeyCallback(cb);}
void remove_key_callback(KeyEventDg cb)                  {Luigi().adapter.removeKeyCallback(cb);}
void remove_mouse_button_callback(MouseButtonEventFn cb) {Luigi().adapter.removeMouseButtonCallback(cb); }
void remove_mouse_button_callback(MouseButtonEventDg cb) {Luigi().adapter.removeMouseButtonCallback(cb); }
void remove_mouse_move_callback(MouseMoveEventFn cb)     {Luigi().adapter.removeMouseMoveCallback(cb);}
void remove_mouse_move_callback(MouseMoveEventDg cb)     {Luigi().adapter.removeMouseMoveCallback(cb);}
void remove_mouse_wheel_callback(MouseWheelEventFn cb)   {Luigi().adapter.removeMouseWheelCallback(cb);}
void remove_mouse_wheel_callback(MouseWheelEventDg cb)   {Luigi().adapter.removeMouseWheelCallback(cb);}
void remove_window_size_callback(WindowSizeEventFn cb)   {Luigi().adapter.removeWindowSizeCallback(cb);}
void remove_window_size_callback(WindowSizeEventDg cb)   {Luigi().adapter.removeWindowSizeCallback(cb);}
void remove_window_close_callback(WindowCloseEventFn cb) {Luigi().adapter.removeWindowCloseCallback(cb);}
void remove_window_close_callback(WindowCloseEventDg cb) {Luigi().adapter.removeWindowCloseCallback(cb);}
void remove_sys_key_callback(KeyEventFn cb)                  {Luigi().adapter.removeSysKeyCallback(cb);}
void remove_sys_key_callback(KeyEventDg cb)                  {Luigi().adapter.removeSysKeyCallback(cb);}
void remove_sys_mouse_button_callback(MouseButtonEventFn cb) {Luigi().adapter.removeSysMouseButtonCallback(cb); }
void remove_sys_mouse_button_callback(MouseButtonEventDg cb) {Luigi().adapter.removeSysMouseButtonCallback(cb); }




/** 
 * Represents a real OS window used exclusively by Luigi. 
 * If the GUI is drawn on top of the app's GL window, use Overlay instead. 
 */
class Frame
{
    // maybe later -- GLFW doesn't have a way to make more than one windowframe.
    // SDL doesn't either.
    // And maybe this should just be Window, because e.g. GLUT has sub-windows that can be
    // treated basically as top level windows.

    private:
    WindowHandle m_winhandle;
}

template WidgetMixin()
{
    alias typeof(this) WidgetType;
    static assert(is(WidgetType:luigi.gui.Widget), 
                  "WidgetMixin should be derived from Widget");
        
    /** 
    This function is a convenient way to add a newly constructed widget
    to an arranger while passing along extra necessary arguments.

    It is always possible to add the widget directy to the parent's arranger using
    the arranger.add(), but this way lets you construct the widget, add it to the 
    arranger with arguments, and assign it to a variable of the derived type all 
    in one line.

    For example:
      Button b = arranger_add(new Button(parent, "Name"), Arranger.Left, Arranger.Top);
    
    Returns: A pointer to 'this', with the proper derived type.
    */
    WidgetType arranged_(VArg...)(VArg varg)
    {
        //static assert(is(T:Widget), "arranger_add requires a Widget as argument 0");
        if (parent && parent.arranger && !parent.arranger.auto_add) {
            parent.arranger.add(this, varg);
        }
        return this;
    }
}

template PanelMixin()
{
    alias typeof(this) PanelType;
    static assert(is(PanelType:luigi.gui.Panel), 
                  "PanelMixin should be derived from Panel");
        
    /** Add the widget to this panel.
     *
     * This method returns the widget passed in with its full derived type,
     * so add_widget can be used like:
     * ---------
     *   auto b = border_panel.add_widget(new Button("Clicky"));
     * ---------
     * and the resulting b will have type Button.
     *
     * See_Also: add_arranged, Panel.add, Arranger.add
     */
    W add_widget(W)(W widget)
    {
        static assert(is(W:luigi.gui.Widget), "add_widget requires a Widget as argument");
        add(widget);
        return widget;
    }

    /** Add the widget to this panel and also to the panel's arranger
     *  with arguments.
     *
     * This method returns the widget passed in with its full derived type,
     * so add_arranged can be used like:
     * ---------
     *   auto b = border_panel.add_arranged(new Button("Clicky"), Region.East);
     * ---------
     * and the resulting b will have type Button.
     *
     * See_Also: add_widget, Panel.add, Arranger.add
     */
    W add_arranged(W, Varg...)(W widget, Varg args)
    {
        static assert(is(W:luigi.gui.Widget),
                      "add_arranged requires a Widget as argument");
        add(widget);
        assert(arranger, "add_arranged called with no arranger set");
        if (arranger && !arranger.auto_add) arranger.add(widget, args);
        return widget;
    }
}

/**
 * The base class for any GUI entity that occupies space on the screen.
 */
class Widget : Arrangeable
{
    mixin WidgetMixin;

    // Some handy aliases everyone should have
    // This makes it so you can refer to these inside a Widget subclass
    // without worry
    alias luigi.base.Size Size;
    alias luigi.base.Rect Rect;
    alias luigi.base.Point Point;


    this() {}
    
    void add(Widget child) {
        throw new GUIException("Attempt to add child to a Widget");  
    }
    
    void draw() {
        // Default is to let the theme draw it.
        if (shown)
            Luigi().theme.draw(this);
    }

    //------------------------------------------------------------------------
    // Default implementation of Arrangeable interface
    override Size minimum_size(Size bounds) {
        return Luigi().theme.minimum_size(this, bounds); 
    }
    override Size preferred_size(Size bounds) { 
        return Luigi().theme.preferred_size(this, bounds);
    }
    override void set_rect(Rect s) { m_rect = s; }
    override void set_position(Point p) { m_rect.x = p.x; m_rect.y = p.y; }
    override void set_size(Size sz) { m_rect.width = sz.width; m_rect.height = sz.height; }
    override void arrange() { /* nothing to do */ }


    /** Return the arranger used to arrange this widget's chilrend */
    Arranger arranger() { return m_arranger; }

    /** Set the arranger used to arrange this widget's chilren */
    Arranger arranger(Arranger a)
    {
        if (a==m_arranger) return m_arranger;
        m_arranger = a;
        m_arranger.set_rect(m_rect);
        return m_arranger;
    }

    /** Return the arranger used to arrange this widget */
    Arranger arranged_by() {
        if (parent) return parent.arranger;
        return null;
    }

    /** Return the items rectangle.  
        The rectangle coordinates are relative to the upper left corner of this
        widget's parent.
    */
    Rect rect() { return m_rect; }
    // Rect rect(Rect r) { return m_rect; }  // use set rect!


    void enable(bool isEnabled=true) { return m_enabled = isEnabled; }
    void disable() { m_enabled = false; }
    bool enabled() { return m_enabled; }
    bool disabled() { return !m_enabled; }
    
    void show() { m_shown = true; }
    void hide() { m_shown = false; }
    bool shown(bool show_) { return m_shown = show_; }
    bool shown() { return m_shown; }
    void toggle_shown() { m_shown = !m_shown; }

    /** Return the parent of this item or null if it has no parent.
    */
    Widget parent() { return m_parent; }

    /** Return the list of items that are parented to this one.
        Panels are the base type for all items with children.
    */
    Widget[] children() { return null; }

    /** Transforms the given point from window coordinates into 
        the widget's coordinates.  In widget coordinates, 
        (rect.x,rect.y) is the upper left corner of the widget.
    */
    void transform_window_to_widget(inout Point winp)
    {
        Widget w = this.parent;
        while( w )
        {
            winp.x -= w.m_rect.x;
            winp.y -= w.m_rect.y;
            w = w.parent;
        }
    }

    /** Return the item after this one in the tab traversal order */
    Widget next_item() { 
        Panel p = cast(Panel)parent;
        if (p) { return p.sibling_after(this); }
        return null;
    }
    /** Return the item before this one in the tab traversal order */
    Widget prev_item() {
        Panel p = cast(Panel)parent;
        if (p) { return p.sibling_before(this); }
        return null;
    }

    /** Find the widget at the root of this item's hierarchy */
    Widget get_root() {
        Widget r = this;
        while ( r.parent ) r = r.parent;
        return r;
    }

    bool is_grabbing_mouse() {
        Widget R = get_root();
        return R._get_mouse_grabber() is this;
    }
    bool grab_mouse() {
        Widget R = get_root();
        return R._set_mouse_grabber(this);
    }
    bool release_mouse() {
        Widget R = get_root();
        return R._release_mouse_grabber(this);
    }
        

    protected bool _set_mouse_grabber(Widget w)
    {
        assert(0, "Widgets don't grab the mouse");
        return false;
    }
    protected bool _release_mouse_grabber(Widget w)
    {
        assert(0, "Widgets don't grab the mouse");
        return false;
    }
    protected Widget _get_mouse_grabber()
    {
        assert(0, "Widgets don't grab the mouse");
        return null;
    }

    //----------------- FOCUS METHODS --------------------------

    /** Returns whether this item has the keyboard focus.
    */
    bool focused()
    in {  get_root()._focus_sanity_check(); } body
    {
        return m_focused;
    }

    bool focusable(bool onOff) 
    in {  get_root()._focus_sanity_check(); } body
    { 
        if (m_focusable == onOff) 
            // no change
            return m_focusable;

        if (!onOff && m_focused) {
            // we were focused but we're being made non-focusable
            // Try to focus the next guy before turning our focus off.
            focus_next();
            if (m_focused) // it didn't work! maybe we're the only item!
                _unfocus_rup();
        }
        m_focusable = onOff;
        return m_focusable;
    }

    bool focusable() 
    in {  get_root()._focus_sanity_check(); } body
    { 
        return m_focusable && m_enabled;
    }

    /** Finds the keyboard focus of this widget's hierarchy. */
    Widget get_focus()
    in {  get_root()._focus_sanity_check(); } body
    {
        Widget R = get_root();
        Widget w = R._find_focus_rdown();
        return w;
    }

    protected Widget _find_focus_rdown()
    {
        writefln("Focus rdown widget");
        // Shouldn't usually get here, unless we have no parent
        assert(parent==null, "I shouldn't even *be* here today.");
        return this;
    }

    /** Sets the keyboard focus of this widget's hierarchy to newfocus.
     *  If newfocus is null any current focus item in this widget tree 
     *  will be unfocused.
     */
    bool set_focus(Widget newfocus)
    in {  get_root()._focus_sanity_check(); } body
    {
        if (newfocus) {
            if (!newfocus.focusable) return false;
            if (newfocus.m_focused) return true;
        }

        // unfocus the current guy first
        Widget oldFocus = get_focus();
        if (oldFocus && !oldFocus._can_lose_focus()) {
            return false;
        }
        else if (oldFocus) {
            oldFocus._unfocus_rup();
        }
        
        // Stop here if there's no new focus item
        if (!newfocus) return true;

        // now give focus to the new guy.
        // Set his m_focused flag and percolate m_childFocused flags up the chain.
        _FocusEvent ev;
        ev.previous = oldFocus;
        newfocus.on_focus(&ev);
        if (!ev.alive) {
            newfocus.m_focused = true;
            if (newfocus.parent) newfocus.parent._set_child_focused_rup();
        }
        return newfocus.m_focused;
    }


    /** Set the keyboard focus to this item.
        Returns true if successful, false otherwise.
    */
    bool focus()
    in {  get_root()._focus_sanity_check(); } body
    {
        return set_focus(this);
    }

    protected void _set_child_focused_rup() {
        // recursively set child focus on all parents
        assert(false, "recursive focus only for Panel-derived classes.");
    }

    protected void _unfocus_rup() {
        // recursively unfocus this item and all parents
        m_focused = false;
        if (parent) parent._unfocus_rup();
    }

    protected Widget _find_first_focusable_rdown() {
        if (focusable) {
            return this;
        }
        return null;
    }
    protected Widget _find_last_focusable_rdown() {
        if (focusable) {
            return this;
        }
        return null;
    }
    protected Widget _find_next_focusable_rdown(inout bool found_focus)
    {
        if (m_focused) { found_focus = true; }
        return null;
    }
    protected Widget _find_prev_focusable_rdown(inout bool found_focus)
    {
        if (m_focused) { found_focus = true; }
        return null;
    }

    protected int _focus_sanity_check(int c=0) {
        return c + (m_focused?1:0);
    }


    /** Moves focus to the next item in this widget's hierarchy, or 
        to the first item if nothing is currently focused. 
        Return the newly focused item.
    */
    Widget focus_next()
    {
        Widget w = get_focus();
        Widget R = get_root();
        if (!w) {
            w = R._find_first_focusable_rdown();
        }
        else {
            bool code = false;
            w = R._find_next_focusable_rdown(code);
            if (!w) {
                w = R._find_first_focusable_rdown();
            }
        }
        if (w.focus()) {
            return w;
        }
        // focus didn't change if we got here
        return this;
    }

    /** Moves focus to the previous item in this widget hierarchy */
    Widget focus_prev()
    {
        Widget w = get_focus();
        Widget R = get_root();
        if (!w) {
            w = R._find_last_focusable_rdown();
        }
        else {
            bool code = false;
            w = R._find_prev_focusable_rdown(code);
            if (!w) {
                w = R._find_last_focusable_rdown();
            }
        }
        if (w.focus()) {
            return w;
        }
        // focus didn't change if we got here
        return this;
    }

    /** Used internally before focusing a new item to make sure it's ok for the 
        current item to lose focus.
    */
    protected bool _can_lose_focus()
    {
        // there is no veto of focus just yet.
        return true;
    }


    bool rollover() { return m_rollover; }

    //---------------------------------------------------
    // Base widget event handling
    void on_key(KeyEvent ev) {
        ev.alive = true;
    }
    void on_mouse_button(MouseButtonEvent ev)
    {
        alias string.toString str;
        version (LuigiTrace)
        writefln("button %s %s: ",str(ev.button), ev.is_press?"press":"release", 
                 "(%s) [%s] ", str(ev.pressed), str(ev.mods), 
                 "(%0.1f,%0.1f) - ", ev.x-rect.x, ev.y-rect.y,
                 this);
        ev.alive = true;
    }
    void on_mouse_move(MouseMoveEvent ev)
    {
        ev.alive = true;
    }
    void on_mouse_wheel(MouseWheelEvent ev)
    {
        ev.alive = true;
    }

    void on_focus(FocusEvent ev) {
        // TODO: do focus change notifications for those who care?
        Button b = cast(Button)this;
    }
    void on_enter(EnterEvent ev) {
        m_rollover = true;
    }

    void on_leave(LeaveEvent ev) {
        m_rollover = false;
    }

    void stretch(ubyte both)       { m_stretchx = m_stretchy = both; }
    void stretch(ubyte x, ubyte y) { m_stretchx = x; m_stretchy = y; }
    int stretch_x(int s) { return m_stretchx=s; }
    int stretch_y(int s) { return m_stretchy=s; }
    int stretch_x() { return m_stretchx; }
    int stretch_y() { return m_stretchy; }

    //------------------------------------------------------------------------
    // Hooks for the theme
    Object theme_instance_data() { return m_themeData; }
    void  theme_instance_data(Object o) { m_themeData = o; }

    protected Widget m_parent = null;

    private:
    Rect m_rect; // always relative to parent
    bool m_focusable = true;
    bool m_shown = true;
    bool m_enabled = true;
    bool m_focused = false;
    bool m_rollover = false;
    Arranger m_arranger = null;
    ubyte m_stretchx = 0; // stretchyness for arrangement 
    ubyte m_stretchy = 0;

    // for any per-instance data needed by theme
    Object m_themeData = null;
}


/**
 * The base class for any GUI entity that occupies space on the screen and can 
 *  potentially have children. 
 */
class Panel : Widget
{
    mixin WidgetMixin;
    mixin PanelMixin;

    this() {
        super();
        m_focusable = false;
    }
    override void add(Widget child) {
        assert(child, ".add called with null child");
        if (!child) return;
        
        m_children ~= child;
        child.m_parent = this;
        if (arranger && arranger.auto_add) {
            arranger.add(child);
            m_needArrange=true;
        }
    }

    override void draw() {
        if (!shown) return;
        _lazy_arrange();
        // draw self, draw children

        super.draw(); // self is just "do what theme tells you"

        drawsys.translate(rect.x, rect.y);
        foreach(c; m_children) {
            if (c.shown) c.draw();
        }
        drawsys.translate(-rect.x, -rect.y);
    }
    
    override void set_rect(Rect r) {
        super.set_rect(r);
        if (arranger) arranger.set_rect(r);
        m_needArrange = true;
    }

    override Size minimum_size(Size bounds)   {
        if (arranger) { return arranger.minimum_size(bounds); }
        return super.minimum_size(bounds);
    }
    override Size preferred_size(Size bounds) {
        if (arranger) { return arranger.preferred_size(bounds); }
        return super.preferred_size(bounds); 
    }

    void _lazy_arrange() {
        if (m_needArrange && arranger) {
            arranger.arrange();
            _updateChildOrder();
            m_needArrange = false;
        }
    }

    override void arrange() {
        m_needArrange = true;
    }

    private void _updateChildOrder() 
    in { assert(arranger); } body
    {
        // Update the child order to reflect the order in the arranger
        int[] order; order.length = m_children.length;
        foreach(i,c; m_children) {
            order[i] = arranger.orderof(c);
        }
        // Now insertion sort the 'order' and children in tandem.
        // Shouldn't be too slow because there generally aren't so many
        // children, and usually you'll be close to sorted order anyway.
        // If you are sorted, insertion sort gives you nice O(N) behavior.

        void insert(int[] a, Widget[] c, int len) {
            int avalue = a[len];
            Widget cvalue = c[len];
            int i;
            for (i = len - 1; i >= 0 && a[i] > avalue; i--)
            {
                // shift right to make room
                a[i+1] = a[i];
                c[i+1] = c[i];
            }
            if (i+1!=len) {
                a[i+1] = avalue;
                c[i+1] = cvalue;
            }
        }
        // insertion sort main loop
        int i = 1;
        while (i < order.length) {
            insert(order, m_children, i);
            i++;
        }
    }

    override Arranger arranger(Arranger a) { 
        super.arranger(a);
        if (arranger && arranger.auto_add()) {
            foreach (c; m_children) 
            {
                arranger.add(c);
            }
            a.arrange();
        }       
        return arranger; 
    }
    override Arranger arranger() { return super.arranger(); }
    
    override Widget[] children() { return m_children; }

    Widget sibling_after(Widget child) 
    in{ 
        assert(m_children.contains(child));
    }
    body{
        auto idx = m_children.find_item(child);
        if (idx==NOT_FOUND || idx==m_children.length-1) return null;
        return m_children[++idx];
    }

    Widget sibling_before(Widget child) 
    in{ 
        assert(m_children.contains(child));
    }
    body{
        auto idx = m_children.find_item(child);
        if (idx==NOT_FOUND || idx==0) return null;
        return m_children[--idx];
    }

    /** Find the gui item at the specified point.
        The point p is assumed to be in the widget coordinates for this item.
        In widget coordinates (rect.x,rect.y) is the upper left corner of this widget.
    */
    Widget find_item_at_point(Point pos) 
    {
        alias pos p;
        if (!m_rect.contains(p.x,p.y)) return null;

        Widget mom = this;
        Widget[] kids = m_children;
        while(kids)
        {
            Widget found=null;
            p.x -= mom.m_rect.x;
            p.y -= mom.m_rect.y;
            foreach(child; kids) 
            {
                if (child.m_rect.contains(p.x,p.y))
                {
                    found = child;
                    break;
                }
            }
            if (!found) {
                return mom;
            }
            mom = found;
            kids = mom.children();
        }
        return mom;
    }


    //--------------- FOCUS HANDLING -------------------------
    
    /* Recursively find the focus item searching down from the top*/
    protected override Widget _find_focus_rdown()
    {
        Panel focusParent = null;
        foreach(c; m_children) {
            if (c.m_focused) {
                return c;
            }
            Panel p = cast(Panel)c;
            if (p && p.m_childFocused) {
                focusParent = p;
            }
        }
        if (!focusParent) return null;
        return focusParent._find_focus_rdown();
    }
    protected override void _set_child_focused_rup() {
        // recursively set child focus flag on all parents
        m_childFocused = true;
        if (parent) parent._set_child_focused_rup();
    }

    protected override void _unfocus_rup() {
        // recursively unfocus this item and all parents
        m_childFocused = false;
        super._unfocus_rup();
    }
    protected override Widget _find_first_focusable_rdown() {
        // depth-first search for first child that can be focused
        Widget w = null;

        foreach(c; m_children) {
            w = c._find_first_focusable_rdown();
            if (w) {
                return w;
            }
        }
        return null;
    }
    protected override Widget _find_last_focusable_rdown() {
        // depth-first search for last child that can be focused
        Widget w = null;

        foreach_reverse(c; m_children) {
            w = c._find_last_focusable_rdown();
            if (w) {
                return w;
            }
        }
        return null;
    }
    protected override Widget _find_next_focusable_rdown(inout bool found_focus)
    {
        // Basically we want to pretend we're doing a depth first traversal
        // to find the focus item, and keep going after we find it till we get 
        // to the next focusable item
        assert(get_focus() != null);

        if (found_focus && focusable) {
            return this;
        }
        if (m_focused) {
            assert(!found_focus, "There can be only one!"); 
            found_focus=true;
        }

        // Recur 
        foreach(c; m_children) {
            if (found_focus && c.focusable)
                return c;
            Widget new_focus = c._find_next_focusable_rdown(found_focus);
            if (new_focus) return new_focus;
        }
        return null;
    }
    protected override Widget _find_prev_focusable_rdown(inout bool found_focus)
    {
        // Basically we want to pretend we're doing a depth first traversal
        // to find the focus item, and keep going after we find it till we get 
        // to the next focusable item
        assert(get_focus() != null);

        // Recur 
        foreach_reverse(c; m_children) {
            if (found_focus && c.focusable)
                return c;
            Widget new_focus = c._find_prev_focusable_rdown(found_focus);
            if (new_focus) return new_focus;
        }

        if (found_focus && focusable) {
            return this;
        }
        if (m_focused) {
            assert(!found_focus, "There can be only one!"); 
            found_focus=true;
        }
        return null;
    }

    override int _focus_sanity_check(int focus_count=0)
    {
        // Currently this just checks that there are 0 or 1 focused items in the tree. 

        // TODO: the other invarient of focus is that if their *is* a focus item, then 
        // every ancestor of that item has the m_childFocused flag set, and NO other 
        // items than those should have m_childFocused set.
        int count = focus_count;
        foreach(c; m_children) {
            count += c._focus_sanity_check();
        }
        assert(count==0 || count==1, "Error: more than one focus item found!");
        return count;
    }

/+
    /** Search recursively from this point down for the current focus item */
    Widget get_focus() 
    {
        if (!m_focused) { return null; }
        Widget focus=null;
        foreach(c; m_children) {
            if (c.m_focused) {
                focus = c; break; 
            }
        }
        if (!focus) return this;
        return focus.get_focus();
    }

    /** Focus the next item in the tab chain.  
        The focus order is a depth first pre-order traversal.
        In other words focus order goes like
                  focus_self():
                  foreach(child in children)  
                      focus_child()
        However most container items are not focusable and so are skipped.
    */        
    void focus_next() {
        writefln("Focus Next!");
        Widget w = get_focus();
        Panel p = cast(Panel)w;
        if (p) { 
            // If current focus is a panel, next step is to go to first focusable child.
            Widget nextFocus = none;
            foreach(c;  p.m_children) {
                if (c.focusable()) {
                    nextFocus = c;
                    break;
                }
            }
            if (nextFocus) { 
                
            }
            
        }
    }

    /** Focus the previous item in the tab chain.
        The focus order is a depth first pre-order traversal.
    */        
    void focus_prev() {
        Widget w = get_focus();
    }

    /** Search immediate children for the one which is focused */
    override Widget get_focus_child()
    in{ 
        int cnt=0;
        foreach(x; m_children) { if (x.m_focused) cnt++; }
        assert(cnt<=1, "Widget has multiple children focused");
    }
    body{
        foreach(w; m_children) { 
            if (w.m_focused) return w;
        }
        return null;
    }

    /** Set focus of immediate children to the item specified.
     Also unsets focus on any child that currently has the focus. */
    override void set_focus_child(Widget item)
    in{
        assert(!item || m_children.contains(item)); 
        int cnt=0;
        foreach(x; m_children) { if (x.m_focused) cnt++; }
        assert(cnt<=1, "Widget has multiple children focused");
    }
    body
    {
        int ifocus = NOT_FOUND;
        if (item) {
            if (item.m_focused) return; // ok, already set
            ifocus = m_children.find_item(item);
            assert(ifocus!=NOT_FOUND);
            if (ifocus==NOT_FOUND)
                return;
        }
        //auto pred = delegate bool(Widget w){ return w.m_focused; };
        int iunfocus = m_children.find( delegate bool(Widget w){ return w.m_focused; } );
        if (iunfocus != NOT_FOUND) {
            m_children[iunfocus]._internal_set_focus(false);
        }
        if (ifocus != NOT_FOUND) {
            m_children[iunfocus]._internal_set_focus(true);
        }
    }

    /** Recursively search for the deepest item in this widget tree with the focus */
    Widget find_focus() {
        Widget focus = null;
        Widget item = this;
        while(item && item.m_focused) {
            focus = item;
            item = item.find_focus_child();
        }
        
        return null;
    }
+/

private:
    Widget[] m_children;
    bool m_needArrange = false;
    bool m_childFocused = false;
}

/** Represents a GUI area laid on top of a user's OpenGL window.  The overlay always
    stretches to cover the full extent of the actual GL window, though actual 
    layout engine and children determine how much of that area is actually used.
    Note: This usage of the term overlay should not be confused with the hardware
    overlays supported by some OpenGL implementations 
*/
class Overlay : Panel
{
    mixin WidgetMixin;
    mixin PanelMixin;

    /** Constructor.  Not every toolkit supports multiple windows (e.g. GLFW,SDL).
        If yours doesn't then you may not need the parameter.
    */
    this(WindowHandle win = null) {
        super();
        hostwin = win;
        Size sz = Luigi().get_window_size(win);
        set_rect(Rect(0,0,sz.w,sz.h));
        Luigi().add_overlay(this);
    }

    /** Sets up the drawing state for 2D GUI drawing and draws it's children. */
    override void draw() {
        if (!shown) return;
        Luigi().theme.begin_drawing(rect);
        super.draw();
        Luigi().theme.end_drawing();
    }

    /** Return the input adapter in current use.  Equivalent to Luigi().adapter. */
    InputAdapter adapter() {
        return Luigi().adapter;
    }

protected:
    override bool _set_mouse_grabber(Widget w)
    {
        if (m_grabItem && m_grabItem != w) {
            // warning("grab mouse: previous widget's grab was not released.");
        } 
        m_grabItem = w;
        return true;
    }
    override bool _release_mouse_grabber(Widget w)
    {
        if (m_grabItem == w) {
            m_grabItem = null;
            return true;
        }
        // warning("release grab: the widget was not grabbing the mouse."); 
        return false;
    }
    override Widget _get_mouse_grabber()
    {
        return m_grabItem;
    }

    // Top-level event interface callbacks
    final void on_sys_key(KeyEvent ev) {
        // find the innermost focus item and send it the key event
        Widget focus = get_focus();
        if (focus) {
            Luigi().theme.on_key(focus,ev);
        } else {
            ev.alive = true;
        }
        if ((!focus || ev.alive) && ev.is_press) 
        {
            // if the key wasn't consumed, check for tab/shift-tab
            // (and also sys hotkeys? Ctrl-X etc)

            if (ev.key==Key.Escape && focus && ev.alive) {
                // Lose gui focus
                set_focus(null);
                ev.alive = false;
            }
            else if (ev.key==Key.Tab)
            {
                if (ev.mods == KeyMods.None) {
                    focus_next();
                    ev.alive = false;
                }
                else if (ev.mods == KeyMods.Shift) {
                    focus_prev();
                    ev.alive = false;
                }
            }
        }
        else if (!focus) {
            ev.alive = true;
        }
    }
    final void on_sys_mouse_button(MouseButtonEvent ev)
    {
        Widget target;
        if (m_grabItem) {
            target = m_grabItem;
        }
        else {
            target = find_item_at_point(Point(ev.winx,ev.winy));
        }
        if (target) {
            // try to give it focus.
            target.focus();
            _xform_event_to_widget(target, ev);
            Luigi().theme.on_mouse_button(target, ev);
        }
        else {
            ev.alive = true;
        }
    }
    final void on_sys_mouse_move(MouseMoveEvent ev)
    {
        Widget target;
        target = find_item_at_point(Point(ev.winx,ev.winy));
        if (target !is m_hoverItem) {
            _EnterEvent eev;
            if (m_hoverItem) 
                m_hoverItem.on_leave(&eev);
            if (target)
                target.on_enter(&eev);
            m_hoverItem = target;
        }
        if (m_grabItem) {
            target = m_grabItem;
        }
        if (target) {
            _xform_event_to_widget(target, ev);
            Luigi().theme.on_mouse_move(target, ev);
        }
    }
    final void on_sys_mouse_wheel(MouseWheelEvent ev)
    {
        Widget target;
        if (m_grabItem) {
            target = m_grabItem;
        }
        else {
            target = find_item_at_point(Point(ev.winx,ev.winy));
        }
        if (target) {
            _xform_event_to_widget(target, ev);
            Luigi().theme.on_mouse_wheel(target, ev);
        }
        return 0;
    }
    final void on_sys_window_close(WindowCloseEvent ev)
    {
    }
    final void on_sys_window_size(WindowSizeEvent ev) 
    {
        //writefln("Size changed! : %f, %f", ev.w, ev.h);
        set_rect(Rect(0,0,ev.w, ev.h));
    }

    void _xform_event_to_widget(EvT)(Widget w, inout EvT ev) {
        Point p = Point(ev.x,ev.y);
        w.transform_window_to_widget(p);
        ev.x = p.x;
        ev.y = p.y;
    }

    

private:
    WindowHandle hostwin = null;

    Widget m_grabItem;  // item grabbing mouse. Could be any descendant
    Widget m_hoverItem; // item containing mouse pointer currently
}

/** A Panel with a button at the top to open or close it */
class Rollout : Panel
{
    mixin WidgetMixin;
    mixin PanelMixin;

    this() {
        super();
    }
}

/** A simple static text label */
class Label : Widget
{
    mixin WidgetMixin;

    this(char[] label_)
    {
        super();
        label=label_;
        m_focusable = false;
    }

    char [] label;
}

interface Valuator
{
    /** Set the current value as a double */
    double value(double);
    /** Get the current value as a double */
    double value();
}

/** A classic Button widget, with optional toggle button behavior. */
class Button : Widget, Valuator
{
    mixin WidgetMixin;

    override void on_key(KeyEvent ev)
    {
        if (ev.is_press && !ev.is_char) {
            if (ev.key == Key.Space ||
                ev.key == Key.Enter ||
                ev.key == Key.KP_Enter) 
            {
                clicked.emit(this);
                if (m_isToggle) { checked = !checked; }
                return;
            }
        }
        ev.alive = true;
    }
    
    override void on_mouse_button(MouseButtonEvent ev)
    {
        if (disabled) { ev.alive=true; return; }

        alias MouseButtons b;
        if (ev.is_press && ev.button == b.Left) {
            grab_mouse();
            m_depressed = true;
        }
        else if (ev.is_release && ev.button == b.Left) {
            if (is_grabbing_mouse()) {release_mouse();}
            if (m_depressed) clicked.emit(this);
            if (m_depressed && m_isToggle) checked = !checked;
            m_depressed = false;
        }
    }

    override void on_mouse_move(MouseMoveEvent ev)
    {
        if (disabled) { ev.alive=true; return; }

        if (is_grabbing_mouse())
        {
            m_depressed = rect.contains(ev.p);
            return;
        }
        ev.alive = true;
    }

    this(char[] label_) {
        super();
        label=label_;
        clicked = new ClickedSignal;
        value_changed = new ValueChangedSignal;
    }

    bool depressed() { return m_depressed; }

    override double value(double v) { 
        checked = (v!=0);
        return m_checked;
    }
    override double value() { return m_checked; }

    bool checked(bool v) { 
        if (v==m_checked) { return m_checked; }
        m_checked = v;
        value_changed.emit(this, m_checked);
        return m_checked;
    }
    bool checked() { return m_checked; }

    bool is_toggle(bool v) { return m_isToggle = v; }
    bool is_toggle() { return m_isToggle; }

    char[] label;

    alias FlexSignal!(Widget) ClickedSignal;
    alias FlexSignal!(Widget, bool) ValueChangedSignal;
    ClickedSignal clicked;
    ValueChangedSignal value_changed;
    //mixin Signal!(Widget) clicked;

protected:
    bool m_depressed = false;
    bool m_isToggle = false;
    bool m_checked = false;
}

/** A checkbox is just a toggle button with an alternate theme-defined look. */
class Checkbox : Button
{
    mixin WidgetMixin;
    this(char[] label_, bool checked_ = false) {
        super(label_);
        is_toggle = true;
        checked = checked_;
    }
}

/// An alternate spelling for Checkbox, because I can never remember which it is.
alias Checkbox CheckBox;

class TextField : Widget
{
    mixin WidgetMixin;

    override void on_key(KeyEvent ev)
    {
        if (ev.is_press) {
            if (ev.is_char && (isprint(ev.ch) || isUniAlpha(ev.ch)))
            {
                replace_selection([ev.ch]);
            }
            else if (ev.is_key)
            {
                switch(ev.key) {
                case Key.Tab:
                case Key.Escape:
                    // pass tab & escape through
                    ev.alive = true;
                    return;
                case 'A':
                    if (ev.ctrl_down) {
                        select_range(0,text.length);
                        sel_active = 1;
                    }
                    break;
                case Key.Left:
                    if (ev.shift_down) {
                        int[2] sel=[sel_begin, sel_end];
                        sel[sel_active]-=1;
                        //_maybe_swap_sel(sel);
                        select_range(sel[0], sel[1]);
                    }
                    else {
                        select_range(sel_range[sel_active]-1);
                    }
                    break;
                case Key.Right:
                    if (ev.shift_down) {
                        int[2] sel=[sel_begin, sel_end];
                        sel[sel_active]+=1;
                        //_maybe_swap_sel(sel);
                        select_range(sel[0], sel[1]);
                    }
                    else {
                        select_range(sel_range[sel_active]+1);
                    }
                    break;
                case Key.Delete:
                    if (selection_length) {
                        delete_selection();
                    }
                    else if (m_text.length) {
                        m_text.drop(cast(size_t)sel_begin);
                    }
                    break;
                case Key.Backspace:
                    if (selection_length) {
                        delete_selection();
                    }
                    else if (m_text.length && sel_begin != 0) {
                        m_text.drop(cast(size_t)sel_begin-1);
                        sel_begin--;
                        sel_end--;
                    }
                    break;
                case Key.Home:
                    if (ev.shift_down) {
                        int[2] sel=sel_range[];
                        sel[sel_active] = 0;
                        //_maybe_swap_sel(sel);
                        select_range(sel[0],sel[1]);
                    }
                    else {
                        sel_begin = sel_end = 0;
                    }
                    break;
                case Key.End:
                    if (ev.shift_down) {
                        int[2] sel=sel_range[];
                        sel[sel_active] = m_text.length;
                        //_maybe_swap_sel(sel);
                        select_range(sel[0],sel[1]);
                    } else {
                        sel_begin = sel_end = m_text.length;
                    }
                    break;
                case Key.Enter:
                case Key.KP_Enter:
                    text_accepted.emit(this, m_text);
                    break;
                default:
                    ;
                }
            }
        }
    }
    
    override void on_mouse_button(MouseButtonEvent ev)
    {
        if (disabled) { ev.alive = true;  return; }

        alias MouseButtons b;
        if (ev.is_press && ev.button == b.Left) {
            grab_mouse();
        }
        else if (ev.is_release && ev.button == b.Left) {
            if (is_grabbing_mouse()) {release_mouse();}
        }
    }

    override void on_mouse_move(MouseMoveEvent ev)
    {
        if (disabled) { ev.alive = true; return; }

        if (is_grabbing_mouse())
        {
            return;
        }
        ev.alive = true;
    }

    void select_range(int begin, int after_end=-1) {
        if (after_end<0) {
            after_end = begin;
        }
        if (begin < 0)     { begin = 0; }
        if (after_end < 0) { after_end = 0; }
        if (begin > m_text.length) { begin = m_text.length; }
        if (after_end > m_text.length) { after_end = m_text.length; }
        if (after_end<begin) {
            int tmp = begin; begin = after_end; after_end = tmp;
            sel_active = !sel_active;
        }
        sel_begin = begin;
        sel_end = after_end;

        // launch selection changed notificaiton
    }
    int selection_start(int begin)   { return sel_begin = begin; }
    int selection_end(int after_end) { return sel_end = after_end; }
    int selection_start()            { return sel_begin; }
    int selection_end()              { return sel_end; }
    int selection_length()           { return sel_end-sel_begin; }

    void select_all() { sel_begin=0; sel_end=m_text.length; }

    /** Delete the current selection.
     */
    void delete_selection() {
        m_text.drop_range(sel_begin,sel_end);
        sel_end = sel_begin;
    }
    /** Replace the current selection with the text, unselect and move insertion
     *    point to the end of the inserted text.
     */
    void replace_selection(char[] txt) {
        if (selection_length) {
            m_text = string.replaceSlice(m_text,m_text[sel_begin..sel_end], txt);
        } else {
            m_text = string.insert(m_text,sel_begin,txt);
        }
        sel_end = sel_begin = sel_begin + txt.length;
    }


    char[] text(char[] t) { 
        if (m_text is t || (m_text && t && m_text == t)) return m_text;
        m_text = t;
        text_accepted.emit(this, m_text);
        return m_text;
    }
    char[] text() { return m_text; }


    this(char[] text_="")
    {
        super();
        rect.width = 130;
        text_accepted = new TextAcceptedSignal;
        m_text=text_;
    }

    alias FlexSignal!(Widget, char[]) TextAcceptedSignal;
    TextAcceptedSignal text_accepted;
    // mixin Signal!(Widget) text_accepted;

    // This is for use by the theme
    int xscroll_offset = 0;

    //int substring_start  = 0;
    //int substring_end  = 0;
    union {
        struct {
            int sel_begin = 0;
            int sel_end = 0;
        }
        int[2] sel_range;
    }
    int sel_active = 0; // 0 or 1

    invariant {
        assert(sel_begin >= 0 && sel_begin <= m_text.length);
        assert(sel_end >= 0 && sel_end <= m_text.length);
        assert(sel_begin <= sel_end);
        assert(sel_active == 0 || sel_active == 1);
    }
 protected:
    char [] m_text;
}

/// An alternate spelling for TextField, because I can never remember which it is.
alias TextField Textfield;

/**
 * Slider represents a floating point value.  Since it uses a double for 
 * internal representation of the value it can also easily handle integral
 * numbers as large (larger even) than an int.
 *
 * The model used for tracking the value is that users can
 * programmatically set any value they wish using value() and it will
 * be honored.  However manipulations of the value through the UI are
 * restricted to integer multiples of the precision.  Zero is always
 * attainable via the UI as long as it is between max and min.
 */
class Slider : Widget, Valuator
{
    mixin WidgetMixin;

    override double value(double v) {
        if (v<m_min) v=m_min;
        if (v>m_max) v=m_max;
        if (v==m_value) return m_value;

        // want to have (m_value = prec * i) for some integer i
        real prec = precision;
        double i = lrint(v/prec);
        v = prec * i;
        if (v==m_value) return m_value;

        m_value = v;
        value_changed.emit(this,v);
        return v;
    }
    override double value() { return m_value; }

    this() {
        super();
        value_changed = new ValueChangedSignal;
        m_focusable = true;
    }

    override void on_key(KeyEvent ev) {
        double get_inc()
        {
            if (ev.ctrl_down) return m_precision;
            if (ev.shift_down) return m_page;
            return m_increment;
        }
        if (ev.is_press && !ev.is_char) {
            switch(ev.key) 
            {
            case Key.Right:
            case Key.Up:
                value = value + get_inc(); break;
            case Key.Left:
            case Key.Down:
                value = value - get_inc(); break;
            case Key.PageUp:
                value = value + m_page; break;
            case Key.PageDown:
                value = value - m_page; break;
            default:
                super.on_key(ev);// pass
            }
        }
    }

    double value_max(double v) { return m_max=v; }
    double value_min(double v) { return m_min=v; }
    double increment(double v) { return m_increment=v; }
    double page_increment(double v) { return m_page=v; }
    double precision(double prec) { return m_precision=prec; }

    double value_max() { return m_max; }
    double value_min() { return m_min; }
    double increment() { return m_increment; }
    double page_increment() { return m_page; }
    double precision() { return m_precision; }

    bool horizontal(bool tf) { 
        m_orientation = (tf ? Orientation.Horizontal : Orientation.Vertical);
        return m_orientation == Orientation.Horizontal; 
    }
    bool vertical(bool tf) {
        m_orientation = (tf ? Orientation.Vertical : Orientation.Horizontal);
        return m_orientation == Orientation.Vertical; 
    }
    bool horizontal() { return m_orientation == Orientation.Horizontal; }
    bool vertical() { return m_orientation != Orientation.Horizontal; }

    alias FlexSignal!(Widget, double) ValueChangedSignal;
    ValueChangedSignal value_changed;
    // mixin Signal!(Widget,double) value_changed;

    // Themes need to access this, hence it's public
    // Maybe a new "save_value()/restore_value()" api
    // so this doesn't need to be public.
    double m_saveValue = 0;

private:
    enum Orientation : ubyte {
        Vertical,
        Horizontal
    }

    double m_value  = 0;
    double m_max    = 100;
    double m_min    = 0;
    double m_precision = 1.; // minimal increment allowed
    double m_increment = 1.; // increment for arrow keys
    double m_page      = 10.;// increment for pgup/down
    Orientation m_orientation = Orientation.Horizontal;
}

class Spinner : Widget, Valuator
{
    mixin WidgetMixin;

    this(char[] label_) {
        super();
        label = label_;
    }

    override double value(double v) { return m_value=v; }
    override double value() { return m_value; }

    char[] label;
private:
    double m_value = 0;
}

class ScrollBar : Panel, Valuator
{
    mixin WidgetMixin;
    // don't use panel mixin because we shouldn't be able to add things to this panel

    class ScrollButton : Button
    {
        // use labels: 'up' 'down' 'left' 'right' to indicate which direction to point
        this(char[] label_) {
            super(label_);
            focusable = false;
        }
    }
    class ScrollTrack : Slider
    {
        this() { 
            super(); 
            focusable = false;
        }
        
    }

    this() {
        super();
        m_track = new ScrollTrack;
        m_upButton = new ScrollButton("left");
        m_downButton = new ScrollButton("right");
        // value_changed = new ValueChangedSignal;
        m_focusable = false;
        add(m_upButton);
        add(m_track);
        add(m_downButton);
        _set_button_labels();
        writefln("%s(%s): trace!", __FILE__, __LINE__);
    }

    // Panel-related overrides

    // Don't allow adding children
    private override void add(Widget child) { super.add(child); }
    // Don't allow setting an arranger
    private override Arranger arranger(Arranger a) { return null; }
    private override Arranger arranger() { return null; }

    override Size minimum_size(Size bounds)   {
        Size sz = m_track.minimum_size(bounds);
        Size szup = m_upButton.minimum_size(bounds);
        Size szdn = m_downButton.minimum_size(bounds);
        if (vertical) {
            sz.width = max(sz.width, szup.width);
            sz.width = max(sz.width, szdn.width);
            sz.height +=  szup.height + szdn.height;
        }
        else {
            sz.height = max(sz.height, szup.height);
            sz.height = max(sz.height, szdn.height);
            sz.width +=  szup.width + szdn.width;
        }
        return sz;
    }
    override Size preferred_size(Size bounds) {
        Size sz = m_track.preferred_size(bounds);
        Size szup = m_upButton.preferred_size(bounds);
        Size szdn = m_downButton.preferred_size(bounds);
        if (vertical) {
            sz.width = max(sz.width, szup.width);
            sz.width = max(sz.width, szdn.width);
            sz.height +=  szup.height + szdn.height;
        }
        else {
            sz.height = max(sz.height, szup.height);
            sz.height = max(sz.height, szdn.height);
            sz.width +=  szup.width + szdn.width;
        }
        return sz;
    }
    override void arrange() {
        Rect urect = Rect(0,0,0,0);
        Rect drect = Rect(0,0,0,0);
        urect.size = m_upButton.preferred_size(rect.size);
        drect.size = m_downButton.preferred_size(rect.size);
        if (vertical) {
            m_upButton.set_rect(urect);
            drect.y = rect.h - drect.h;
            m_downButton.set_rect(drect);
            m_track.set_rect(
                Rect( urect.x, urect.y2, urect.w, rect.h-urect.h-drect.h));
        } else {
            m_downButton.set_rect(drect);
            urect.x = rect.x2 - urect.w;
            m_upButton.set_rect(urect);
            m_track.set_rect(
                Rect( drect.x2, drect.y, rect.w-urect.w-drect.w, drect.h));
        }
    }

    // Slider related overrides

    override double value(double v) { return m_track.value(v); }
    override double value() { return m_track.value(); }

    double value_max(double v) { return m_track.value_max(v); }
    double value_min(double v) { return m_track.value_min(v); }
    double increment(double v) { return m_track.increment(v); }
    double page_increment(double v) { return m_track.page_increment(v); }
    double precision(double prec) { return m_track.precision(prec); }

    double value_max() { return m_track.value_max(); }
    double value_min() { return m_track.value_min(); }
    double increment() { return m_track.increment(); }
    double page_increment() { return m_track.page_increment(); }
    double precision() { return m_track.precision(); }

    bool horizontal(bool tf) { 
        if (horizontal == tf) return tf;
        m_track.horizontal(tf);
        _set_button_labels();
        return tf;
    }
    bool vertical(bool tf) { 
        if (vertical == tf) return tf;
        m_track.vertical(tf);
        _set_button_labels();
        return tf;
    }
    bool horizontal() { return m_track.horizontal(); }
    bool vertical() { return m_track.vertical(); }
        
    private void _set_button_labels() {
        if (m_track.vertical) {
            m_downButton.label = "down";
            m_upButton.label = "up";
        }
        else {
            m_downButton.label = "left";
            m_upButton.label = "right";
        }
    }

    double page_size(double v) { return m_page_size = v; }
    double page_size() { return m_page_size; }


    alias m_track.ValueChangedSignal ValueChangedSignal;

    double m_page_size = 1;
    ScrollTrack  m_track;
    ScrollButton m_downButton; // (or left)
    ScrollButton m_upButton; // (or right)
}

/** A radio button is just a Button with an alternate appearance 
    determined by the theme.
 */
class RadioButton : Button
{
    mixin WidgetMixin;

    this(char[] label_) {
        super(label_);
        is_toggle = true;
    }
}

/** RadioGroup is a non-GUI class that implements exclusive selection 
    behavior using signals.
*/
class RadioGroup : Valuator
{
    override double value(double v) {
        checked_item = lrint(v);
        return checked_item;
    }
    override double value() { return m_checkedItem; }

    int checked_item(int v) {
        if (v == m_checkedItem) { return m_checkedItem; }
        m_checkedItem = v;
        selection_changed.emit(m_checkedItem);

        // Make sure the one we want is indeed selected
        Button[] currentSels = opIndex(m_checkedItem);
        foreach(s; currentSels) {
            s.value = 1;
        }

        return m_checkedItem;
    }
    int checked_item() { return m_checkedItem; }

    this() {
        selection_changed = new SelectionChangedSignal;
    }

    private class _ButtonUpdater {
        int id;
        Valuator button;
        void update(int v) {
            if (id == v) { button.value=1; }
            else { button.value=0; }
        }
    }

    /** Have button b join this group, using value val */
    void add(Button b, int val) {
        // We observe the button's value
        b.value_changed.connect( &_member_value_changed );
        // The button value should also observe us
        auto bup = new _ButtonUpdater;
        bup.id = val;
        bup.button = b;
        m_obj2id[b] = bup;
        selection_changed ~= &bup.update;
        // Leave this up to the user?
        b.is_toggle = true;
    }

    /**  Add button b to the group, generating an automatic id.
         If this is the first item, the 0 is used as the id.
         Otherwise the current max id plus one is used.
    */
    void add(Button b) {
        int v = length() ? max_val()+1 : 0;
        add(b,v);
    }

    /** Have all the buttons in the parameter list join this group.
     *  If all items are Buttons then call add(Button) to add to the group
     *  using default values.  If a button is followed by an integer, then 
     *  use that as it's id.  For example:
     *  ---------------
     *    Button b1,b2; Checkbox c1,c2; RadioButton r1,r2;
     *    ...
     *    rgroup1.multiadd(b1,b2); // adds with ids 0 and 1.
     *    rgroup2.multiadd(c1,5, c2,10); // adds with ids 5 and 10
     *    rgroup3.multiadd(r1,1, r2); // adds with ids 1 and 2.
     *  ---------------
     */
    void multiadd(ButtonOrInt ...)(ButtonOrInt buttons) {
        static if (buttons.length == 0) {}
        else static if( is (ButtonOrInt[0] : Button ) ) {
            static if( (buttons.length > 1) && is(ButtonOrInt[1] : int)) {
                add(buttons[0], buttons[1]);
                multiadd(buttons[2..$]);
            }
            else {
                add(buttons[0]);
                multiadd(buttons[1..$]);
            }
        }
        else {
            static assert(
                0, 
                string.format("RadioGroup.multiadd: bad type '%s'. Button expected",
                              typeid(ButtonOrInt[0])));
        }
    }
    // Simpler version -- just allows buttons
    //void multiadd(Button[] buttons ...) {
    //    foreach(btn; buttons) {
    //        add(btn);
    //   }
    //}


    /** Have button b leave this group */
    void remove(Button b) {
        assert(0, "Not implemented yet!");
        m_obj2id.remove(b);
    }

    /** Return the Button(s) assocated with the given group value. */
    Button[] opIndex(int v) {
        Button [] ret;
        foreach(Button b, _ButtonUpdater val; m_obj2id) {
            if (v==val.id) ret ~= b;
        }
        return ret;
    }

    /** Return the group value associated with the given button. 
     *  Returns int.min if the item is not found
     */
    int opIndex(Button b) {
        _ButtonUpdater *p = (b in m_obj2id);
        if (p) return p.id;
        return int.min;
    }

    /** Return the minimum value associated with any item in the group. 
     *  Returns: the minimum value or int.max if there are no items.
     */
    int min_val() {
        int minv = int.max;
        foreach(v; m_obj2id.values) {
            if (v.id < minv) minv = v.id;
        }
        return minv;
    }

    /** Return the maximum value associated with any item in the group. */
    int max_val() {
        int maxv = int.min;
        foreach(v; m_obj2id.values) {
            if (v.id > maxv) maxv = v.id;
        }
        return maxv;
    }

    void _member_value_changed(Widget w, bool bSet)
    {
        Button b = cast(Button)w;
        if (!b)
            return;
        int new_check_id = opIndex(b);
        if (new_check_id == int.min) {
            return;
        }
        if (bSet && m_checkedItem != new_check_id) {
            // This is a new selection, different from previous
            int oldcheck = m_checkedItem;
            checked_item = new_check_id;
            
            // Try to uncheck any old item
            if (oldcheck != int.min) {
                Button[] currentSels = opIndex(oldcheck);
                foreach(s; currentSels) {
                    s.value = 0;
                }
            }
        }
        else if (!bSet && m_checkedItem == new_check_id) {
            // Attempt to deselect current selection -- don't let it happen
            Button[] currentSels = opIndex(m_checkedItem);
            foreach(s; currentSels) {
                s.value = 1;
            }
        }
    }

    int length() { return m_obj2id.length; }

    alias FlexSignal!(int) SelectionChangedSignal;
    SelectionChangedSignal selection_changed;

private:
    int m_checkedItem = int.min;
    _ButtonUpdater[Button] m_obj2id;
}




/** A basic panel with a built-in FlowArranger */
class FlowPanel : Panel
{
    mixin WidgetMixin;
    mixin PanelMixin;

    this() {
        super();
        arranger = new FlowArranger;
    }
    this(Alignment flow_align) {
        super();
        arranger = new FlowArranger(flow_align);
    }
    this(Alignment flow_align, Gaps gaps) {
        super();
        arranger = new FlowArranger(flow_align, gaps);
    }
}

/** A basic panel with a built-in GridArranger */
class GridPanel : Panel
{
    mixin WidgetMixin;
    mixin PanelMixin;

    this() {
        super();
        arranger = new GridArranger;
    }
    this(int Cols, int Rows=0) {
        super();
        arranger = new GridArranger(Cols,Rows);
    }
    this(int Cols, int Rows, Gaps gaps) {
        super();
        arranger = new GridArranger(Cols, Rows, gaps);
    }
}

/** A basic panel with a built-in BorderArranger */
class BorderPanel : Panel
{
    mixin WidgetMixin;
    mixin PanelMixin;

    this() {
        super();
        arranger = new BorderArranger;
    }
    this(Gaps  gaps) {
        super();
        arranger = new BorderArranger(gaps);
    }
}



