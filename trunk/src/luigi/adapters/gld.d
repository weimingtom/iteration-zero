//---------------------------------------------------------------------
/*
  luigi/adapter/gld.d
     -- GLD input adapter for the 'Luigi' user interface library.
     GLD is a port of GLFW to D.
     It can be obtained from http://dsource.org/projects/schooner/

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
module luigi.adapters.gld;

import luigi.base;
import luigi.event;
import luigi.adapter;
private import gld;
import luigi.signalobj;

import std.stdio : writefln;

int translate_key(int gldkey)
{
    if (gldkey<Key.Special) { return gldkey; }
    switch(gldkey) {
    case GLD_KEY_SPACE: return Key.Space;
    case GLD_KEY_ESC: return Key.Escape;
    case GLD_KEY_F1: return Key.F1;
    case GLD_KEY_F2: return Key.F2;
    case GLD_KEY_F3: return Key.F3;
    case GLD_KEY_F4: return Key.F4;
    case GLD_KEY_F5: return Key.F5;
    case GLD_KEY_F6: return Key.F6;
    case GLD_KEY_F7: return Key.F7;
    case GLD_KEY_F8: return Key.F8;
    case GLD_KEY_F9: return Key.F9;
    case GLD_KEY_F10: return Key.F10;
    case GLD_KEY_F11: return Key.F11;
    case GLD_KEY_F12: return Key.F12;
    case GLD_KEY_F13: return Key.F13;
    case GLD_KEY_F14: return Key.F14;
    case GLD_KEY_F15: return Key.F15;
    case GLD_KEY_F16: return Key.F16;
    case GLD_KEY_F17: return Key.F17;
    case GLD_KEY_F18: return Key.F18;
    case GLD_KEY_F19: return Key.F19;
    case GLD_KEY_F20: return Key.F20;
    case GLD_KEY_F21: return Key.F21;
    case GLD_KEY_F22: return Key.F22;
    case GLD_KEY_F23: return Key.F23;
    case GLD_KEY_F24: return Key.F24;
    case GLD_KEY_F25: return Key.F25;
    case GLD_KEY_UP: return Key.Up;
    case GLD_KEY_DOWN: return Key.Down;
    case GLD_KEY_LEFT: return Key.Left;
    case GLD_KEY_RIGHT: return Key.Right;
    case GLD_KEY_LSHIFT: return Key.LShift;
    case GLD_KEY_RSHIFT: return Key.RShift;
    case GLD_KEY_LCTRL: return Key.LCtrl;
    case GLD_KEY_RCTRL: return Key.        RCtrl;
    case GLD_KEY_LALT: return Key.LAlt;
    case GLD_KEY_RALT: return Key.RAlt;
    case GLD_KEY_TAB: return Key.Tab;
    case GLD_KEY_ENTER: return Key.Enter;
    case GLD_KEY_BACKSPACE: return Key.Backspace;
    case GLD_KEY_INSERT: return Key.Insert;
    case GLD_KEY_DEL: return Key.Delete;
    case GLD_KEY_PAGEUP: return Key.PageUp;
    case GLD_KEY_PAGEDOWN: return Key.PageDown;
    case GLD_KEY_HOME: return Key.Home;
    case GLD_KEY_END: return Key.End;
    case GLD_KEY_KP_0: return Key.KP0;
    case GLD_KEY_KP_1: return Key.KP1;
    case GLD_KEY_KP_2: return Key.KP2;
    case GLD_KEY_KP_3: return Key.KP3;
    case GLD_KEY_KP_4: return Key.KP4;
    case GLD_KEY_KP_5: return Key.KP5;
    case GLD_KEY_KP_6: return Key.KP6;
    case GLD_KEY_KP_7: return Key.KP7;
    case GLD_KEY_KP_8: return Key.KP8;
    case GLD_KEY_KP_9: return Key.KP9;
    case GLD_KEY_KP_DIVIDE: return Key.KP_Divide;
    case GLD_KEY_KP_MULTIPLY: return Key.KP_Multiply;
    case GLD_KEY_KP_SUBTRACT: return Key.KP_Subtract;
    case GLD_KEY_KP_ADD: return Key.KP_Add;
    case GLD_KEY_KP_DECIMAL: return Key.KP_Decimal;
    case GLD_KEY_KP_EQUAL: return Key.KP_Equals;
    case GLD_KEY_KP_ENTER: return Key.KP_Enter;
    default:
        return Key.Unknown;
    }
}


class GLDAdapter : InputAdapter
{
    // Singleton access via static opCall GLDAdapter()
    static GLDAdapter opCall() {
        static GLDAdapter myinstance = null;
        if (!myinstance) {
            myinstance = new GLDAdapter;
            myinstance.init();
        }
        return myinstance;
    }
    // Singleton access via 'inst' property
    static GLDAdapter inst() {
        return GLDAdapter();
    }

    Size get_window_size(WindowHandle win) {
        // only one window in GLD to query the size of
        int w,h;
        gldGetWindowSize(&w, &h);
        return Size(w,h);
    }


private:
    int m_lastWheelPos = 0;
    static int m_lastKey = Key.Unknown;

    MouseButtons _getButtons() {
        alias MouseButtons M;
        MouseButtons m = M.None;
        if (gldGetMouseButton(GLD_MOUSE_BUTTON_LEFT)) m|=M.Left;
        if (gldGetMouseButton(GLD_MOUSE_BUTTON_RIGHT)) m|=M.Right;
        if (gldGetMouseButton(GLD_MOUSE_BUTTON_MIDDLE)) m|=M.Middle;
        return m;
    }
    KeyMods _getMods() {
        alias KeyMods M;
        KeyMods m = M.None;
        if (gldGetKey(GLD_KEY_LCTRL) || gldGetKey(GLD_KEY_RCTRL))   m|= M.Ctrl;
        if (gldGetKey(GLD_KEY_LSHIFT) || gldGetKey(GLD_KEY_RSHIFT)) m|= M.Shift;
        if (gldGetKey(GLD_KEY_LALT) || gldGetKey(GLD_KEY_RALT))     m|= M.Alt;
        return m;
    }

    // All of our GLD callbacks
    static void keyCallbackC(int k, int action)
    {
        _KeyEvent ev;
        void _initEvent() {
            ev.key = translate_key(k);
            m_lastKey = ev.key;
            ev.ch = 0;
            ev.is_press  = (action==GLD_PRESS);
            ev.mods = inst._getMods();
            ev.is_key = true;
            ev.is_char = false;
        }
        _initEvent();
        inst.sig.sysKey.emit(&ev);
        if (ev.alive) {
            _initEvent();
            inst.sig.key.emit(&ev);
        }
    }
    static void charCallbackC(int ch, int action)
    {
        _KeyEvent ev;
        // In practice it seems that char callbacks always come _after_
        // key callbacks. (That's probably the case for any sane operating system).
        // Also, you only get one char for one key.  This isn't 
        // necessarily the case in general with fancy input methods 
        // (e.g. european and asian) but it seems to be the case with GLD.
        // Not every key has a char, but every char has a key in GLD
        void _initEvent() {
            ev.key = m_lastKey;
            ev.ch = ch;
            ev.is_press  = (action==GLD_PRESS);
            ev.mods = inst._getMods();
            ev.is_key = false;
            ev.is_char = true;
        }
        _initEvent();
        inst.sig.sysKey.emit(&ev);
        if (ev.alive) {
            _initEvent();
            inst.sig.key.emit(&ev);
        }
    }
    static void mouseButtonCallbackC(int button, int action)
    {
        _MouseButtonEvent ev; 
        int x,y;
        gldGetMousePos(&x, &y);
        void _initEvent() {
            ev.x = ev.winx = x;
            ev.y = ev.winy = y;
            ev.is_press  = (action==GLD_PRESS);
            ev.mods = inst._getMods();
            ev.pressed = inst._getButtons();
            if      (button == GLD_MOUSE_BUTTON_LEFT) ev.button = MouseButtons.Left;
            else if (button == GLD_MOUSE_BUTTON_RIGHT) ev.button = MouseButtons.Right;
            else if (button == GLD_MOUSE_BUTTON_MIDDLE) ev.button = MouseButtons.Middle;
        }        
        _initEvent();
        inst.sig.sysMouseButton.emit(&ev);
        if (ev.alive) {
            // reinit because GUI might much with it
            _initEvent();
            inst.sig.mouseButton.emit(&ev);
        }
    }
    static void mousePosCallbackC(int x, int y)
    {
        _MouseMoveEvent ev;
        ev.x = ev.winx = x;
        ev.y = ev.winy = y;
        ev.pressed = inst._getButtons();
        ev.mods = inst._getMods();

        inst.sig.mouseMove.emit(&ev);
    }
    static void mouseWheelCallbackC(int pos)
    {
        _MouseWheelEvent ev;
        int x,y;
        gldGetMousePos(&x, &y);
        ev.x = ev.winx = x;
        ev.y = ev.winy = y;
        ev.delta = pos - inst.m_lastWheelPos;
        ev.pressed = inst._getButtons();
        ev.mods = inst._getMods();
        inst.m_lastWheelPos = pos;

        inst.sig.mouseWheel.emit(&ev);
    }
    static void windowSizeCallbackC(int width, int height)
    {
        _WindowSizeEvent ev;
        ev.w = width;
        ev.h = height;
        inst.sig.windowSize.emit(&ev);
    }
    static int  windowCloseCallbackC()
    {
        _WindowCloseEvent ev;
        inst.sig.windowClose.emit(&ev);
        return true;
    }
    static void windowRefreshCallbackC()
    {
        //WindowRefreshEvent ev;
    }

    void setCallbacks() {
        // Note: a window has to be open for these to work!
        gldSetKeyCallback( &keyCallbackC );
        gldSetCharCallback( &charCallbackC );
        gldSetMouseButtonCallback( &mouseButtonCallbackC );
        gldSetMousePosCallback( &mousePosCallbackC );
        gldSetMouseWheelCallback( &mouseWheelCallbackC );
        gldSetWindowSizeCallback( &windowSizeCallbackC );
        gldSetWindowCloseCallback( &windowCloseCallbackC );
        gldSetWindowRefreshCallback( &windowRefreshCallbackC );
    }
    void clearCallbacks() {
        gldSetKeyCallback(null);
        gldSetCharCallback(null);
        gldSetMouseButtonCallback(null);
        gldSetMousePosCallback(null);
        gldSetMouseWheelCallback(null);
        gldSetWindowSizeCallback(null);
        gldSetWindowCloseCallback(null);
        gldSetWindowRefreshCallback(null);
    }

    // Singleton, so constructor is private
    this() {
        initInputAdapter();
    }
    void init() {
        // These can't be in the constructor because they trigger callbacks
        // and the callbacks want to call inst().
        // gldInit();
        setCallbacks();
    }
    ~this() {
        clearCallbacks();
    }

public:
    mixin InputAdapterMix;

}


alias GLDAdapter DefaultAdapter;
