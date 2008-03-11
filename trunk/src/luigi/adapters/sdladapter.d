//---------------------------------------------------------------------
/*
 Copyright:

  luigi/adapter/glfw.d
     -- GLFW input adapter for the 'Luigi' user interface library.

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
module luigi.adapters.sdladapter;

import luigi.base;
import luigi.event;
import luigi.adapter;
import derelict.sdl.sdl;
import luigi.signalobj;

import std.ctype;

import std.stdio : writefln;

int translate_key(int sdlkey)
{
    if (sdlkey<256) { 
        switch(sdlkey) {
        case SDLK_BACKSPACE: return Key.Backspace;
        case SDLK_TAB: return Key.Tab;
        case SDLK_RETURN: return Key.Enter;
        case SDLK_ESCAPE: return Key.Escape;
        case SDLK_SPACE: return Key.Space;
        case SDLK_DELETE: return Key.Delete;
/*
  // these are all just regular ASCII
        case SDLK_CLEAR: return Key.Clear;
        case SDLK_PAUSE: return Key.Pause;
        case SDLK_EXCLAIM: return Key.;
        case SDLK_QUOTEDBL: return Key.;
        case SDLK_HASH: return Key.;
        case SDLK_DOLLAR: return Key.;
        case SDLK_AMPERSAND: return Key.;
        case SDLK_QUOTE: return Key.;
        case SDLK_LEFTPAREN: return Key.;
        case SDLK_RIGHTPAREN: return Key.;
        case SDLK_ASTERISK: return Key.;
        case SDLK_PLUS: return Key.;
        case SDLK_COMMA: return Key.;
        case SDLK_MINUS: return Key.;
        case SDLK_PERIOD: return Key.;
        case SDLK_SLASH: return Key.;
        case SDLK_0: return Key.;
        case SDLK_1: return Key.;
        case SDLK_2: return Key.;
        case SDLK_3: return Key.;
        case SDLK_4: return Key.;
        case SDLK_5: return Key.;
        case SDLK_6: return Key.;
        case SDLK_7: return Key.;
        case SDLK_8: return Key.;
        case SDLK_9: return Key.;
        case SDLK_COLON: return Key.;
        case SDLK_SEMICOLON: return Key.;
        case SDLK_LESS: return Key.;
        case SDLK_EQUALS: return Key.;
        case SDLK_GREATER: return Key.;
        case SDLK_QUESTION: return Key.;
        case SDLK_AT: return Key.;
        case SDLK_LEFTBRACKET: return Key.;
        case SDLK_BACKSLASH: return Key.;
        case SDLK_RIGHTBRACKET: return Key.;
        case SDLK_CARET: return Key.;
        case SDLK_UNDERSCORE: return Key.;
        case SDLK_BACKQUOTE: return Key.;
*/
        default:
            return sdlkey; // must be ascii?
        }
    }

    switch(sdlkey) {
    case SDLK_KP0: return Key.KP0;
    case SDLK_KP1: return Key.KP1;
    case SDLK_KP2: return Key.KP2;
    case SDLK_KP3: return Key.KP3;
    case SDLK_KP4: return Key.KP4;
    case SDLK_KP5: return Key.KP5;
    case SDLK_KP6: return Key.KP6;
    case SDLK_KP7: return Key.KP7;
    case SDLK_KP8: return Key.KP8;
    case SDLK_KP9: return Key.KP9;
    case SDLK_KP_PERIOD: return Key.KP_Decimal;
    case SDLK_KP_DIVIDE: return Key.KP_Divide;
    case SDLK_KP_MULTIPLY: return Key.KP_Multiply;
    case SDLK_KP_MINUS: return Key.KP_Subtract;
    case SDLK_KP_PLUS: return Key.KP_Add;
    case SDLK_KP_ENTER: return Key.KP_Enter;
    case SDLK_KP_EQUALS: return Key.KP_Equals;

    /* Arrows + Home/End pad */
    case SDLK_UP: return Key.Up;
    case SDLK_DOWN: return Key.Down;
    case SDLK_RIGHT: return Key.Right;
    case SDLK_LEFT: return Key.Left;
    case SDLK_INSERT: return Key.Insert;
    case SDLK_HOME: return Key.Home;
    case SDLK_END: return Key.End;
    case SDLK_PAGEUP: return Key.PageUp;
    case SDLK_PAGEDOWN: return Key.PageDown;

    /* Function keys */
    case SDLK_F1: return Key.F1;
    case SDLK_F2: return Key.F2;
    case SDLK_F3: return Key.F3;
    case SDLK_F4: return Key.F4;
    case SDLK_F5: return Key.F5;
    case SDLK_F6: return Key.F6;
    case SDLK_F7: return Key.F7;
    case SDLK_F8: return Key.F8;
    case SDLK_F9: return Key.F9;
    case SDLK_F10: return Key.F10;
    case SDLK_F11: return Key.F11;
    case SDLK_F12: return Key.F12;
    case SDLK_F13: return Key.F13;
    case SDLK_F14: return Key.F14;
    case SDLK_F15: return Key.F15;

    /* Key state modifier keys */
    case SDLK_NUMLOCK: return Key.NumLock;
    case SDLK_CAPSLOCK: return Key.CapsLock;
    case SDLK_SCROLLOCK: return Key.ScrollLock;
    case SDLK_RSHIFT: return Key.RShift;
    case SDLK_LSHIFT: return Key.LShift;
    case SDLK_RCTRL: return Key.RCtrl;
    case SDLK_LCTRL: return Key.LCtrl;
    case SDLK_RALT: return Key.RAlt;
    case SDLK_LALT: return Key.LAlt;
    case SDLK_RMETA: return Key.RMeta;
    case SDLK_LMETA: return Key.LMeta;
    case SDLK_RSUPER: return Key.RSuper;
    case SDLK_LSUPER: return Key.LSuper;
    case SDLK_MODE: return Key.Mode;
    case SDLK_COMPOSE: return Key.Compose;

    /* Miscellaneous function keys */
    case SDLK_HELP: return Key.Help;
    case SDLK_PRINT: return Key.Print;
    case SDLK_SYSREQ: return Key.SysReq;
    case SDLK_BREAK: return Key.Break;
    case SDLK_MENU: return Key.Menu;
    case SDLK_POWER: return Key.Power;
    case SDLK_EURO: return Key.Euro;
    case SDLK_UNDO: return Key.Undo;

    default:
        return Key.Unknown;
    }
}
dchar translate_char(int sdlkey, KeyMods mods)
{
    if (mods & (KeyMods.Alt|KeyMods.Ctrl)) return 0;

    // Space tends to print as space regardless of shift key status
    if (isspace(sdlkey)) return sdlkey; 

    if (mods & KeyMods.Shift) {
        if (islower(sdlkey)) {
            return toupper(sdlkey);
        }
        // this part is keyboard specific.  This is only correct for a US keyboard.
        switch(sdlkey) {
        case '1': return '!';
        case '2': return '@';
        case '3': return '#';
        case '4': return '$';
        case '5': return '%';
        case '6': return '^';
        case '7': return '&';
        case '8': return '*';
        case '9': return '(';
        case '0': return ')';
        case '-': return '_';
        case '=': return '+';
        case '\\': return '|';
        case '[': return '{';
        case ']': return '}';
        case '`': return '~';
        case ',': return '<';
        case '.': return '>';
        case '/': return '?';
        case '\'': return '"';
        case ';': return ':';
        default:
            // shift+something that we don't understand
            if (isprint(sdlkey)) return sdlkey;
            else return 0;
        }
    }
    else {
        // non-shift key
        switch(sdlkey) {
        case SDLK_KP0: return '0';
        case SDLK_KP1: return '1';
        case SDLK_KP2: return '2';
        case SDLK_KP3: return '3';
        case SDLK_KP4: return '4';
        case SDLK_KP5: return '5';
        case SDLK_KP6: return '6';
        case SDLK_KP7: return '7';
        case SDLK_KP8: return '8';
        case SDLK_KP9: return '9';
        case SDLK_KP_PERIOD: return '.';
        case SDLK_KP_DIVIDE: return '/';
        case SDLK_KP_MULTIPLY: return '*';
        case SDLK_KP_MINUS: return '-';
        case SDLK_KP_PLUS: return '+';
        case SDLK_KP_EQUALS: return '=';
        default:
            if (isprint(sdlkey)) return sdlkey;
        }
    }        
    // must not be a printable char
    return 0;
}


class SDLAdapter : InputAdapter
{
    /// Singleton access via static opCall SDLAdapter()
    static SDLAdapter opCall() {
        static SDLAdapter myinstance = null;
        if (!myinstance) {
            myinstance = new SDLAdapter;
            myinstance.init();
        }
        return myinstance;
    }
    /// Singleton access via 'inst' property
    static SDLAdapter inst() {
        return SDLAdapter();
    }

    /** Return the size of the SDL window.
     *  Params: an SDL_Surface* cast to a WindowHandle
     */
    Size get_window_size(WindowHandle win) {
        // only one window in SDL to query the size of
        SDL_Surface *surf = cast(SDL_Surface*)win;
        if (surf is null) {
            throw new GUIException(
                "Null is not a valid WindowHandle for SDLAdapter.  \n"
                "Overlay needs the window's SDL_Surface pointer: \n"
                "  e.g. gui = new Overlay( cast(WindowHandle)main_surface );");
        }
        return Size(surf.w,surf.h);
    }

    /** Call this to give Luigi access to SDL events coming into your app's
     *  main event loop. Returns whether the event was sucessfully dispatched 
     *  or not.
     *  So the pattern is:
     *  ---------------
     *  while ( SDL_PollEvent(&event) ) {
     *      if(dispatchEvent(event)) 
     *          continue;
     *      // otherwise handle the event
     *      switch(event.type) {
     *          ...
     *      }
     *  ---------------
     */
    bool dispatchEvent(inout SDL_Event event) 
    {
        const bool EVENT_CONSUMED = true;
        const bool EVENT_ALIVE = false;

        switch(event.type) {
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            return !keyCallback(event.key);
        case SDL_MOUSEMOTION:
            mouseMotionCallback(event.motion);
            return EVENT_CONSUMED;

        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
            return !mouseButtonCallback(event.button);

        case SDL_VIDEORESIZE:
            windowSizeCallback(event.resize);
            return EVENT_ALIVE;

        case SDL_QUIT:
            windowCloseCallback(event.quit);
            return EVENT_ALIVE;

        // We don't do anything with these events currently
        case SDL_VIDEOEXPOSE:
        case SDL_SYSWMEVENT:
        case SDL_USEREVENT:
        case SDL_ACTIVEEVENT:
        case SDL_JOYAXISMOTION:
        case SDL_JOYHATMOTION:
        case SDL_JOYBALLMOTION:
        case SDL_JOYBUTTONDOWN:
        case SDL_JOYBUTTONUP:
        default:
            return EVENT_ALIVE;
        }
        return EVENT_ALIVE;
    }

private:

    MouseButtons _getButtons(Uint8 sdlbtn) {
        alias MouseButtons M;
        MouseButtons m = M.None;
        if (sdlbtn & (1<<0)) m|=M.Left;
        if (sdlbtn & (1<<1)) m|=M.Middle;
        if (sdlbtn & (1<<2)) m|=M.Right;
        return m;
    }
    MouseButtons _getButtons() {
        return _getButtons(SDL_GetMouseState(null,null));
    }
    KeyMods _getMods(SDLMod sdlmod) {
        alias KeyMods M;
        KeyMods m = M.None;
        if (sdlmod & KMOD_CTRL)  m|= M.Ctrl;
        if (sdlmod & KMOD_SHIFT) m|= M.Shift;
        if (sdlmod & KMOD_ALT)   m|= M.Alt;
        return m;
    }
    KeyMods _getMods() {
        return _getMods(SDL_GetModState());
    }

    // SDL Event Handlers 
    bool keyCallback(inout SDL_KeyboardEvent sdl)
    {
        _KeyEvent ev;
        int k = translate_key(sdl.keysym.sym);
        dchar ch;
        if (SDL_EnableUNICODE(-1)) {
            ch = sdl.keysym.unicode;
        }
        else {
            ch = translate_char(sdl.keysym.sym, ev.mods);
        }
        void _initEvent() {
            ev.key = k;
            ev.ch = ch;
            ev.mods = inst._getMods();
            ev.is_press  = (sdl.type==SDL_KEYDOWN);
            ev.is_key = true;
            ev.is_char = (ev.ch != 0);
        }
        _initEvent();
        inst.sig.sysKey.emit(&ev);
        if (ev.alive) {
            _initEvent();
            inst.sig.key.emit(&ev);
        }
        return ev.alive;
    }
    bool mouseButtonCallback(inout SDL_MouseButtonEvent sdl)
    {
        if (sdl.button == SDL_BUTTON_WHEELUP || sdl.button == SDL_BUTTON_WHEELDOWN)
        {
            return mouseWheelCallback(sdl);
        }
        _MouseButtonEvent ev; 
        void _initEvent() {
            ev.x = ev.winx = sdl.x;
            ev.y = ev.winy = sdl.y;
            ev.is_press  = (sdl.type==SDL_MOUSEBUTTONDOWN);
            ev.mods = inst._getMods();
            ev.pressed = inst._getButtons();
            if      (sdl.button == SDL_BUTTON_LEFT) ev.button = MouseButtons.Left;
            else if (sdl.button == SDL_BUTTON_RIGHT) ev.button = MouseButtons.Right;
            else if (sdl.button == SDL_BUTTON_MIDDLE) ev.button = MouseButtons.Middle;
        }
        _initEvent();
        inst.sig.sysMouseButton.emit(&ev);
        if (ev.alive) {
            _initEvent();
            inst.sig.mouseButton.emit(&ev);
        }
        return ev.alive;
    }
    bool mouseWheelCallback(inout SDL_MouseButtonEvent sdl)
    {
        if (sdl.type == SDL_MOUSEBUTTONUP) return true; // ignore up of down/up pair
        _MouseWheelEvent ev;
        ev.x = ev.winx = sdl.x;
        ev.y = ev.winy = sdl.y;
        const int inc = 1; // is this right?
        if (sdl.button == SDL_BUTTON_WHEELUP)
            ev.delta = inc;
        else 
            ev.delta = -inc;
        ev.pressed = inst._getButtons();
        ev.mods = inst._getMods();

        inst.sig.mouseWheel.emit(&ev);
        return ev.alive;
    }
    bool mouseMotionCallback(inout SDL_MouseMotionEvent sdl)
    {
        _MouseMoveEvent ev;
        ev.x = ev.winx = sdl.x;
        ev.y = ev.winy = sdl.y;
        ev.pressed = inst._getButtons(sdl.state);
        ev.mods = inst._getMods();

        inst.sig.mouseMove.emit(&ev);
        return ev.alive;
    }
    void windowSizeCallback(inout SDL_ResizeEvent sdl)
    {
        writefln("Resize!");
        _WindowSizeEvent ev;
        ev.w = sdl.w;
        ev.h = sdl.h;
        inst.sig.windowSize.emit(&ev);
    }
    bool  windowCloseCallback(inout SDL_QuitEvent sdl)
    {
        _WindowCloseEvent ev;
        inst.sig.windowClose.emit(&ev);
        return ev.alive;
    }
    void windowRefreshCallback()
    {
        //_WindowRefreshEvent ev;
    }

    // Singleton, so constructor is private
    this() {
        initInputAdapter(); // from mixin
    }

    void init() {
    }

    ~this() {
    }

public:
    mixin InputAdapterMix;

}


alias SDLAdapter DefaultAdapter;
