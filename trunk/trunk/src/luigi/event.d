//---------------------------------------------------------------------
/*
 Copyright:

  luigi/event.d -- event basics for the 'Luigi' user
  interface library.

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

module luigi.event;

//import sslot.signal;
import luigi.signalobj;
import luigi.base;


/** WindowHandle is just an opaque piece of data used to identify
   windows on the underlying windowing system.  It could be an int for
   GLUT, or an HWND for Win32, etc.
*/
typedef void* WindowHandle;

enum KeyMods {
    None = 0,
    Ctrl  = 0x1, CtrlBit  = 1,
    Shift = 0x2, ShiftBit = 2,
    Alt   = 0x4, AltBit   = 3
};

char[] modsToString(KeyMods k) {
    alias KeyMods e;
    char [] ret;
    if (k & e.Shift) { ret ~= "Shift"; }
    if (k & e.Ctrl)  { if (ret) ret~="|"; ret ~= "Ctrl"; }
    if (k & e.Alt)   { if (ret) ret~="|"; ret ~= "Alt"; }
    if (!ret) ret = "None";
    return ret;
}

enum MouseButtons {
    None = 0,
    Left  = 0x1,    LeftBit = 1,
    Right = 0x2,    RightBit = 2,
    Middle = 0x4,   MiddleBit = 3,
    Button0 = Left,   Button0Bit = 1,
    Button1 = Right,  Button1Bit = 2,
    Button2 = Middle, Button2Bit = 3,
    Button3 = 0x8,    Button3Bit = 4,
    Button4 = 0x10,   Button4Bit = 5,
    Button5 = 0x20,   Button5Bit = 6
};

char[] buttonsToString(MouseButtons b) {
    alias MouseButtons e;
    char[] ret;
    if (b & e.Left)   { ret ~= "Left"; }
    if (b & e.Middle) { if (ret) ret~="|"; ret ~= "Middle"; }
    if (b & e.Right)  { if (ret) ret~="|"; ret ~= "Right"; } 
    if (b & e.Button3){ if (ret) ret~="|"; ret ~= "Button3"; }
    if (b & e.Button4) { if (ret) ret~="|"; ret ~= "Button4"; }
    if (b & e.Button5) { if (ret) ret~="|"; ret ~= "Button5"; }
    if (!ret) ret = "None";
    return ret;
}

enum Key {
    Unknown = -1,
    Space = 32,
    Special = 256,
    Escape,
    F1,
    F2,
    F3, 
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    Up,
    Down,
    Left,
    Right,
    LShift,  Shift = LShift,
    RShift,
    LCtrl,   Ctrl = LCtrl,
    RCtrl,
    LAlt,    Alt = LAlt,
    RAlt,
    LMeta,
    RMeta,
    LSuper,
    RSuper,
    Mode,
    Compose,
    Tab,
    Enter,
    Backspace,
    Insert,
    Delete,
    PageUp,
    PageDown,
    Home,
    End,
    KP0,
    KP1,
    KP2,
    KP3,
    KP4,
    KP5,
    KP6,
    KP7,
    KP8,
    KP9,
    KP_Divide,
    KP_Multiply,
    KP_Subtract,
    KP_Add,
    KP_Decimal,
    KP_Equals,
    KP_Enter,

    NumLock,
    CapsLock,
    ScrollLock,

    Help,
    Print,
    SysReq,
    Break,
    Menu,
    Power,
    Euro,
    Undo,
}



struct _KeyEvent
{
    union {
        Point p;
        struct { float x,y; }
    }
    union { 
        Point winp;
        struct { float winx,winy; }
    }
    int key; // an ascii char (uppercase) or enum Key
    dchar ch;
    KeyMods mods;
    bool shift_down() { return (mods & KeyMods.Shift)!=0; }
    bool ctrl_down() { return (mods & KeyMods.Ctrl)!=0; }
    bool alt_down() { return (mods & KeyMods.Alt)!=0; }
    bool is_press;
    bool is_release() { return !is_press; }
    bool is_char;
    bool is_key;

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _MouseButtonEvent
{
    union {
        Point p;
        struct { float x,y; }
    }
    union { 
        Point winp;
        struct { float winx,winy; }
    }
    bool is_press; // otherwise release
    bool is_release() { return !is_press; }
    MouseButtons button;
    bool is_left() { return (button & MouseButtons.Left)!=0; }
    bool is_right() { return (button & MouseButtons.Right)!=0; }
    bool is_middle() { return (button & MouseButtons.Middle)!=0; }
    MouseButtons pressed;
    bool left_down() { return (pressed & MouseButtons.Left)!=0; }
    bool right_down() { return (pressed & MouseButtons.Right)!=0; }
    bool middle_down() { return (pressed & MouseButtons.Middle)!=0; }
    KeyMods mods;
    bool shift_down() { return (mods & KeyMods.Shift)!=0; }
    bool ctrl_down() { return (mods & KeyMods.Ctrl)!=0; }
    bool alt_down() { return (mods & KeyMods.Alt)!=0; }

    bool is_left_press() { return (is_press && is_left); }
    bool is_right_press() { return (is_press && is_right); }
    bool is_middle_press() { return (is_press && is_middle); }
    bool is_left_release() { return (is_release && is_left); }
    bool is_right_release() { return (is_release && is_right); }
    bool is_middle_release() { return (is_release && is_middle); }

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _MouseMoveEvent
{
    union {
        Point p;
        struct { float x,y; }
    }
    union { 
        Point winp;
        struct { float winx,winy; }
    }
    MouseButtons pressed;
    bool left_down() { return (pressed & MouseButtons.Left)!=0; }
    bool right_down() { return (pressed & MouseButtons.Right)!=0; }
    bool middle_down() { return (pressed & MouseButtons.Middle)!=0; }
    KeyMods mods;
    bool shift_down() { return (mods & KeyMods.Shift)!=0; }
    bool ctrl_down() { return (mods & KeyMods.Ctrl)!=0; }
    bool alt_down() { return (mods & KeyMods.Alt)!=0; }

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _MouseWheelEvent
{
    union {
        Point p;
        struct { float x,y; }
    }
    union { 
        Point winp;
        struct { float winx,winy; }
    }
    int delta;
    MouseButtons pressed;
    bool left_down() { return (pressed & MouseButtons.Left)!=0; }
    bool right_down() { return (pressed & MouseButtons.Right)!=0; }
    bool middle_down() { return (pressed & MouseButtons.Middle)!=0; }
    KeyMods mods;
    bool shift_down() { return (mods & KeyMods.Shift)!=0; }
    bool ctrl_down() { return (mods & KeyMods.Ctrl)!=0; }
    bool alt_down() { return (mods & KeyMods.Alt)!=0; }

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _WindowSizeEvent
{
    WindowHandle id;
    float width,height;
    alias width w;
    alias height h;

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _WindowCloseEvent
{
    WindowHandle id;

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _FocusEvent
{
    Object previous; // would rather have Widget here

    bool is_alive = false;
    bool alive(bool tf) { return is_alive=tf; }
    bool alive() { return is_alive; }
}

struct _EnterEvent
{
    // is anything needed here?
}


alias _KeyEvent* KeyEvent;
alias _MouseButtonEvent* MouseButtonEvent;
alias _MouseMoveEvent* MouseMoveEvent;
alias _MouseWheelEvent* MouseWheelEvent;
alias _WindowSizeEvent* WindowSizeEvent;
alias _WindowCloseEvent* WindowCloseEvent;
alias _FocusEvent* FocusEvent;
alias _EnterEvent* EnterEvent;
alias _EnterEvent* LeaveEvent;

alias void function(KeyEvent)         KeyEventFn;
alias void function(MouseButtonEvent) MouseButtonEventFn;
alias void function(MouseMoveEvent)   MouseMoveEventFn;
alias void function(MouseWheelEvent)  MouseWheelEventFn;
alias void function(WindowSizeEvent)  WindowSizeEventFn;
alias void function(WindowCloseEvent) WindowCloseEventFn;

alias void delegate(KeyEvent)         KeyEventDg;
alias void delegate(MouseButtonEvent) MouseButtonEventDg;
alias void delegate(MouseMoveEvent)   MouseMoveEventDg;
alias void delegate(MouseWheelEvent)  MouseWheelEventDg;
alias void delegate(WindowSizeEvent)  WindowSizeEventDg;
alias void delegate(WindowCloseEvent) WindowCloseEventDg;

alias FlexSignal!(KeyEvent)         KeyEventSignal;
alias FlexSignal!(MouseButtonEvent) MouseButtonEventSignal;
alias FlexSignal!(MouseMoveEvent)   MouseMoveEventSignal;
alias FlexSignal!(MouseWheelEvent)  MouseWheelEventSignal;
alias FlexSignal!(WindowSizeEvent)  WindowSizeEventSignal;
alias FlexSignal!(WindowCloseEvent) WindowCloseEventSignal;

