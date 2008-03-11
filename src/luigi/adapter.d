//---------------------------------------------------------------------
/*
 Copyright:

  luigi/adapter/base.d -- input adaptor basics for the 'Luigi' user
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
module luigi.adapter;

//import sslot.signal;
import luigi.signalobj;
import luigi.base;
import luigi.event;


/** The InputAdapter interface allows the GUI to connect to various windowing toolkits.
    Luigi wants to be able to get input from whatever source possible, be it GLFW, GLUT, SDL,
    native Win32, GTK, wxWidgets, or whatever.  Implementations of the input adapter
    allow Luigi and applications to pretend that all toolkits work the same way, regardless
    of the back-end chosen.  In particular Luigi puts a facade of a Signals-and-Slots type 
    of delivery mechanism on top of whatever the underlying mechanism is.

    One common way simple toolkits like GLUT deliver input to applications is via callbacks.
    They usually allow one, and only one, function to be called.
    A concrete InputAdapter for one of these will override all callback methods and emit
    signals identifying all the events.

    To implement a new adapter, derive from the InputAdapter interface, and mixin the 
    InputAdapterMix.  This sets up the external interface and the signals.
 */
interface InputAdapter
{
    /// Get the size of the specified window, using an opaque
    /// window handle.
    Size get_window_size(WindowHandle h);


    /**
     * These are standard user-level callbacks.  
     * These get dispatched to your code only for
     * events that weren't handled by any widget
     * on the GUI overlay.
     */
    void addKeyCallback(KeyEventFn cb)                 ;
    void addMouseButtonCallback(MouseButtonEventFn cb) ;
    void addMouseMoveCallback(MouseMoveEventFn cb)     ;
    void addMouseWheelCallback(MouseWheelEventFn cb)   ;
    void addWindowSizeCallback(WindowSizeEventFn cb)   ;
    void addWindowCloseCallback(WindowCloseEventFn cb) ;

    // And again for delegates...
    void addKeyCallback(KeyEventDg cb)                 ;
    void addMouseButtonCallback(MouseButtonEventDg cb) ;
    void addMouseMoveCallback(MouseMoveEventDg cb)     ;
    void addMouseWheelCallback(MouseWheelEventDg cb)   ;
    void addWindowSizeCallback(WindowSizeEventDg cb)   ;
    void addWindowCloseCallback(WindowCloseEventDg cb) ;


    // remove raw system callback
    void removeKeyCallback(KeyEventFn cb)                 ;
    void removeMouseButtonCallback(MouseButtonEventFn cb) ;
    void removeMouseMoveCallback(MouseMoveEventFn cb)     ;
    void removeMouseWheelCallback(MouseWheelEventFn cb)   ;
    void removeWindowSizeCallback(WindowSizeEventFn cb)   ;
    void removeWindowCloseCallback(WindowCloseEventFn cb) ;

    // And again for delegates...
    void removeKeyCallback(KeyEventDg cb)                 ;
    void removeMouseButtonCallback(MouseButtonEventDg cb) ;
    void removeMouseMoveCallback(MouseMoveEventDg cb)     ;
    void removeMouseWheelCallback(MouseWheelEventDg cb)   ;
    void removeWindowSizeCallback(WindowSizeEventDg cb)   ;
    void removeWindowCloseCallback(WindowCloseEventDg cb) ;

    /**
     * These are "raw" system callbacks in that they haven't yet been
     * translated or dispatched to widgets.  This is all the input
     * that comes in at the window level.  Luigi uses these to get
     * input which is then dispatched to widgets based on current
     * focus item, location of the mouse, etc.  See event.d for the
     * definitons of the function types.
     */
    void addSysKeyCallback(KeyEventFn cb)                 ;
    void addSysMouseButtonCallback(MouseButtonEventFn cb) ;

    // And again for delegates...
    void addSysKeyCallback(KeyEventDg cb)                 ;
    void addSysMouseButtonCallback(MouseButtonEventDg cb) ;

    // remove raw system callback
    void removeSysKeyCallback(KeyEventFn cb)                 ;
    void removeSysMouseButtonCallback(MouseButtonEventFn cb) ;

    // And again for delegates...
    void removeSysKeyCallback(KeyEventDg cb)                 ;
    void removeSysMouseButtonCallback(MouseButtonEventDg cb) ;

    // addRaw Paint
    // addRaw Idle

    // pollInput?
    // swapBuffers?

    // Other candidates: Create window, Move/Resize window, Set window title
    // maybe need a separate WindowSys adapter?
    // I don't want to be in the business of wrapping the OS, or becoming another GLUT.
    // Just the minimal things needed to run an on-screen UI.  Let them use GLUT for 
    // what GLUT does as much as possible.  I really just need the inputs, and to know when my
    // window changes size, and perhaps goes away.
}

template InputAdapterMix()
{
    //-------------------------------------------------------
    // Use 'sig.*.emit' to emit signals from your InputAdapter implementation.
    // Use example:  
    //       KeyEvent ev;
    //       [...build KeyEvent from native data...]
    //       sig.key.emit(ev);

    // You MUST call this from your Adapter's constructor!
    void initInputAdapter() {
        with (_lsig) {
            key = new KeyEventSignal;
            mouseButton = new MouseButtonEventSignal;
            mouseMove = new MouseMoveEventSignal;  
            mouseWheel = new MouseWheelEventSignal; 
            windowSize = new WindowSizeEventSignal; 
            windowClose = new WindowCloseEventSignal;

            sysKey = new KeyEventSignal;
            sysMouseButton = new MouseButtonEventSignal;
        }
    }

    //-------------------------------------------------------
    // The rest shouldn't concernt InputAdapter implementors
    private struct _LuigiSignals {
        KeyEventSignal         key;
        MouseButtonEventSignal mouseButton;
        MouseMoveEventSignal   mouseMove;  
        MouseWheelEventSignal  mouseWheel; 
        WindowSizeEventSignal  windowSize; 
        WindowCloseEventSignal windowClose;

        KeyEventSignal         sysKey;
        MouseButtonEventSignal sysMouseButton;
    }
    private _LuigiSignals _lsig;
    alias _lsig sig;

    // Implementation of InputAdapter
    void addKeyCallback(KeyEventFn cb)                 {_lsig.key.connect(cb);}
    void addMouseButtonCallback(MouseButtonEventFn cb) {_lsig.mouseButton.connect(cb);}
    void addMouseMoveCallback(MouseMoveEventFn cb)     {_lsig.mouseMove.connect(cb);}
    void addMouseWheelCallback(MouseWheelEventFn cb)   {_lsig.mouseWheel.connect(cb);}
    void addWindowSizeCallback(WindowSizeEventFn cb)   {_lsig.windowSize.connect(cb);}
    void addWindowCloseCallback(WindowCloseEventFn cb) {_lsig.windowClose.connect(cb);} 

    // And again for delegates...
    void addKeyCallback(KeyEventDg cb)                 {_lsig.key.connect(cb);}         
    void addMouseButtonCallback(MouseButtonEventDg cb) {_lsig.mouseButton.connect(cb);} 
    void addMouseMoveCallback(MouseMoveEventDg cb)     {_lsig.mouseMove.connect(cb);}   
    void addMouseWheelCallback(MouseWheelEventDg cb)   {_lsig.mouseWheel.connect(cb);}  
    void addWindowSizeCallback(WindowSizeEventDg cb)   {_lsig.windowSize.connect(cb);}  
    void addWindowCloseCallback(WindowCloseEventDg cb) {_lsig.windowClose.connect(cb);} 

    // Implementation of InputAdapter
    void removeKeyCallback(KeyEventFn cb)                 {_lsig.key.disconnect(cb);}
    void removeMouseButtonCallback(MouseButtonEventFn cb) {_lsig.mouseButton.disconnect(cb);}
    void removeMouseMoveCallback(MouseMoveEventFn cb)     {_lsig.mouseMove.disconnect(cb);}
    void removeMouseWheelCallback(MouseWheelEventFn cb)   {_lsig.mouseWheel.disconnect(cb);}
    void removeWindowSizeCallback(WindowSizeEventFn cb)   {_lsig.windowSize.disconnect(cb);}
    void removeWindowCloseCallback(WindowCloseEventFn cb) {_lsig.windowClose.disconnect(cb);} 

    // And again for delegates...
    void removeKeyCallback(KeyEventDg cb)                 {_lsig.key.disconnect(cb);}         
    void removeMouseButtonCallback(MouseButtonEventDg cb) {_lsig.mouseButton.disconnect(cb);} 
    void removeMouseMoveCallback(MouseMoveEventDg cb)     {_lsig.mouseMove.disconnect(cb);}   
    void removeMouseWheelCallback(MouseWheelEventDg cb)   {_lsig.mouseWheel.disconnect(cb);}  
    void removeWindowSizeCallback(WindowSizeEventDg cb)   {_lsig.windowSize.disconnect(cb);}  
    void removeWindowCloseCallback(WindowCloseEventDg cb) {_lsig.windowClose.disconnect(cb);} 


    // Implementation of InputAdapter
    void addSysKeyCallback(KeyEventFn cb)                 {_lsig.sysKey.connect(cb);}
    void addSysMouseButtonCallback(MouseButtonEventFn cb) {_lsig.sysMouseButton.connect(cb);}

    // And again for delegates...
    void addSysKeyCallback(KeyEventDg cb)                 {_lsig.sysKey.connect(cb);}
    void addSysMouseButtonCallback(MouseButtonEventDg cb) {_lsig.sysMouseButton.connect(cb);}

    // remove raw system callback
    void removeSysKeyCallback(KeyEventFn cb)                 {_lsig.sysKey.disconnect(cb);}
    void removeSysMouseButtonCallback(MouseButtonEventFn cb) {_lsig.sysMouseButton.disconnect(cb);};

    // And again for delegates...
    void removeSysKeyCallback(KeyEventDg cb)                 {_lsig.sysKey.disconnect(cb);}
    void removeSysMouseButtonCallback(MouseButtonEventDg cb) {_lsig.sysMouseButton.disconnect(cb);};



}



/** Gather input from multiple places */
/*
class CompoundInputAdapter : InputAdapter
{
    InputAdapter[] adapters;
}
*/

