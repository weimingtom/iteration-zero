/*      _______   __   __   __   ______   __   __   _______   __   __
 *     / _____/\ / /\ / /\ / /\ / ____/\ / /\ / /\ / ___  /\ /  |\/ /\
 *    / /\____\// / // / // / // /\___\// /_// / // /\_/ / // , |/ / /
 *   / / /__   / / // / // / // / /    / ___  / // ___  / // /| ' / /
 *  / /_// /\ / /_// / // / // /_/_   / / // / // /\_/ / // / |  / /
 * /______/ //______/ //_/ //_____/\ /_/ //_/ //_/ //_/ //_/ /|_/ /
 * \______\/ \______\/ \_\/ \_____\/ \_\/ \_\/ \_\/ \_\/ \_\/ \_\/
 *
 * Copyright (c) 2004 - 2008 Olof Naess�n and Per Larsson
 *
 *
 * Per Larsson a.k.a finalman
 * Olof Naess�n a.k.a jansem/yakslem
 *
 * Visit: http://guichan.sourceforge.net
 *
 * License: (BSD)
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of Guichan nor the names of its contributors may
 *    be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

module guichan.gui;

import std.stdio;

import guichan.event;
import guichan.listener;
import guichan.mouseinput;
import guichan.keyinput;
import guichan.key;
import guichan.widget;
import guichan.graphics;
import guichan.input;
// import guichan.focushandler;
import guichan.exception;
import guichan.rectangle;

import dlisp.bind;

private import guichan.util;

/**
  * @mainpage
  * @section Introduction
  * This documentation is mostly intended as a reference to the API. If you want to get started with Guichan, we suggest you check out the programs in the examples directory of the Guichan release.
  * @n
  * @n
  * This documentation is, and will always be, work in progress. If you find any errors, typos or inconsistencies, or if you feel something needs to be explained in more detail - don't hesitate to tell us.
  */

/**
  * Contains a Guichan GUI. This is the core class of Guichan to which
  * implementations of back ends are passed, to make Guichan work with
  * a specific library, and to where the top widget (root widget of GUI)
  * is added. If you want to be able to have more then one widget in your 
  * GUI, the top widget should be a container.
  *
  * A Gui object cannot work properly without passing back end 
  * implementations to it. A Gui object must have an implementation of a
  * Graphics and an implementation of Input. 
  *
  * NOTE: A complete GUI also must have the ability to load images.
  *       Images are loaded with the Image class, so to make Guichan
  *       able to load images an implementation of ImageLoader must be
  *       passed to Image.
  *
  * @see Graphics, Input, Image
  */

class Gui
{
public:

    mixin BindClass!("Gui");

    /**
      * Constructor.
      */
    this()
    {
      mTop = null;
      mGraphics = null;
      mInput = null;
      mTabbing = true;
      mShiftPressed = false;
      mMetaPressed = false;
      mControlPressed = false;
      mAltPressed = false;
      mLastMousePressButton = 0;
      mLastMousePressTimeStamp = 0;
      mLastMouseX = 0;
      mLastMouseY = 0;
      mClickCount = 1;
      mLastMouseDragButton = 0;
      mFocusHandler = new FocusHandler;
    }


    /**
      * Destructor.
      */
    ~this()
    {
        setTop(null);
        delete mFocusHandler;
    }

    /**
      * Sets the top widget. The top widget is the root widget
      * of the GUI. If you want a GUI to be able to contain more
      * than one widget the top widget should be a container.
      *
      * @param top The top widget.
      * @see Container
      * @since 0.1.0
      */
    void setTop(Widget top)
    {
      if (mTop !is null)
      {
          mTop._setFocusHandler(null);
      }

      if (top !is null)
      {
          top._setFocusHandler(mFocusHandler);
      }
      mTop = top;
    }

    /**
      * Gets the top widget. The top widget is the root widget
      * of the GUI.
      *
      * @return The top widget. null if no top widget has been set.
      * @since 0.1.0
      */
    Widget getTop()
    {
      return mTop;
    }

    /**
      * Sets the graphics object to use for drawing.
      *
      * @param graphics The graphics object to use for drawing.
      * @see getGraphics, AllegroGraphics, HGEGraphics, 
      *      OpenLayerGraphics, OpenGLGraphics, SDLGraphics
      * @since 0.1.0
      */
    void setGraphics(Graphics graphics)
    {
        mGraphics = graphics;
    }

    /**
      * Gets the graphics object used for drawing.
      *
      *  @return The graphics object used for drawing. null if no
      *          graphics object has been set.
      * @see setGraphics, AllegroGraphics, HGEGraphics, 
      *      OpenLayerGraphics, OpenGLGraphics, SDLGraphics
      * @since 0.1.0
      */
    Graphics getGraphics()
    {
      return mGraphics;
    }

    /**
      * Sets the input object to use for input handling.
      *
      * @param input The input object to use for input handling.
      * @see getInput, AllegroInput, HGEInput, OpenLayerInput,
      *      SDLInput
      * @since 0.1.0
      */
    void setInput(Input input)
    {
        mInput = input;
    }

    /**
      * Gets the input object being used for input handling.
      *
      *  @return The input object used for handling input. null if no
      *          input object has been set.
      * @see setInput, AllegroInput, HGEInput, OpenLayerInput,
      *      SDLInput
      * @since 0.1.0
      */
    Input getInput()
    {
      return mInput;
    }

    /**
      * Performs logic of the GUI. By calling this function all logic
      * functions down in the GUI heirarchy will be called. When logic
      * is called for Gui, user input will be handled.
      *
      * @see Widget.logic
      * @since 0.1.0
      */
    void logic()
    {
        if (mTop is null)
        {
            throw new GCN_Exception("No top widget set");
        }

        handleModalFocus();
        handleModalMouseInputFocus();

        if (mInput !is null)
        {
            mInput._pollInput();

            handleKeyInput();
            handleMouseInput();
        } // end if

        mTop.logic();
    }

    /**
      * Draws the GUI. By calling this funcion all draw functions
      * down in the GUI hierarchy will be called. When draw is called
      * the used Graphics object will be initialised and drawing of
      * the top widget will commence.
      *
      * @see Widget.draw
      * @since 0.1.0
      */
    void draw()
    {
        if (mTop is null)
        {
            throw new GCN_Exception("No top widget set");
        }
        if (mGraphics is null)
        {
            throw new GCN_Exception("No graphics set");
        }

        if (!mTop.isVisible())
        {
            return;
        }

        mGraphics._beginDraw();

        // If top has a frame,
        // draw it before drawing top
        if (mTop.getFrameSize() > 0)
        {
            Rectangle rec = mTop.getDimension();
            rec.x -= mTop.getFrameSize();
            rec.y -= mTop.getFrameSize();
            rec.width += 2 * mTop.getFrameSize();
            rec.height += 2 * mTop.getFrameSize();
            mGraphics.pushClipArea(rec);
            mTop.drawFrame(mGraphics);
            mGraphics.popClipArea();
        }

        mGraphics.pushClipArea(mTop.getDimension());
        mTop.draw(mGraphics);
        mGraphics.popClipArea();

        mGraphics._endDraw();
    }

    /**
      * Focuses none of the widgets in the Gui.
      *
      * @since 0.1.0
      */
    void focusNone()
    {
        mFocusHandler.focusNone;
    }

    /**
      * Sets tabbing enabled, or not. Tabbing is the usage of
      * changing focus by utilising the tab key.
      *
      * @param tabbing True if tabbing should be enabled, false
      *                otherwise.
      * @see isTabbingEnabled
      * @since 0.1.0
      */
    void setTabbingEnabled(bool tabbing)
    {
        mTabbing = tabbing;
    }

    /**
      * Checks if tabbing is enabled.
      *
      * @return True if tabbing is enabled, false otherwise.
      * @see setTabbingEnabled
      * @since 0.1.0
      */
    bool isTabbingEnabled()
    {
        return mTabbing;
    }

    /**
      * Adds a global key listener to the Gui. A global key listener
      * will receive all key events generated from the GUI and global
      * key listeners will receive the events before key listeners
      * of widgets.
      *
      * @param keyListener The key listener to add.
      * @see removeGlobalKeyListener
      * @since 0.5.0
      */
//     virtual void addGlobalKeyListener(KeyListener* keyListener);

    /**
      * Removes global key listener from the Gui.
      *
      * @param keyListener The key listener to remove.
      * @throws Exception if the key listener hasn't been added.
      * @see addGlobalKeyListener
      * @since 0.5.0
      */
//     virtual void removeGlobalKeyListener(KeyListener* keyListener);


    mixin BindMethods!(getTop,setTabbingEnabled,isTabbingEnabled,focusNone);

protected:
    /**
      * Handles all mouse input.
      *
      * @since 0.6.0
      */
    void handleMouseInput()
    {
        while (!mInput.isMouseQueueEmpty())
         {
             MouseInput mouseInput = mInput.dequeueMouseInput();

             // Save the current mouse state. It will be needed if modal focus
             // changes or modal mouse input focus changes.
             mLastMouseX = mouseInput.getX();
             mLastMouseY = mouseInput.getY();

             switch (mouseInput.getType())
             {
               case MouseInput.PRESSED:
                   handleMousePressed(mouseInput);
                   break;
               case MouseInput.RELEASED:
                   handleMouseReleased(mouseInput);
                   break;
               case MouseInput.MOVED:
                   handleMouseMoved(mouseInput);
                   break;
               case MouseInput.WHEEL_MOVED_DOWN:
                   handleMouseWheelMovedDown(mouseInput);
                   break;
               case MouseInput.WHEEL_MOVED_UP:
                   handleMouseWheelMovedUp(mouseInput);
                   break;
               default:
                   throw new GCN_Exception("Unknown mouse input type.");
                   break;
             }
         }
    }

    /**
      * Handles key input.
      *
      * @since 0.6.0
      */
    void handleKeyInput()
    {
        while (!mInput.isKeyQueueEmpty())
        {
            KeyInput keyInput = mInput.dequeueKeyInput();

            // Save modifiers state
            mShiftPressed = keyInput.isShiftPressed();
            mMetaPressed = keyInput.isMetaPressed();
            mControlPressed = keyInput.isControlPressed();
            mAltPressed = keyInput.isAltPressed();

//             KeyEvent keyEventToGlobalKeyListeners(null,
//                                                   mShiftPressed,
//                                                   mControlPressed,
//                                                   mAltPressed,
//                                                   mMetaPressed,
//                                                   keyInput.getType(),
//                                                   keyInput.isNumericPad(),
//                                                   keyInput.getKey());
// 
//             distributeKeyEventToGlobalKeyListeners(keyEventToGlobalKeyListeners);

            // If a global key listener consumes the event it will not be
            // sent further to the source of the event.
//             if (keyEventToGlobalKeyListeners.isConsumed())
//             {
//                 continue;
//             }

            bool keyEventConsumed = false;
            
            // Send key inputs to the focused widgets
            if (mFocusHandler.getFocused() !is null)
            {
                KeyEvent keyEvent = new KeyEvent(getKeyEventSource(),
                                  mShiftPressed,
                                  mControlPressed,
                                  mAltPressed,
                                  mMetaPressed,
                                  keyInput.getType(),
                                  keyInput.isNumericPad(),
                                  keyInput.getKey());
                

                if (!mFocusHandler.getFocused().isFocusable())
                {
                    mFocusHandler.focusNone();
                }
                else
                {                    
                    distributeKeyEvent(keyEvent);                    
                }

                keyEventConsumed = keyEvent.isConsumed();
            }

            // If the key event hasn't been consumed and
            // tabbing is enable check for tab press and
            // change focus.
            if (!keyEventConsumed
                && mTabbing
                && keyInput.getKey().getValue() == Key.TAB
                && keyInput.getType() == KeyInput.PRESSED)
            {
                if (keyInput.isShiftPressed())
                {
                    mFocusHandler.tabPrevious();
                }
                else
                {
                    mFocusHandler.tabNext();
                }
            }                           
                
        } // end while
    }

    /**
      * Handles mouse moved input.
      *
      * @param mouseInput The mouse input to handle.
      * @since 0.6.0
      */
    void handleMouseMoved(MouseInput mouseInput)
    {
        // Check if the mouse leaves the application window.
        if (mWidgetWithMouseQueue.length
            && (mouseInput.getX() < 0
                || mouseInput.getY() < 0
                || !mTop.getDimension().isPointInRect(mouseInput.getX(), mouseInput.getY()))
            )
        {
            // Distribute an event to all widgets in the "widget with mouse" queue.
            while (mWidgetWithMouseQueue.length)
            {
                Widget widget = mWidgetWithMouseQueue[0];

                if (Widget.widgetExists(widget))
                {
                    distributeMouseEvent(widget,
                                         MouseEvent.EXITED,
                                         cast(int)mouseInput.getButton(),
                                         mouseInput.getX(),
                                         mouseInput.getY(),
                                         true,
                                         true);
                }

                mWidgetWithMouseQueue = mWidgetWithMouseQueue[1 .. $] ;
            }

            return;
        }

        // Check if there is a need to send mouse exited events by
        // traversing the "widget with mouse" queue.
        bool widgetWithMouseQueueCheckDone = mWidgetWithMouseQueue.length == 0;
        while (!widgetWithMouseQueueCheckDone)
        {
            int iterations = 0;
            foreach (int i, Widget widget;  mWidgetWithMouseQueue)
            {
                // If a widget in the "widget with mouse queue" doesn't
                // exists anymore it should be removed from the queue.
                if (!Widget.widgetExists(widget))
                {
                    mWidgetWithMouseQueue.remove_index(i);
                    break;
                }
                else
                {
                    int x, y;
                    widget.getAbsolutePosition(x, y);

                    if (x > mouseInput.getX()
                        || y > mouseInput.getY()
                        || x + widget.getWidth() <= mouseInput.getX()
                        || y + widget.getHeight() <= mouseInput.getY()
                        || !widget.isVisible())
                    {
                        distributeMouseEvent(widget,
                                             MouseEvent.EXITED,
                                             mouseInput.getButton(),
                                             mouseInput.getX(),
                                             mouseInput.getY(),
                                             true,
                                             true);                                       
                        mClickCount = 1;
                        mLastMousePressTimeStamp = 0;
                        mWidgetWithMouseQueue.remove_index(i);
                        break;
                    }
                }

                iterations++;
            }

            widgetWithMouseQueueCheckDone = iterations == mWidgetWithMouseQueue.length;
        }

        // Check all widgets below the mouse to see if they are
        // present in the "widget with mouse" queue. If a widget
        // is not then it should be added and an entered event should
        // be sent to it.
        Widget parent = getMouseEventSource(mouseInput.getX(), mouseInput.getY());
        Widget widget = parent;

        // If a widget has modal mouse input focus then it will
        // always be returned from getMouseEventSource, but we only wan't to send
        // mouse entered events if the mouse has actually entered the widget with
        // modal mouse input focus, hence we need to check if that's the case. If
        // it's not we should simply ignore to send any mouse entered events.
        if (mFocusHandler.getModalMouseInputFocused() !is null
            && widget == mFocusHandler.getModalMouseInputFocused()
            && Widget.widgetExists(widget))
        {
            int x, y;
            widget.getAbsolutePosition(x, y);

            if (x > mouseInput.getX()
                || y > mouseInput.getY()
                || x + widget.getWidth() <= mouseInput.getX() 
                || y + widget.getHeight() <= mouseInput.getY())
            {
                parent = null;
            }
        }

        while (parent !is null)
        {
            parent = widget.getParent();

            // Check if the widget is present in the "widget with mouse" queue.
            bool widgetIsPresentInQueue = false;
            foreach(Widget w; mWidgetWithMouseQueue)
            {
                if (w is widget)
                {
                    widgetIsPresentInQueue = true;
                    break;
                }
            }

            // Widget is not present, send an entered event and add
            // it to the "widget with mouse" queue.
            if (!widgetIsPresentInQueue
                && Widget.widgetExists(widget))
            {
                distributeMouseEvent(widget,
                                     MouseEvent.ENTERED,
                                     mouseInput.getButton(),
                                     mouseInput.getX(),
                                     mouseInput.getY(),
                                     true,
                                     true);
                mWidgetWithMouseQueue = [widget] ~ mWidgetWithMouseQueue;
            }

            Widget swap = widget;
            widget = parent;
            parent = swap.getParent();
        }

        if (mFocusHandler.getDraggedWidget() !is null)
        {
            distributeMouseEvent(cast(Widget)mFocusHandler.getDraggedWidget(),
                                 MouseEvent.DRAGGED,
                                 mLastMouseDragButton,
                                 mouseInput.getX(),
                                 mouseInput.getY());
        }
        else
        {
            Widget sourceWidget = getMouseEventSource(mouseInput.getX(), mouseInput.getY());
            distributeMouseEvent(sourceWidget,
                                 MouseEvent.MOVED,
                                 mouseInput.getButton(),
                                 mouseInput.getX(),
                                 mouseInput.getY());
        }
    }

    /**
      * Handles mouse pressed input.
      *
      * @param mouseInput The mouse input to handle.
      * @since 0.6.0
      */
    void handleMousePressed(in MouseInput mouseInput)
    {
        Widget sourceWidget = getMouseEventSource(mouseInput.getX(), mouseInput.getY());

        if (mFocusHandler.getDraggedWidget() !is null)
        {
            sourceWidget = cast(Widget)mFocusHandler.getDraggedWidget();
        }

        int sourceWidgetX, sourceWidgetY;
        sourceWidget.getAbsolutePosition(sourceWidgetX, sourceWidgetY);
        
		if (mFocusHandler.getModalFocused() !is null
            && sourceWidget.isModalFocused()
            || mFocusHandler.getModalFocused() is null)
        {
            sourceWidget.requestFocus();
        }

        distributeMouseEvent(sourceWidget,
                             MouseEvent.PRESSED,
                             mouseInput.getButton(),
                             mouseInput.getX(),
                             mouseInput.getY());

        mFocusHandler.setLastWidgetPressed(sourceWidget);
        
        mFocusHandler.setDraggedWidget(sourceWidget);
        mLastMouseDragButton = mouseInput.getButton();

        if (mLastMousePressTimeStamp < 300
            && mLastMousePressButton == mouseInput.getButton())
        {
            mClickCount++;
        }
        else
        {
            mClickCount = 1;
        }

        mLastMousePressButton = mouseInput.getButton();
        mLastMousePressTimeStamp = mouseInput.getTimeStamp();
    }

    /**
      *
      * Handles mouse wheel moved down input.
      *
      * @param mouseInput The mouse input to handle.
      * @since 0.6.0
      */
    void handleMouseWheelMovedDown(MouseInput mouseInput)
    {
        Widget sourceWidget = getMouseEventSource(mouseInput.getX(), mouseInput.getY());

        if (mFocusHandler.getDraggedWidget() !is null)
        {
            sourceWidget = cast(Widget)mFocusHandler.getDraggedWidget();
        }

        int sourceWidgetX, sourceWidgetY;
        sourceWidget.getAbsolutePosition(sourceWidgetX, sourceWidgetY);

        distributeMouseEvent(sourceWidget,
                             MouseEvent.WHEEL_MOVED_DOWN,
                             mouseInput.getButton(),
                             mouseInput.getX(),
                             mouseInput.getY());
    }

    /**
      * Handles mouse wheel moved up input.
      *
      * @param mouseInput The mouse input to handle.
      * @since 0.6.0
      */
    void handleMouseWheelMovedUp(MouseInput mouseInput)
    {
        Widget sourceWidget = getMouseEventSource(mouseInput.getX(), mouseInput.getY());

        if (mFocusHandler.getDraggedWidget() !is null)
        {
            sourceWidget = cast(Widget)mFocusHandler.getDraggedWidget();
        }

        int sourceWidgetX, sourceWidgetY;
        sourceWidget.getAbsolutePosition(sourceWidgetX, sourceWidgetY);

        distributeMouseEvent(sourceWidget,
                             MouseEvent.WHEEL_MOVED_UP,
                             mouseInput.getButton(),
                             mouseInput.getX(),
                             mouseInput.getY());
    }    

    /**
      * Handles mouse released input.
      *
      * @param mouseInput The mouse input to handle.
      * @since 0.6.0
      */
    void handleMouseReleased(MouseInput mouseInput)
    {
        Widget sourceWidget = getMouseEventSource(mouseInput.getX(), mouseInput.getY());

        if (mFocusHandler.getDraggedWidget() !is null)
        {
            if (sourceWidget !is mFocusHandler.getLastWidgetPressed())
            {
                mFocusHandler.setLastWidgetPressed(null);
            }
            
            sourceWidget = cast(Widget)mFocusHandler.getDraggedWidget();
        }

        int sourceWidgetX, sourceWidgetY;
        sourceWidget.getAbsolutePosition(sourceWidgetX, sourceWidgetY);
        
        distributeMouseEvent(sourceWidget,
                             MouseEvent.RELEASED,
                             mouseInput.getButton(),
                             mouseInput.getX(),
                             mouseInput.getY());

        if (mouseInput.getButton() == mLastMousePressButton            
            && mFocusHandler.getLastWidgetPressed() is sourceWidget)
        {
            distributeMouseEvent(sourceWidget,
                                 MouseEvent.CLICKED,
                                 mouseInput.getButton(),
                                 mouseInput.getX(),
                                 mouseInput.getY());
            
            mFocusHandler.setLastWidgetPressed(null);
        }
        else
        {
            mLastMousePressButton = 0;
            mClickCount = 0;
        }

        if (mFocusHandler.getDraggedWidget() !is null)
        {
            mFocusHandler.setDraggedWidget(null);
        }
    }

    /**
      * Handles modal focus. Modal focus needs to be checked at 
      * each logic iteration as it might be necessary to distribute
      * mouse entered or mouse exited events.
      *
      * @since 0.8.0
      */
    void handleModalFocus()
    {
        // Check if modal focus has been gained by a widget.
        if ((mFocusHandler.getLastWidgetWithModalFocus() 
                !is mFocusHandler.getModalFocused())
             && (mFocusHandler.getLastWidgetWithModalFocus() is null))
        {
            handleModalFocusGained();
            mFocusHandler.setLastWidgetWithModalFocus(mFocusHandler.getModalFocused());
        }
        // Check if modal focus has been released.
        else if ((mFocusHandler.getLastWidgetWithModalFocus()
                    !is mFocusHandler.getModalFocused())
                    && (mFocusHandler.getLastWidgetWithModalFocus() !is null))
        {
            handleModalFocusReleased();
            mFocusHandler.setLastWidgetWithModalFocus(null);
        }
    }

    /**
      * Handles modal mouse input focus. Modal mouse input focus needs 
      * to be checked at each logic iteration as it might be necessary to 
      * distribute mouse entered or mouse exited events.
      *
      * @since 0.8.0
      */
    void handleModalMouseInputFocus()
    {
        // Check if modal mouse input focus has been gained by a widget.
        if ((mFocusHandler.getLastWidgetWithModalMouseInputFocus() 
                !is mFocusHandler.getModalMouseInputFocused())
             && (mFocusHandler.getLastWidgetWithModalMouseInputFocus() is null))
        {
            handleModalFocusGained();
            mFocusHandler.setLastWidgetWithModalMouseInputFocus(mFocusHandler.getModalMouseInputFocused());
        }
        // Check if modal mouse input focus has been released.
        else if ((mFocusHandler.getLastWidgetWithModalMouseInputFocus()
                    !is mFocusHandler.getModalMouseInputFocused())
                    && (mFocusHandler.getLastWidgetWithModalMouseInputFocus() !is null))
        {
            handleModalFocusReleased();
            mFocusHandler.setLastWidgetWithModalMouseInputFocus(null);
        }
    }

    /**
      * Handles modal focus gained. If modal focus has been gaind it might 
      * be necessary to distribute mouse entered or mouse exited events.
      *
      * @since 0.8.0
      */
    void handleModalFocusGained()
    {
         // Distribute an event to all widgets in the "widget with mouse" queue.
        foreach(Widget widget; mWidgetWithMouseQueue)
        {
            if (Widget.widgetExists(widget))
            {
                distributeMouseEvent(widget,
                                     MouseEvent.EXITED,
                                     mLastMousePressButton,
                                     mLastMouseX,
                                     mLastMouseY,
                                     true,
                                     true);
            }
        }
        mWidgetWithMouseQueue = [];
        mFocusHandler.setLastWidgetWithModalMouseInputFocus(mFocusHandler.getModalMouseInputFocused());
    }

    /**
      * Handles modal mouse input focus gained. If modal focus has been gaind 
      * it might be necessary to distribute mouse entered or mouse exited events.
      *
      * @since 0.8.0
      */
    void handleModalFocusReleased()
    {
         // Check all widgets below the mouse to see if they are
        // present in the "widget with mouse" queue. If a widget
        // is not then it should be added and an entered event should
        // be sent to it.
        Widget widget = getMouseEventSource(mLastMouseX, mLastMouseY);
        Widget parent = widget;

        while (parent !is null)
        {
            parent = widget.getParent();

            // Check if the widget is present in the "widget with mouse" queue.
            bool widgetIsPresentInQueue = false;
            foreach(Widget w; mWidgetWithMouseQueue)
            {
              if( w is widget )
              {
                  widgetIsPresentInQueue =true;
                  break;
              }
            }

            // Widget is not present, send an entered event and add
            // it to the "widget with mouse" queue.
            if (!widgetIsPresentInQueue
                && Widget.widgetExists(widget))
            {
                distributeMouseEvent(widget,
                                     MouseEvent.ENTERED,
                                     mLastMousePressButton,
                                     mLastMouseX,
                                     mLastMouseY,
                                     false,
                                     true);
                mWidgetWithMouseQueue= [widget] ~ mWidgetWithMouseQueue;
            }

            Widget swap = widget;
            widget = parent;
            parent = swap.getParent();
        }
    }

    /**
      * Distributes a mouse event.
      *
      * @param type The type of the event to distribute,
      * @param button The button of the event (if any used) to distribute.
      * @param x The x coordinate of the event.
      * @param y The y coordinate of the event.
      * @param fource indicates whether the distribution should be forced or not.
      *               A forced distribution distributes the event even if a widget
      *               is not enabled, not visible, another widget has modal
      *               focus or another widget has modal mouse input focus. 
      *               Default value is false.
      * @param toSourceOnly indicates whether the distribution should be to the
      *                     source widget only or to it's parent's mouse listeners
      *                     as well.
      *
      * @since 0.6.0
      */
    void distributeMouseEvent(Widget source,
                                      int type,
                                      int button,
                                      int x,
                                      int y,
                                      bool force = false,
                                      bool toSourceOnly = false)
    {
//         writefln("distribute mouse events (%d %d)",x,y);
        Widget parent = source;
        Widget widget = source;

        if (mFocusHandler.getModalFocused() !is null
            && !widget.isModalFocused()
            && !force)
        {
            return;
        }

        if (mFocusHandler.getModalMouseInputFocused() !is null
            && !widget.isModalMouseInputFocused()
            && !force)
        {
            return;
        }

        MouseEvent mouseEvent = new MouseEvent(source,
                              mShiftPressed,
                              mControlPressed,
                              mAltPressed,
                              mMetaPressed,
                              type,
                              button,
                              x,
                              y,
                              mClickCount);

        while (parent !is null)
        {
            // If the widget has been removed due to input
            // cancel the distribution.
            if (!Widget.widgetExists(widget))
            {
                break;
            }

            parent = widget.getParent();

            if (widget.isEnabled() || force)
            {

                int widgetX, widgetY;
                widget.getAbsolutePosition(widgetX, widgetY);

                mouseEvent.setX( x - widgetX );
                mouseEvent.setY( y - widgetY );

//                 writefln("distribute mouse event to widget #%s, (%d %d)",widget.getId,mouseEvent.getX,mouseEvent.getY);

                auto mouseListeners = widget._getMouseListeners();

                // Send the event to all mouse listeners of the widget.
                foreach (MouseListener listener; mouseListeners)
                {
                    switch (mouseEvent.getType())
                    {
                      case MouseEvent.ENTERED:
                          listener.mouseEntered(mouseEvent);
                          break;
                      case MouseEvent.EXITED:
                          listener.mouseExited(mouseEvent);
                          break;
                      case MouseEvent.MOVED:
                          listener.mouseMoved(mouseEvent);
                          break;
                      case MouseEvent.PRESSED:
                          listener.mousePressed(mouseEvent);
                          break;
                      case MouseEvent.RELEASED:
                          listener.mouseReleased(mouseEvent);
                          break;
                      case MouseEvent.WHEEL_MOVED_UP:
                          listener.mouseWheelMovedUp(mouseEvent);
                          break;
                      case MouseEvent.WHEEL_MOVED_DOWN:
                          listener.mouseWheelMovedDown(mouseEvent);
                          break;
                      case MouseEvent.DRAGGED:
                          listener.mouseDragged(mouseEvent);
                          break;
                      case MouseEvent.CLICKED:
                          listener.mouseClicked(mouseEvent);
                          break;
                      default:
                          throw new GCN_Exception("Unknown mouse event type.");
                    }
                }
                if (toSourceOnly)
                {
                    break;
                }

            }

            Widget swap = widget;
            widget = parent;
            parent = swap.getParent();
            

            // If a non modal focused widget has been reach
            // and we have modal focus cancel the distribution.
            if (mFocusHandler.getModalFocused() !is null
                && !widget.isModalFocused())
            {
                break;
            }

            // If a non modal mouse input focused widget has been reach
            // and we have modal mouse input focus cancel the distribution.
            if (mFocusHandler.getModalMouseInputFocused() !is null
                && !widget.isModalMouseInputFocused())
            {
                break;
            }
        }
    }
    /**
      * Distributes a key event.
      *
      * @param keyEvent The key event to distribute.

      * @since 0.6.0
      */
    void distributeKeyEvent(KeyEvent keyEvent)
    {
        Widget parent = cast(Widget)keyEvent.getSource();
        Widget widget = cast(Widget)keyEvent.getSource();

        if (mFocusHandler.getModalFocused() !is null
            && !widget.isModalFocused())
        {
            return;
        }

        if (mFocusHandler.getModalMouseInputFocused() !is null
            && !widget.isModalMouseInputFocused())
        {
            return;
        }

        while (parent !is null)
        {
            // If the widget has been removed due to input
            // cancel the distribution.
            if (!Widget.widgetExists(widget))
            {
                break;
            }

            parent = widget.getParent();

            if (widget.isEnabled())
            {
               auto keyListeners = widget._getKeyListeners();
                // Send the event to all key listeners of the source widget.
                foreach (KeyListener listener; keyListeners)
                {
                    switch (keyEvent.getType())
                    {
                      case KeyEvent.PRESSED:
                          listener.keyPressed(keyEvent);
                          break;
                      case KeyEvent.RELEASED:
                          listener.keyReleased(keyEvent);
                          break;
                      default:
                          throw new GCN_Exception("Unknown key event type.");
                    }
                }
            }

            Widget swap = widget;
            widget = parent;
            parent = swap.getParent();

            // If a non modal focused widget has been reach
            // and we have modal focus cancel the distribution.
            if (mFocusHandler.getModalFocused() !is null
                && !widget.isModalFocused())
            {
                break;
            }
        }
    }

    /**
      * Distributes a key event to the global key listeners.
      *
      * @param keyEvent The key event to distribute.
      *
      * @since 0.6.0
      */
//     virtual void distributeKeyEventToGlobalKeyListeners(KeyEvent& keyEvent);

    /**
      * Gets the widget at a certain position.
      *
      * @return The widget at a certain position.
      * @since 0.6.0
      */
    Widget getWidgetAt(int x, int y)
    {
        // If the widget's parent has no child then we have found the widget..
        Widget parent = mTop;
        Widget child = mTop;

        while (child !is null)
        {
            Widget swap = child;
            int parentX, parentY;
            parent.getAbsolutePosition(parentX, parentY);
            child = parent.getWidgetAt(x - parentX, y - parentY);
            parent = swap;
        }
        return parent;
    }


    /**
      * Gets the source of the mouse event.
      *
      * @return The source widget of the mouse event.
      * @since 0.6.0
      */
    Widget getMouseEventSource(int x, int y)
    {
        Widget widget = getWidgetAt(x, y);

        if (mFocusHandler.getModalMouseInputFocused() !is null
            && !widget.isModalMouseInputFocused())
        {
            return cast(Widget)mFocusHandler.getModalMouseInputFocused();
        }

        return widget;
    }

    /**
      * Gets the source of the key event.
      *
      * @return The source widget of the key event.
      * @since 0.6.0
      */
    Widget getKeyEventSource()
    {
        Widget widget = cast(Widget)mFocusHandler.getFocused;

        while (widget._getInternalFocusHandler !is null
               && widget._getInternalFocusHandler.getFocused !is null)
        {
            widget = cast(Widget)widget._getInternalFocusHandler.getFocused;
        }

        return widget;
    }

    /**
      * Holds the top widget.
      */
    Widget mTop;

    /**
      * Holds the graphics implementation used.
      */
    Graphics mGraphics;

    /**
      * Holds the input implementation used.
      */
    Input mInput;

    /**
      * Holds the focus handler for the Gui.
      */
    FocusHandler mFocusHandler;

    /**
      * True if tabbing is enabled, false otherwise.
      */
    bool mTabbing;

/+    /**
      * Typedef.
      */
    typedef std.list<KeyListener*> KeyListenerList;

    /**
      * Typedef.
      */
    typedef KeyListenerList.iterator KeyListenerListIterator;

    /**
      * Holds the global key listeners of the Gui.
      */
    KeyListenerList mKeyListeners;+/
    
    /**
      * True if shift is pressed, false otherwise.
      */
    bool mShiftPressed;

    /**
      * True if meta is pressed, false otherwise.
      */
    bool mMetaPressed;

    /**
      * True if control is pressed, false otherwise.
      */
    bool mControlPressed;

    /**
      * True if alt is pressed, false otherwise.
      */
    bool mAltPressed;

    /**
      * Holds the last mouse button pressed.
      */
    uint mLastMousePressButton;

    /**
      * Holds the last mouse press time stamp.
      */
    int mLastMousePressTimeStamp;

    /**
      * Holds the last mouse x coordinate.
      */
    int mLastMouseX;

    /**
      * Holds the last mouse y coordinate.
      */
    int mLastMouseY;

    /**
      * Holds the current click count. Used to keep track
      * of clicks for a the last pressed button.
      */
    int mClickCount;

    /**
      * Holds the last button used when a drag of a widget
      * was initiated. Used to be able to release a drag
      * when the same button is released.
      */
    int mLastMouseDragButton;

    /**
      * Holds a stack with all the widgets with the mouse.
      * Used to properly distribute mouse events.
      */
    Widget[] mWidgetWithMouseQueue;
}
