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

/*
 * For comments regarding functions please see the header file.
 */

module guichan.focushandler;

import guichan.exception;
import guichan.event;
import guichan.widget;
import guichan.util;
import guichan.listener;

class FocusHandler
{
    this()
    {
        mFocusedWidget = (null);
        mModalFocusedWidget = (null);
        mModalMouseInputFocusedWidget = (null);
        mDraggedWidget = (null);
        mLastWidgetWithMouse = (null);
        mLastWidgetWithModalFocus = (null);
        mLastWidgetWithModalMouseInputFocus = (null);
        mLastWidgetPressed = (null);
    }

    void requestFocus(Widget widget)
    {
        if (widget is null
            || widget is mFocusedWidget)
        {
            return;
        }
        
        uint i = 0;
        int toBeFocusedIndex = -1;
        for (i = 0; i < mWidgets.length; ++i)
        {
            if (mWidgets[i] == widget)
            {
                toBeFocusedIndex = i;
                break;
            }
        }
        
        if (toBeFocusedIndex < 0)
        {
            throw new GCN_Exception("Trying to focus a none existing widget.");
        }
        
        Widget oldFocused = mFocusedWidget;
        
        if (oldFocused !is widget)
        {
            mFocusedWidget = mWidgets[toBeFocusedIndex];
            
            if (oldFocused !is null)
            {
                distributeFocusLostEvent(new Event(oldFocused));
            }
            distributeFocusGainedEvent(new Event(mWidgets[toBeFocusedIndex]));
        }
    }

    void requestModalFocus(Widget widget)
    {
        if (mModalFocusedWidget !is null && mModalFocusedWidget !is widget)
        {
            throw new GCN_Exception("Another widget already has modal focus.");
        }

        mModalFocusedWidget = widget;

        if (mFocusedWidget !is null 
            && !mFocusedWidget.isModalFocused)
        {
            focusNone;
        }
    }

    void requestModalMouseInputFocus(Widget widget)
    {
        if (mModalMouseInputFocusedWidget !is null
            && mModalMouseInputFocusedWidget !is widget)
        {
            throw new GCN_Exception("Another widget already has modal input focus.");
        }

        mModalMouseInputFocusedWidget = widget;
    }

    void releaseModalFocus(Widget widget)
    {
        if (mModalFocusedWidget is widget)
        {
            mModalFocusedWidget = null;
        }
    }

    void releaseModalMouseInputFocus(Widget widget)
    {
        if (mModalMouseInputFocusedWidget is widget)
        {
            mModalMouseInputFocusedWidget = null;
        }
    }

    Widget getFocused()
    {
        return mFocusedWidget;
    }

    Widget getModalFocused()
    {
        return mModalFocusedWidget;
    }

    Widget getModalMouseInputFocused()
    {
        return mModalMouseInputFocusedWidget;
    }

    void focusNext()
    {
        int i;
        int focusedWidget = -1;
        for (i = 0; i < mWidgets.length; ++i)
        {
            if (mWidgets[i] is mFocusedWidget)
            {
                focusedWidget = i;
            }
        }
        int focused = focusedWidget;

        // i is a counter that ensures that the following loop
        // won't get stuck in an infinite loop
        i = cast(int)mWidgets.length;
        do
        {
            ++focusedWidget;

            if (i==0)
            {
                focusedWidget = -1;
                break;
            }

            --i;

            if (focusedWidget >= cast(int)mWidgets.length)
            {
                focusedWidget = 0;
            }

            if (focusedWidget == focused)
            {
                return;
            }
        }
        while (!mWidgets[focusedWidget].isFocusable());

        if (focusedWidget >= 0)
        {
            mFocusedWidget = mWidgets[focusedWidget];

            Event focusEvent = new Event(mFocusedWidget);
            distributeFocusGainedEvent(focusEvent);
        }

        if (focused >= 0)
        {
            Event focusEvent = new Event(mWidgets[focused]);
            distributeFocusLostEvent(focusEvent);
        }
    }

    void focusPrevious()
    {
        if (mWidgets.length == 0)
        {
            mFocusedWidget = null;
            return;
        }

        int i;
        int focusedWidget = -1;
        for (i = 0; i < cast(int)mWidgets.length; ++i)
        {
            if (mWidgets[i] is mFocusedWidget)
            {
                focusedWidget = i;
            }
        }
        int focused = focusedWidget;

        // i is a counter that ensures that the following loop
        // won't get stuck in an infinite loop
        i = cast(int)mWidgets.length;
        do
        {
            --focusedWidget;

            if (i==0)
            {
                focusedWidget = -1;
                break;
            }

            --i;

            if (focusedWidget <= 0)
            {
                focusedWidget = mWidgets.length - 1;
            }

            if (focusedWidget == focused)
            {
                return;
            }
        }
        while (!mWidgets[focusedWidget].isFocusable());

        if (focusedWidget >= 0)
        {
            mFocusedWidget = mWidgets[focusedWidget];
            Event focusEvent = new Event(mFocusedWidget);
            distributeFocusGainedEvent(focusEvent);
        }

        if (focused >= 0)
        {
            Event focusEvent = new Event(mWidgets[focused]);
            distributeFocusLostEvent(focusEvent);
        }
    }

    bool isFocused(Widget widget)
    {
        return mFocusedWidget is widget;
    }

    void add(Widget widget)
    {
        mWidgets~=(widget);
    }

    void remove(Widget widget)
    {
        if (isFocused(widget))
        {
            mFocusedWidget = null;
        }

        mWidgets.remove_all(widget);

        if (mDraggedWidget is widget)
        {
            mDraggedWidget = null;
            return;
        }
        
        if (mLastWidgetWithMouse is widget)
        {
            mLastWidgetWithMouse = null;
            return;
        }

        if (mLastWidgetWithModalFocus is widget)
        {
            mLastWidgetWithModalFocus = null;
            return;
        }

        if (mLastWidgetWithModalMouseInputFocus is widget)
        {
            mLastWidgetWithModalMouseInputFocus = null;
            return;
        }

        if (mLastWidgetPressed is widget)
        {
            mLastWidgetPressed = null;
            return;
        }
    }

    void focusNone()
    {
        if (mFocusedWidget !is null)
        {
            Widget focused = mFocusedWidget;
            mFocusedWidget = null;

            Event focusEvent = new Event(focused);
            distributeFocusLostEvent(focusEvent);
        }
    }

    void tabNext()
    {
        if (mFocusedWidget !is null)
        {
            if (!mFocusedWidget.isTabOutEnabled)
            {
                return;
            }
        }

        if (mWidgets.length == 0)
        {
            mFocusedWidget = null;
            return;
        }

        int i;
        int focusedWidget = -1;
        for (i = 0; i < cast(int)mWidgets.length; ++i)
        {
            if (mWidgets[i] is mFocusedWidget)
            {
                focusedWidget = i;
            }
        }
        int focused = focusedWidget;
        bool done = false;

        // i is a counter that ensures that the following loop
        // won't get stuck in an infinite loop
        i = cast(int)mWidgets.length;
        do
        {
            ++focusedWidget;

            if (i==0)
            {
                focusedWidget = -1;
                break;
            }

            --i;

            if (focusedWidget >= cast(int)mWidgets.length)
            {
                focusedWidget = 0;
            }

            if (focusedWidget == focused)
            {
                return;
            }

            if (mWidgets[focusedWidget].isFocusable &&
                mWidgets[focusedWidget].isTabInEnabled &&
                (mModalFocusedWidget is null ||
                 mWidgets[focusedWidget].isModalFocused))
            {
                done = true;
            }
        }
        while (!done);

        if (focusedWidget >= 0)
        {
            mFocusedWidget = mWidgets[focusedWidget];
            Event focusEvent = new Event(mFocusedWidget);
            distributeFocusGainedEvent(focusEvent);
        }

        if (focused >= 0)
        {
            Event focusEvent = new Event(mWidgets[focused]);
            distributeFocusLostEvent(focusEvent);
        }
    }

    void tabPrevious()
    {
        if (mFocusedWidget !is null)
        {
            if (!mFocusedWidget.isTabOutEnabled)
            {
                return;
            }
        }

        if (mWidgets.length == 0)
        {
            mFocusedWidget = null;
            return;
        }

        int i;
        int focusedWidget = -1;
        for (i = 0; i < cast(int)mWidgets.length; ++i)
        {
            if (mWidgets[i] is mFocusedWidget)
            {
                focusedWidget = i;
            }
        }
        int focused = focusedWidget;
        bool done = false;

        // i is a counter that ensures that the following loop
        // won't get stuck in an infinite loop
        i = cast(int)mWidgets.length;
        do
        {
            --focusedWidget;

            if (i==0)
            {
                focusedWidget = -1;
                break;
            }

            --i;

            if (focusedWidget <= 0)
            {
                focusedWidget = mWidgets.length - 1;
            }

            if (focusedWidget is focused)
            {
                return;
            }

            if (mWidgets[focusedWidget].isFocusable &&
                mWidgets[focusedWidget].isTabInEnabled &&
                (mModalFocusedWidget is null ||
                 mWidgets[focusedWidget].isModalFocused))
            {
                done = true;
            }
        }
        while (!done);

        if (focusedWidget >= 0)
        {
            mFocusedWidget = mWidgets[focusedWidget];
            Event focusEvent = new Event(mFocusedWidget);
            distributeFocusGainedEvent(focusEvent);
        }

        if (focused >= 0)
        {
            Event focusEvent = new Event(mWidgets[focused]);
            distributeFocusLostEvent(focusEvent);
        }
    }

    void distributeFocusLostEvent(Event focusEvent)
    {
        Widget sourceWidget = cast(Widget)focusEvent.getSource;

        FocusListener[] focusListeners = sourceWidget._getFocusListeners;

        // Send the event to all focus listeners of the widget.
        foreach (FocusListener f; focusListeners)
        {
            f.focusLost(focusEvent);
        }
    }

    void distributeFocusGainedEvent(Event focusEvent)
    {
        Widget sourceWidget = cast(Widget)focusEvent.getSource;

        FocusListener[] focusListeners = sourceWidget._getFocusListeners;

        // Send the event to all focus listeners of the widget.
        foreach (FocusListener f; focusListeners)
        {
            f.focusGained(focusEvent);
        }
    }

    Widget getDraggedWidget()
    {
        return mDraggedWidget;
    }

    void setDraggedWidget(Widget draggedWidget)
    {
        mDraggedWidget = draggedWidget;
    }

    Widget getLastWidgetWithMouse()
    {
        return mLastWidgetWithMouse;
    }

    void setLastWidgetWithMouse(Widget lastWidgetWithMouse)
    {
        mLastWidgetWithMouse = lastWidgetWithMouse;
    }

    Widget getLastWidgetWithModalFocus()
    {
        return mLastWidgetWithModalFocus;
    }

    void setLastWidgetWithModalFocus(Widget lastWidgetWithModalFocus)
    {
        mLastWidgetWithModalFocus = lastWidgetWithModalFocus;
    }

    Widget getLastWidgetWithModalMouseInputFocus()
    {
        return mLastWidgetWithModalMouseInputFocus;
    }

    void setLastWidgetWithModalMouseInputFocus(Widget lastWidgetWithModalMouseInputFocus)
    {
        mLastWidgetWithModalMouseInputFocus = lastWidgetWithModalMouseInputFocus;
    }

    Widget getLastWidgetPressed()
    {
        return mLastWidgetPressed;
    }

    void setLastWidgetPressed(Widget lastWidgetPressed)
    {
        mLastWidgetPressed = lastWidgetPressed;
    }

    protected:
    /**
      * Holds the widgets currently being handled by the
      * focus handler.
      */
    Widget[] mWidgets;

    /**
      * Holds the focused widget. NULL if no widget has focus.
      */
    Widget mFocusedWidget;

    /**
      * Holds the modal focused widget. NULL if no widget has
      * modal focused.
      */
    Widget mModalFocusedWidget;

    /**
      * Holds the modal mouse input focused widget. NULL if no widget 
      * is being dragged.
      */
    Widget mModalMouseInputFocusedWidget;

    /** 
      * Holds the dragged widget. NULL if no widget is
      * being dragged.
      */
    Widget mDraggedWidget;

    /**
      * Holds the last widget with the mouse.
      */
    Widget mLastWidgetWithMouse;

    /**
      * Holds the last widget with modal focus.
      */
    Widget mLastWidgetWithModalFocus;

    /**
      * Holds the last widget with modal mouse input focus.
      */
    Widget mLastWidgetWithModalMouseInputFocus;

    /**
      * Holds the last widget pressed.
      */
    Widget mLastWidgetPressed;
}
