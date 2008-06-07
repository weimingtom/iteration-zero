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

module guichan.widget;

import guichan.iwidget;
import guichan.listener;
import guichan.util;
import guichan.basiccontainer;

import guichan.defaultfont;
import guichan.font;
import guichan.rectangle;
import guichan.color;

import guichan.exception;
import guichan.focushandler;
import guichan.graphics;
import guichan.keyinput;
import guichan.mouseinput;
import guichan.event;

import dlisp.bind;

class Widget : IWidget, MouseListener, KeyListener, FocusListener, WidgetListener
{
  private:
    static Widget[] mWidgets;

  protected:
    /**
      * Holds the foreground color of the widget.
      */
    Color mForegroundColor;

    /**
      * Holds the background color of the widget.
      */
    Color mBackgroundColor;

    /**
      * Holds the Frame color of the widget.
      */
    Color mFrameColor;

    /**
      * Holds the selection color of the widget.
      */
    Color mSelectionColor;

    /**
      * Holds the focus handler used by the widget.
      */
    FocusHandler mFocusHandler;

    /**
      * Holds the focus handler used by the widget. NULL
      * if no internal focus handler is used.
      */
    FocusHandler mInternalFocusHandler;

    /**
      * Holds the parent of the widget. NULL if the widget
      * has no parent.
      */
    Widget mParent;

    /**
      * Holds the dimension of the widget.
      */
    Rectangle mDimension;

    /** 
      * Holds the frame size of the widget.
      */
    uint mFrameSize;

    /**
      * Holds the action event of the widget.
      */
    string mActionEventId;

    /**
      * True if the widget focusable, false otherwise.
      */
    bool mFocusable;

    /**
      * True if the widget visible, false otherwise.
      */
    bool mVisible;

    /**
      * True if the widget has tab in enabled, false otherwise.
      */
    bool mTabIn;

    /**
      * True if the widget has tab in enabled, false otherwise.
      */
    bool mTabOut;

    /**
      * True if the widget is enabled, false otherwise.
      */
    bool mEnabled;

    /**
      * Holds the id of the widget.
      */
    string mId;

    /**
      * Holds the font used by the widget.
      */
    Font mCurrentFont;

    /**
    * Holds the mouse listeners of the widget.
    */
    MouseListener[] mMouseListeners;
  
    /**
    * Holds the key listeners of the widget.
    */
    KeyListener[] mKeyListeners;
  
    /** 
    * Holds the action listeners of the widget.
    */
    //  ActionListenerList mActionListeners;
  
    /**
    * Holds the focus listeners of the widget.
    */
    FocusListener[] mFocusListeners;
  
    /**
    * Holds the widget listeners of the widget.
    */
    WidgetListener[] mWidgetListeners;

    /**
      * Holds the default font used by the widget.
      */
    static DefaultFont mDefaultFont;

    /**
      * Holds the global font used by the widget.
      */
    static Font mGlobalFont;

    public:

    this()
    {
        mForegroundColor = Color(238,239,247);
        mBackgroundColor = Color(28,29,33,255);
        mFrameColor = Color(49,53,61,255);
//         mColor = Color(68,88,120,255);
        mSelectionColor = Color(146,205,207,255);
        mFocusHandler = null;
        mInternalFocusHandler = null;
        mParent = null;
        mFrameSize = 0;
        mFocusable = false;
        mVisible = true;
        mTabIn = true;
        mTabOut = true;
        mEnabled = true;
        mCurrentFont = null;

        mWidgets ~= this;

        addMouseListener(this);
        addKeyListener(this);
        addFocusListener(this);
        addWidgetListener(this);

    }

    ~this()
    {
        _setFocusHandler(null);

        mWidgets.remove(this);
    }

    abstract void draw(Graphics graphics);

    void drawFrame(Graphics graphics)
    {
        graphics.setColor(getFrameColor);
        uint i;
        for (i = 0; i < getFrameSize; ++i)
        {
            graphics.drawLine(i,i, getWidth - i, i);
            graphics.drawLine(i,i + 1, i, getHeight - i - 1);
            graphics.drawLine(getWidth - i,i + 1, getWidth - i, getHeight - i);
            graphics.drawLine(i,getHeight - i, getWidth - i - 1, getHeight - i);
        }
    }

    void _setParent(Widget parent)
    {
        mParent = parent;
    }

    Widget getParent()
    {
        return mParent;
    }

    void setWidth(int width)
    {
        Rectangle newDimension = mDimension;
        newDimension.width = width;

        setDimension(newDimension);
    }

    int getWidth()
    {
        return mDimension.width;
    }

    void setHeight(int height)
    {
        Rectangle newDimension = mDimension;
        newDimension.height = height;

        setDimension(newDimension);
    }

    int getHeight()
    {
        return mDimension.height;
    }

    void setX(int x)
    {
        Rectangle newDimension = mDimension;
        newDimension.x = x;

        setDimension(newDimension);
    }

    int getX()
    {
        return mDimension.x;
    }

    void setY(int y)
    {
        Rectangle newDimension = mDimension;
        newDimension.y = y;

        setDimension(newDimension);
    }

    int getY()
    {
        return mDimension.y;
    }

    void setPosition(int x, int y)
    {
        Rectangle newDimension = mDimension;
        newDimension.x = x;
        newDimension.y = y;
        setDimension(newDimension);
    }

    void setDimension(in Rectangle dimension)
    { 
        Rectangle oldDimension = mDimension;
        mDimension = dimension;

        if (mDimension.width != oldDimension.width
            || mDimension.height != oldDimension.height)
        {
            distributeResizedEvent;
        }

        if (mDimension.x != oldDimension.x
            || mDimension.y != oldDimension.y)
        {
            distributeMovedEvent;
        }
    }

    void setFrameSize(uint frameSize)
    {
        mFrameSize = frameSize;
    }

    int getFrameSize()
    {
        return mFrameSize;
    }

    Rectangle getDimension()
    {
        return mDimension;
    }

    string getActionEventId()
    {
        return mActionEventId;
    }

    void setActionEventId(in string actionEventId)
    {
        mActionEventId = actionEventId;
    }

    bool isFocused()
    {
        if (!mFocusHandler)
        {
            return false;
        }

        return (mFocusHandler.isFocused(this));
    }

    void setFocusable(bool focusable)
    {
        if (!focusable && isFocused())
        {
            mFocusHandler.focusNone;
        }

        mFocusable = focusable;
    }

    bool isFocusable()
    {
        return mFocusable && isVisible && isEnabled;
    }

    void requestFocus()
    {
        if (mFocusHandler is null)
        {
            throw new GCN_Exception("No focushandler set (did you add the widget to the gui?).");
        }

        if (isFocusable)
        {
            mFocusHandler.requestFocus(this);
        }
    }

    void requestMoveToTop()
    {
        if (mParent)
        {
            mParent.moveToTop(this);
        }
    }

    void requestMoveToBottom()
    {
        if (mParent)
        {
            mParent.moveToBottom(this);
        }
    }

    void moveToTop(Widget widget) { }
    void moveToBottom(Widget widget) { }
    void fontChanged() { }
    void widgetAction(Event) { }
    void logic() { }

    void setVisible(bool visible)
    {
        if (!visible && isFocused)
        {
            mFocusHandler.focusNone;
        }
        
        if (visible)
        {
            distributeShownEvent;
        }
        else if(!visible)
        {
            distributeHiddenEvent;
        }

        mVisible = visible;
    }

    bool isVisible()
    {
        if (getParent is null)
        {
            return mVisible;
        }
        else
        {
            return mVisible && getParent.isVisible;
        }
    }

    void setFrameColor(in Color color)
    {
        mFrameColor = color;
    }

    Color getFrameColor()
    {
        return mFrameColor;
    }

    void setForegroundColor(Color color)
    {
        mForegroundColor = color;
    }

    Color getForegroundColor()
    {
        return mForegroundColor;
    }

    void setBackgroundColor(Color color)
    {
        mBackgroundColor = color;
    }

    Color getBackgroundColor()
    {
        return mBackgroundColor;
    }

    void setSelectionColor(Color color)
    {
        mSelectionColor = color;
    }

    Color getSelectionColor()
    {
        return mSelectionColor;
    }
    
    void _setFocusHandler(FocusHandler focusHandler)
    {
        if (mFocusHandler)
        {
            releaseModalFocus;
            mFocusHandler.remove(this);
        }

        if (focusHandler)
        {
            focusHandler.add(this);
        }

        mFocusHandler = focusHandler;
    }

    FocusHandler _getFocusHandler()
    {
        return mFocusHandler;
    }

    void addKeyListener(KeyListener keyListener)
    {
        mKeyListeners ~= keyListener;
    }

    void removeKeyListener(KeyListener keyListener)
    {
        mKeyListeners.remove(keyListener);
    }

    void addFocusListener(FocusListener focusListener)
    {
        mFocusListeners ~= (focusListener);
    }

    void removeFocusListener(FocusListener focusListener)
    {
        mFocusListeners.remove(focusListener);
    }

    void addMouseListener(MouseListener mouseListener)
    {
        mMouseListeners ~= (mouseListener);
    }

    void removeMouseListener(MouseListener mouseListener)
    {
        mMouseListeners.remove(mouseListener);
    }

    void addWidgetListener(WidgetListener widgetListener)
    {
        mWidgetListeners ~= (widgetListener);
    }

    void removeWidgetListener(WidgetListener widgetListener)
    {
        mWidgetListeners.remove(widgetListener);
    }

    void getAbsolutePosition(ref int x, ref int y)
    {
        if (getParent is null)
        {
            x = mDimension.x;
            y = mDimension.y;
            return;
        }

        int parentX;
        int parentY;

        getParent.getAbsolutePosition(parentX, parentY);

        x = parentX + mDimension.x + getParent.getChildrenArea().x;
        y = parentY + mDimension.y + getParent.getChildrenArea().y;
    }

    Font getFont()
    {
        if (mCurrentFont is null)
        {
            if (mGlobalFont is null)
            {
                return mDefaultFont;
            }

            return mGlobalFont;
        }

        return mCurrentFont;
    }

    static void setGlobalFont(Font font)
    {
        mGlobalFont = font;

        foreach(Widget widget; mWidgets)
        {
            if (widget.mCurrentFont is null)
            {
                widget.fontChanged;
            }
        }
    }

    void setFont(Font font)
    {
        mCurrentFont = font;
        fontChanged;
    }

    static bool widgetExists(Widget widget)
    {
      foreach(Widget w; mWidgets)
        if( widget is w)
          return true;
      return false;
    }

    bool isTabInEnabled()
    {
        return mTabIn;
    }

    void setTabInEnabled(bool enabled)
    {
        mTabIn = enabled;
    }

    bool isTabOutEnabled()
    {
        return mTabOut;
    }

    void setTabOutEnabled(bool enabled)
    {
        mTabOut = enabled;
    }

    void setSize(int width, int height)
    {
        Rectangle newDimension = mDimension;
        newDimension.width = width;
        newDimension.height = height;
        setDimension(newDimension);
    }

    void setEnabled(bool enabled)
    {
        mEnabled = enabled;
    }

    bool isEnabled()
    {
        return mEnabled && isVisible;
    }

    void requestModalFocus()
    {
        if (mFocusHandler is null)
        {
            throw new GCN_Exception("No focushandler set (did you add the widget to the gui?).");
        }

        mFocusHandler.requestModalFocus(this);
    }

    void requestModalMouseInputFocus()
    {
        if (mFocusHandler is null)
        {
           throw new GCN_Exception("No focushandler set (did you add the widget to the gui?).");
        }

        mFocusHandler.requestModalMouseInputFocus(this);
    }

    void releaseModalFocus()
    {
        if (mFocusHandler is null)
        {
            return;
        }

        mFocusHandler.releaseModalFocus(this);
    }

    void releaseModalMouseInputFocus()
    {
        if (mFocusHandler is null)
        {
            return;
        }

        mFocusHandler.releaseModalMouseInputFocus(this);
    }

    bool isModalFocused()
    {
        if (mFocusHandler is null)
        {
          throw new GCN_Exception("No focushandler set (did you add the widget to the gui?).");
        }

        if (getParent !is null)
        {
            return (mFocusHandler.getModalFocused is this) 
                || getParent.isModalFocused;
        }

        return mFocusHandler.getModalFocused is this;
    }

    bool isModalMouseInputFocused()
    {
        if (mFocusHandler is null)
        {
            throw new GCN_Exception("No focushandler set (did you add the widget to the gui?).");
        }

        if (getParent !is null)
        {
            return (mFocusHandler.getModalMouseInputFocused is this) 
                || getParent.isModalMouseInputFocused;
        }

        return mFocusHandler.getModalMouseInputFocused is this;
    }

    Widget getWidgetAt(int x, int y)
    {
        return null;
    }

    MouseListener[] _getMouseListeners()
    {
        return mMouseListeners;
    }

    KeyListener[] _getKeyListeners()
    {
        return mKeyListeners;
    }

    FocusListener[] _getFocusListeners()
    {
        return mFocusListeners;
    }

    Rectangle getChildrenArea()
    {
        return Rectangle(0, 0, 0, 0);
    }

    FocusHandler _getInternalFocusHandler()
    {
        return mInternalFocusHandler;
    }

    void setInternalFocusHandler(FocusHandler focusHandler)
    {
        mInternalFocusHandler = focusHandler;
    }

    void setId(string id)
    {
        mId = id;
    }

    string getId()
    {
        return mId;
    }

    void distributeResizedEvent()
    {
        foreach(Widget widget; mWidgets)
        {
            widget.widgetResized(new Event(this));
        }
    }

    void distributeMovedEvent()
    {
        foreach(Widget widget; mWidgets)
        {
            widget.widgetMoved(new Event(this));
        }
    }

    void distributeHiddenEvent()
    {
        foreach(Widget widget; mWidgets)
        {
            widget.widgetHidden(new Event(this));
        }
    }

    void distributeActionEvent()
    {
        foreach(Widget widget; mWidgets)
        {
            widget.widgetAction(new Event(this));
        }
    }

    void distributeShownEvent()
    {
        foreach(Widget widget; mWidgets)
        {
            widget.widgetShown(new Event(this));
        }
    }

    void showPart(Rectangle rectangle)
    {
        if (mParent !is null)
        {
            mParent.showWidgetPart(this, rectangle);
        }
    }

    void showWidgetPart(Widget widget, Rectangle area) { }

    void widgetResized(Event event) { widgetResizedCallback(event); }

    void widgetMoved(Event event) { widgetMovedCallback(event); }
    void widgetHidden(Event event) { widgetHiddenCallback(event);  }
    void widgetShown(Event event) {  widgetShownCallback(event);  }

    void focusGained(Event event) { focusGainedCallback(event); }
    void focusLost(Event event) { focusLostCallback(event);  }

    void keyPressed(KeyEvent keyEvent) { keyPressedCallback(keyEvent); }
    void keyReleased(KeyEvent keyEvent) { keyReleasedCallback(keyEvent);  }

    void mouseEntered(MouseEvent mouseEvent) { mouseEnteredCallback(mouseEvent); }

    void mouseExited(MouseEvent mouseEvent) { mouseExitedCallback(mouseEvent); }
    void mousePressed(MouseEvent mouseEvent) { mousePressedCallback(mouseEvent); }
    void mouseReleased(MouseEvent mouseEvent) { mouseReleasedCallback(mouseEvent); }
    void mouseClicked(MouseEvent mouseEvent) { mouseClickedCallback(mouseEvent); }

    void mouseWheelMovedUp(MouseEvent mouseEvent) { mouseWheelMovedUpCallback(mouseEvent); }
    void mouseWheelMovedDown(MouseEvent mouseEvent) { mouseWheelMovedDownCallback(mouseEvent); }
    void mouseMoved(MouseEvent mouseEvent) { mouseMovedCallback(mouseEvent); }
    void mouseDragged(MouseEvent mouseEvent) { mouseDraggedCallback(mouseEvent); }

    mixin BindClass!("C/Widget");
    mixin BindMethods!(setId,getId,getParent);
    mixin BindMethods!(getX,getY,getWidth,getHeight);
    mixin BindMethods!(setX,setY,setWidth,setHeight);
    mixin BindMethods!(setPosition,setSize);

    mixin BindHandlers!(widgetResized,widgetMoved,widgetHidden,widgetShown);
    mixin BindHandlers!(keyPressed,keyReleased);
    mixin BindHandlers!(focusGained,focusLost);
    mixin BindHandlers!(mouseEntered,mouseExited,mousePressed,mouseReleased,mouseClicked);
    mixin BindHandlers!(mouseWheelMovedUp,mouseWheelMovedDown,mouseMoved,mouseDragged);
}
