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

module guichan.event;

import guichan.widget;
import guichan.key;

import dlisp.bind;

class Event
{
    private Widget mSource;

    this(Widget source)
    {
      mSource = source;
    }

    Widget getSource()
    {
        return mSource;
    }

    mixin BindClass!("Event");
}

class InputEvent : Event
{
    protected:
      /**
        * True if shift is pressed, false otherwise.
        */
      bool mShiftPressed;

      /**
        * True if control is pressed, false otherwise.
        */
      bool mControlPressed;

      /**
        * True if alt is pressed, false otherwise.
        */
      bool mAltPressed;

      /**
        * True if meta is pressed, false otherwise.
        */
      bool mMetaPressed;

      /**
        * True if the input event is consumed, 
        * false otherwise.
        */
      bool mIsConsumed;
    public:


    this(Widget source, bool isShiftPressed, bool isControlPressed, bool isAltPressed, bool isMetaPressed)
    {
        super(source);
        mShiftPressed = isShiftPressed;
        mControlPressed = isControlPressed;
        mAltPressed = isAltPressed;
        mMetaPressed = isMetaPressed;
        mIsConsumed = false;
    }

    bool isShiftPressed()
    {
        return mShiftPressed;
    }

    bool isControlPressed()
    {
        return mControlPressed;
    }

    bool isAltPressed()
    {
        return mAltPressed;
    }

    bool isMetaPressed()
    {
        return mMetaPressed;
    }

    void consume()
    {
        mIsConsumed = true;
    }

    bool isConsumed()
    {
        return mIsConsumed;
    }

    mixin BindClass!("InputEvent");
}

class KeyEvent: InputEvent
{
    protected:
        /**
         * Holds the type of the key event.
         */
        uint mType;

        /**
         * True if the numeric pad was used, false otherwise.
         */
        bool mIsNumericPad;

        /** 
         * Holds the key of the key event.
         */
        Key mKey;
    public:
    /**
      * Key event types.
      */
    enum
    {
        PRESSED = 0,
        RELEASED
    }

    this(Widget source, bool isShiftPressed, bool isControlPressed, bool isAltPressed, bool isMetaPressed, uint type, bool isNumericPad,Key key)
    {
        super(source,isShiftPressed,isControlPressed,isAltPressed,isMetaPressed);
        mType = type;
        mIsNumericPad = isNumericPad;
        mKey = key;
    }

    uint getType()
    {
        return mType;
    }

    bool isNumericPad()
    {
        return mIsNumericPad;
    }

    Key getKey()
    {
        return mKey;
    }

    mixin BindClass!("KeyEvent");
}

class MouseEvent : InputEvent
{
    this(Widget source, bool isShiftPressed, bool isControlPressed, bool isAltPressed, bool isMetaPressed, uint type, uint button, int x, int y, int clickCount)
    {
        super(source, isShiftPressed, isControlPressed, isAltPressed, isMetaPressed);
        mType = type;
        mButton = button;
        mX = x;
        mY = y;
        mClickCount = clickCount;
    }

    uint getButton()
    {
        return mButton;
    }

    int getX()
    {
        return mX;
    }

    int getY()
    {
        return mY;
    }

    void setX(int x)
    {
        mX = x;
    }

    void setY(int y)
    {
        mY = y;
    }

    int getClickCount()
    {
        return mClickCount;
    }

    uint getType()
    {
        return mType;
    }

    /**
      * Mouse event types.
      */
    enum
    {
        MOVED = 0,
        PRESSED,
        RELEASED,
        WHEEL_MOVED_DOWN,
        WHEEL_MOVED_UP,
        CLICKED,
        ENTERED,
        EXITED,
        DRAGGED

    }

    /**
      * Mouse button types.
      */
    enum
    {
        EMPTY = 0,
        LEFT,
        RIGHT,
        MIDDLE
    }

    mixin BindClass!("MouseEvent");

    protected:
        /**
         * Holds the type of the mouse event.
         */
        uint mType;

        /**
         * Holds the button of the mouse event.
         */
        uint mButton;

        /**
         * Holds the x-coordinate of the mouse event.
         */
        int mX;

        /**
         * Holds the y-coordinate of the mouse event.
         */
        int mY;

        /**
         * The number of clicks generated with the same button.
         * It's set to zero if another button is used.
         */
        int mClickCount;

}

class ActionEvent : Event
{
  private string mId;

    this(Widget source, string id)
    {
      super(source);
      mId = id;
    }

    string getId()
    {
        return mId;
    }

    mixin BindClass!("ActionEvent");
}

class SelectionEvent : Event
{
    this(Widget source)
    {
       super(source);
    }
}
