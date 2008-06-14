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

module guichan.keyinput;
import guichan.key;

class KeyInput
{

  private:
  /**
    * Holds the key of the key input.
    */
  Key mKey;

  /**
    * Holds the type of the key input.
    */
  uint mType;

  /**
    * True if shift was pressed at the same time as the key,
    * false otherwise.
    */ 
  bool mShiftPressed;

  /**
    * True if control was pressed at the same time as the key,
    * false otherwise.
    */ 
  bool mControlPressed;

  /**
    * True if alt was pressed at the same time as the key,
    * false otherwise.
    */ 
  bool mAltPressed;

  /**
    * True if meta was pressed at the same time as the key,
    * false otherwise.
    */ 
  bool mMetaPressed;

  /**
    * True if the numeric pad was used when the key was pressed,
    * false otherwise.
    */ 
  bool mNumericPad;

  public:
    /**
      * Key input types. This enum corresponds to the enum with event
      * types on KeyEvent for easy mapping.
      */
    enum
    {
        PRESSED = 0,
        RELEASED
    }

    this()
    {
      mKey = new Key;
    }


    this(Key key, uint type)
    {
      mKey = (key);
      mType = (type);
      mShiftPressed= (false);
      mControlPressed= (false);
      mAltPressed= (false);
      mMetaPressed= (false);
      mNumericPad = (false);
    }

    void setType(uint type)
    {
        mType = type;
    }

    int getType()
    {
        return mType;
    }

    void setKey(Key key)
    {
        mKey = key;
    }

    Key getKey()
    {
        return mKey;
    }

    bool isShiftPressed()
    {
        return mShiftPressed;
    }

    void setShiftPressed(bool pressed)
    {
        mShiftPressed = pressed;
    }

    bool isControlPressed()
    {
        return mControlPressed;
    }

    void setControlPressed(bool pressed)
    {
        mControlPressed = pressed;
    }

    bool isAltPressed()
    {
        return mAltPressed;
    }

    void setAltPressed(bool pressed)
    {
        mAltPressed = pressed;
    }

    bool isMetaPressed()
    {
        return mMetaPressed;
    }

    void setMetaPressed(bool pressed)
    {
        mMetaPressed = pressed;
    }

    bool isNumericPad()
    {
        return mNumericPad;
    }

    void setNumericPad(bool numpad)
    {
        mNumericPad = numpad;
    }
}

