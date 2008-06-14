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

module guichan.key;

/**
  * Represents a key or a character.
  */
class Key
{
public:

    /**
      *ructor.
      *
      * @param value The ascii or enum value for the key.
      */
    this(int value = 0)
    {
      mValue = value;
    }

    bool isCharacter()
    {
        return (mValue >= 32 && mValue <= 126)
            || (mValue >= 162 && mValue <= 255)
            || (mValue == 9);
    }

    bool isNumber()
    {
        return mValue >= 48 && mValue <= 57;
    }

    bool isLetter()
    {
        return (((mValue >= 65 && mValue <= 90)
                 || (mValue >= 97 && mValue <= 122)
                 || (mValue >= 192 && mValue <= 255))
                && (mValue != 215) && (mValue != 247));
    }

    int getValue()
    {
        return mValue;
    }

    /**
      * An enum with key values.
      */
    enum
    {
        SPACE              = ' ',
        TAB                = '\t',
        ENTER              = '\n',
        LEFT_ALT           = 1000,
        RIGHT_ALT,
        LEFT_SHIFT,
        RIGHT_SHIFT,
        LEFT_CONTROL,
        RIGHT_CONTROL,
        LEFT_META,
        RIGHT_META,
        LEFT_SUPER,
        RIGHT_SUPER,
        INSERT,
        HOME,
        PAGE_UP,
        DELETE,
        END,
        PAGE_DOWN,
        ESCAPE,
        CAPS_LOCK,
        BACKSPACE,
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
        PRINT_SCREEN,
        SCROLL_LOCK,
        PAUSE,
        NUM_LOCK,
        ALT_GR,
        LEFT,
        RIGHT,
        UP,
        DOWN
    };

    /**
      * Holds the value of the key. It may be an ascii value
      * or an enum value.
      */
    int mValue;
}
