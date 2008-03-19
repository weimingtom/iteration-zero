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

module guichan.font;

import guichan.graphics;

    abstract class Font
    {
    public:

        /**
         * Gets the width of a string. The width of a string is not necesserily
         * the sum of all the widths of it's glyphs.
         *
         * @param text The string to return the width of.
         * @return The width of a string.
         */
        abstract int getWidth(dstring text);

        /**
         * Gets the height of the glyphs in the font.
         *
         * @return The height of the glyphs int the font.
         */
        abstract int getHeight();

        /**
         * Gets a string index in a string providing an x coordinate.
         * Used to retrive a string index (for a character in a
         * string) at a certain x position. It is especially useful
         * when a mouse clicks in a TextField and you want to know which
         * character was clicked.
         *
         * @return A string index in a string providing an x coordinate.
         */
        int getStringIndexAt(dstring text, int x, int y)
        {
            uint i;
            int size = 0;

            for (i = 0; i < text.length; ++i)
            {
                size = getWidth(text[0..i]);
                if (size > x)
                {
                    return i;
                }
            }
            return text.length;
        }

        /**
         * Draws a string.
         *
         * NOTE: You normally won't use this function to draw text since
         *       Graphics contains better functions for drawing text.
         *
         * @param graphics A Graphics object to use for drawing.
         * @param text The string to draw.
         * @param x The x coordinate where to draw the string.
         * @param y The y coordinate where to draw the string.
         */
        abstract void drawString(Graphics graphics, dstring text, int x, int y);

}
