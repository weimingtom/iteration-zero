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

module guichan.graphics;

import guichan.rectangle;
import guichan.color;
import guichan.exception;
import guichan.font;
import guichan.image;
import guichan.iwidget;
enum Alignment
{
  LEFT,CENTER,RIGHT
}

class Graphics : IGraphics
{
//     /**
//       * Alignments for text drawing.
//       */
//     enum Alignment
//     {
//         LEFT = 0,
//         CENTER,
//         RIGHT
//     }

    this()
    {
        mFont = null;
    }

    abstract void _beginDraw();
    abstract void _endDraw();

    bool pushClipArea(Rectangle area)
    {
        if (mClipStack.length == 0)
        {
            ClipRectangle carea;
            carea.x = area.x;
            carea.y = area.y;
            carea.width = area.width;
            carea.height = area.height;
            carea.xOffset = area.x;
            carea.yOffset = area.y;
            mClipStack ~= carea;
            return true;
        }

        ClipRectangle top = mClipStack[$-1];
        ClipRectangle carea = ClipRectangle(area);
        carea.xOffset = top.xOffset + carea.x;
        carea.yOffset = top.yOffset + carea.y;
        carea.x += top.xOffset;
        carea.y += top.yOffset;

        // Clamp the pushed clip rectangle.
        if (carea.x < top.x)
        {
            carea.x = top.x;
        }
        
        if (carea.y < top.y)
        {
            carea.y = top.y;            
        }
                
        if (carea.x + carea.width > top.x + top.width)
        {
            carea.width = top.x + top.width - carea.x;

			if (carea.width < 0)
			{
				carea.width = 0;
			}
        }
        
        if (carea.y + carea.height > top.y + top.height)
        {
            carea.height = top.y + top.height - carea.y;

			if (carea.height < 0)
			{
				carea.height = 0;
			}
        }

        bool result = carea.isIntersecting(top);

        mClipStack ~= carea;

        return result;
    }

    void popClipArea()
    {

        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("Tried to pop clip area from empty stack.");
        }

        mClipStack.length = mClipStack.length - 1;
    }

    ClipRectangle getCurrentClipArea()
    {
        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("The clip area stack is empty.");
        }

        return mClipStack[$-1];
    }

    /**
      * Draws a part of an image.
      *
      * NOTE: Width and height arguments will not scale the image but
      *       specifies the size of the part to be drawn. If you want
      *       to draw the whole image there is a simplified version of
      *       this function.
      *
      * EXAMPLE: @code drawImage(myImage, 10, 10, 20, 20, 40, 40); @endcode
      *          Will draw a rectangular piece of myImage starting at
      *          coordinate (10, 10) in myImage, with width and height 40.
      *          The piece will be drawn with it's top left corner at
      *          coordinate (20, 20).
      *
      * @param image The image to draw.
      * @param srcX The source image x coordinate.
      * @param srcY The source image y coordinate.
      * @param dstX The destination x coordinate.
      * @param dstY The destination y coordinate.
      * @param width The width of the piece.
      * @param height The height of the piece.
      */
    abstract void drawImage(Image image, int srcX, int srcY, int dstX, int dstY, int width, int height);

    void drawImage(Image image, int dstX, int dstY)
    {
        drawImage(image, 0, 0, dstX, dstY, image.getWidth, image.getHeight);
    }

    void setFont(Font font)
    {
        mFont = font;
    }

    void drawText(dstring text, int x, int y, Alignment alignment)
    {
        if (mFont is null)
        {
            throw new GCN_Exception("No font set.");
        }

        with( Alignment )
        switch (alignment)
        {
          case LEFT:
              mFont.drawString(this, text, x, y);
              break;
          case CENTER:
              mFont.drawString(this, text, x - mFont.getWidth(text) / 2, y);
              break;
          case RIGHT:
              mFont.drawString(this, text, x - mFont.getWidth(text), y);
              break;
          default:
              throw new GCN_Exception("Unknown alignment.");
        }
    }

      /**
        * Draws a single point/pixel.
        *
        * @param x The x coordinate.
        * @param y The y coordinate.
        */
      abstract void drawPoint(int x, int y) ;

      /**
        * Ddraws a line.
        *
        * @param x1 The first x coordinate.
        * @param y1 The first y coordinate.
        * @param x2 The second x coordinate.
        * @param y2 The second y coordinate.
        */
      abstract void drawLine(int x1, int y1, int x2, int y2) ;

      /**
        * Draws a simple, non-filled, rectangle with a one pixel width.
        *
        * @param rectangle The rectangle to draw.
        */
      abstract void drawRectangle( Rectangle rectangle) ;

      /**
        * Draws a filled rectangle.
        *
        * @param rectangle The filled rectangle to draw.
        */
      abstract void fillRectangle( Rectangle rectangle) ;

      /**
        * Sets the color to use when drawing.
        *
        * @param color A color.
        * @see getColor
        */
      abstract void setColor( Color color) ;

      /**
        * Gets the color to use when drawing.
        *
        * @return The color used when drawing.
        * @see setColor
        */
      abstract  Color getColor()  ;

    protected:
        /**
         * Holds the clip area stack.
         */
        ClipRectangle[] mClipStack;

        /**
         * Holds the current font.
         */
        Font mFont;
}
