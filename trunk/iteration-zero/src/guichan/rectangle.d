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

module guichan.rectangle;

struct Rectangle
{
    int x,y,width,height;
    static Rectangle opCall()
    {
      Rectangle rect;
      return rect;
    }

    static Rectangle opCall(Rectangle r)
    {
      return Rectangle(r.x,r.y,r.width,r.height);
    }

    static Rectangle opCall(int x_, int y_, int width_, int height_)
    {
      Rectangle rect;
      return rect.set(x_,y_,width_,height_);
    }

    Rectangle set(int x_, int y_, int width_, int height_)
    {
        x = x_;
        y = y_;
        width = width_;
        height = height_;
        return *this;
    }

    bool isIntersecting(in Rectangle rectangle)
    {
        int x_ = x;
        int y_ = y;
        int width_ = width;
        int height_ = height;

        x_ -= rectangle.x;
        y_ -= rectangle.y;

        if (x_ < 0)
        {
            width_ += x_;
            x_ = 0;
        }
        else if (x_ + width_ > rectangle.width)
        {
            width_ = rectangle.width - x_;
        }

        if (y_ < 0)
        {
            height_ += y_;
            y_ = 0;
        }
        else if (y_ + height_ > rectangle.height)
        {
            height_ = rectangle.height - y_;
        }

        if (width_ <= 0 || height_ <= 0)
        {
            return false;
        }

        return true;
    }

    bool isPointInRect(int x_, int y_)
    {
        return x_ >= x
            && y_ >= y
            && x_ < x + width
            && y_ < y + height;
    }

//     std::ostream& operator<<(std::ostream& out,
//                              const Rectangle& rectangle)
//     {
//         out << "Rectangle [x = " << rectangle.x
//             << ", y = " << rectangle.y
//             << ", width = " << rectangle.width
//             << ", height = " << rectangle.height
//             << "]";
// 
//         return out;
//     }
}

struct ClipRectangle
{
    int x,y,width,height;
    int xOffset, yOffset;

    static ClipRectangle opCall(Rectangle other)
    {
        return ClipRectangle(other.x,other.y,other.width,other.height,0,0);
    }

    static ClipRectangle opCall()
    {
        ClipRectangle crect;
        return crect;
    }

    static ClipRectangle opCall(int x_, int y_, int width_, int height_, int xOffset_, int yOffset_)
    {
        ClipRectangle crect;
        return crect.set( x_,  y_,  width_,  height_,  xOffset_,  yOffset_);
    }

    ClipRectangle opAssign(in Rectangle other)
    {
        x = other.x;
        y = other.y;
        width = other.width;
        height = other.height;
        return *this;
    }

    ClipRectangle set(int x_, int y_, int width_, int height_, int xOffset_, int yOffset_)
    {
        x = x_;
        y = y_;
        width = width_;
        height = height_;
        xOffset = xOffset_;
        yOffset = yOffset_;
        return *this;
    }

    bool isIntersecting(T) (in T rectangle)
    {
        int x_ = x;
        int y_ = y;
        int width_ = width;
        int height_ = height;

        x_ -= rectangle.x;
        y_ -= rectangle.y;

        if (x_ < 0)
        {
            width_ += x_;
            x_ = 0;
        }
        else if (x_ + width_ > rectangle.width)
        {
            width_ = rectangle.width - x_;
        }

        if (y_ < 0)
        {
            height_ += y_;
            y_ = 0;
        }
        else if (y_ + height_ > rectangle.height)
        {
            height_ = rectangle.height - y_;
        }

        if (width_ <= 0 || height_ <= 0)
        {
            return false;
        }

        return true;
    }

    bool isPointInRect(int x_, int y_)
    {
        return x_ >= x
            && y_ >= y
            && x_ < x + width
            && y_ < y + height;
    }
}