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

module guichan.opengl.graphics;

// #if defined (_WIN32)
// #define WIN32_LEAN_AND_MEAN
// #include <windows.h>
// #endif
// 
// #if defined (__amigaos4__)
// #include <mgl/gl.h>
// #define glVertex3i glVertex3f
// #elif defined(__APPLE__)
// #include <OpenGL/gl.h>
// #else
// #include <GL/gl.h>
// #endif

import guichan.graphics;

import guichan.exception;
import guichan.image;
import guichan.color;
import guichan.rectangle;

import guichan.opengl.image;

import derelict.opengl.gl;

class OpenGLGraphics : Graphics
{
    this()
    {
        setTargetPlane(640, 480);
        mAlpha = false;
    }

    this(int width, int height)
    {
        setTargetPlane(width, height);
    }

    void _beginDraw()
    {
        glPushAttrib(
            GL_COLOR_BUFFER_BIT |
            GL_CURRENT_BIT |
            GL_DEPTH_BUFFER_BIT |
            GL_ENABLE_BIT |
            GL_FOG_BIT |
            GL_LIGHTING_BIT |
            GL_LINE_BIT |
            GL_POINT_BIT |
            GL_POLYGON_BIT |
            GL_SCISSOR_BIT |
            GL_STENCIL_BUFFER_BIT |
            GL_TEXTURE_BIT |
            GL_TRANSFORM_BIT |
            GL_POINT_BIT |
            GL_LINE_BIT
            );

        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glLoadIdentity();

        glMatrixMode(GL_TEXTURE);
        glPushMatrix();
        glLoadIdentity();

        glMatrixMode(GL_PROJECTION);
        glPushMatrix();
        glLoadIdentity();
        
        glOrtho(0.0,
                cast(double)mWidth,
                cast(double)mHeight,
                0.0,
                -1.0,
                1.0);

        glDisable(GL_LIGHTING);
        glDisable(GL_CULL_FACE);
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_TEXTURE_2D);

        glEnable(GL_SCISSOR_TEST);
        glPointSize(1.0);
        glLineWidth(1.0);

        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

        pushClipArea(new Rectangle(0, 0, mWidth, mHeight));
    }

    void _endDraw()
    {
        glMatrixMode(GL_MODELVIEW);
        glPopMatrix();

        glMatrixMode(GL_TEXTURE);
        glPopMatrix();

        glMatrixMode(GL_PROJECTION);
        glPopMatrix();

        glPopAttrib();

        popClipArea();
    }

    bool pushClipArea(Rectangle area)
    {
        bool result = super.pushClipArea(area);

        glScissor(mClipStack[0].x,
                  mHeight - mClipStack[0].y - mClipStack[0].height,
                  mClipStack[0].width,
                  mClipStack[0].height);

        return result;
    }

    void popClipArea()
    {
        super.popClipArea();

        if (mClipStack.length == 0)
        {
            return;
        }

        glScissor(mClipStack[0].x,
                  mHeight - mClipStack[0].y - mClipStack[0].height,
                  mClipStack[0].width,
                  mClipStack[0].height);
    }

    void setTargetPlane(int width, int height)
    {
        mWidth = width;
        mHeight = height;
    }

    void drawImage(Image image,
                                   int srcX,
                                   int srcY,
                                   int dstX,
                                   int dstY,
                                   int width,
                                   int height)
    {
        OpenGLImage srcImage = cast(OpenGLImage)(image);

        if (srcImage is null)
        {
            throw new GCN_Exception("Trying to draw an image of unknown format, must be an OpenGLImage.");
        }

        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("Clip stack is empty, perhaps you called a draw funtion outside of _beginDraw() and _endDraw()?");
        }

        ClipRectangle top = mClipStack[0];

        dstX += top.xOffset;
        dstY += top.yOffset;

        // Find OpenGL texture coordinates
        float texX1 = srcX / cast(float)srcImage.getTextureWidth();
        float texY1 = srcY / cast(float)srcImage.getTextureHeight();
        float texX2 = (srcX+width) / cast(float)srcImage.getTextureWidth();
        float texY2 = (srcY+height) / cast(float)srcImage.getTextureHeight();

        glBindTexture(GL_TEXTURE_2D, srcImage.getTextureHandle());
        glEnable(GL_TEXTURE_2D);

        // Check if blending already is enabled
        if (!mAlpha)
        {
            glEnable(GL_BLEND);
        }

        // Draw a textured quad -- the image
        glBegin(GL_QUADS);
        glTexCoord2f(texX1, texY1);
        glVertex3i(dstX, dstY, 0);

        glTexCoord2f(texX1, texY2);
        glVertex3i(dstX, dstY + height, 0);

        glTexCoord2f(texX2, texY2);
        glVertex3i(dstX + width, dstY + height, 0);

        glTexCoord2f(texX2, texY1);
        glVertex3i(dstX + width, dstY, 0);
        glEnd();
        glDisable(GL_TEXTURE_2D);

        // Don't disable blending if the color has alpha
        if (!mAlpha)
        {
            glDisable(GL_BLEND);
        }
    }

    void drawPoint(int x, int y)
    {
        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("Clip stack is empty, perhaps you called a draw funtion outside of _beginDraw() and _endDraw()?");
        }

        ClipRectangle top = mClipStack[0];

        x += top.xOffset;
        y += top.yOffset;

        glBegin(GL_POINTS);
        glVertex2i(x, y);
        glEnd();
    }

    void drawLine(int x1, int y1, int x2, int y2)
    {
        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("Clip stack is empty, perhaps you called a draw funtion outside of _beginDraw() and _endDraw()?");
        }

        ClipRectangle top = mClipStack[0];

        x1 += top.xOffset;
        y1 += top.yOffset;
        x2 += top.xOffset;
        y2 += top.yOffset;

        glBegin(GL_LINES);
        glVertex2f(x1 + 0.375f,
                   y1 + 0.375f);
        glVertex2f(x2 + 1.0f - 0.375f,
                   y2 + 1.0f - 0.375f);
        glEnd();

        glBegin(GL_POINTS);
        glVertex2f(x2 + 1.0f - 0.375f,
                   y2 + 1.0f - 0.375f);
        glEnd();

        glBegin(GL_POINTS);
        glVertex2f(x1 + 0.375f,
                   y1 + 0.375f);
        glEnd();

        
    }

    void drawRectangle(Rectangle rectangle)
    {
        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("Clip stack is empty, perhaps you called a draw funtion outside of _beginDraw() and _endDraw()?");
        }

        ClipRectangle top = mClipStack[0];
        
        glBegin(GL_LINE_LOOP);
        glVertex2f(rectangle.x + top.xOffset,
                   rectangle.y + top.yOffset);
        glVertex2f(rectangle.x + rectangle.width + top.xOffset - 1.0f,
                   rectangle.y + top.yOffset + 0.375f);
        glVertex2f(rectangle.x + rectangle.width + top.xOffset - 1.0f,
                   rectangle.y + rectangle.height + top.yOffset);
        glVertex2f(rectangle.x + top.xOffset,
                   rectangle.y + rectangle.height + top.yOffset);
        glEnd();
    }

    void fillRectangle(Rectangle rectangle)
    {
        if (mClipStack.length == 0)
        {
            throw new GCN_Exception("Clip stack is empty, perhaps you called a draw funtion outside of _beginDraw() and _endDraw()?");
        }

        ClipRectangle top = mClipStack[0];

        glBegin(GL_QUADS);
        glVertex2i(rectangle.x + top.xOffset,
                   rectangle.y + top.yOffset);
        glVertex2i(rectangle.x + rectangle.width + top.xOffset,
                   rectangle.y + top.yOffset);
        glVertex2i(rectangle.x + rectangle.width + top.xOffset,
                   rectangle.y + rectangle.height + top.yOffset);
        glVertex2i(rectangle.x + top.xOffset,
                   rectangle.y + rectangle.height + top.yOffset);
        glEnd();
    }

    void setColor(Color color)
    {
        mColor = color;
        glColor4ub(color.r, color.g, color.b, color.a);

        mAlpha = color.a != 255;

        if (mAlpha)
        {
            glEnable(GL_BLEND);
        }
    }

    Color getColor()
    {
        return mColor;
    }

    protected:
      int mWidth, mHeight;
      bool mAlpha;
      Color mColor;
}
