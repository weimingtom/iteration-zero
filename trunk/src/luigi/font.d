//---------------------------------------------------------------------
/*
 Copyright:

  luigi/font.d -- font support for 'luigi' user interface library.

  Copyright (C) 2006 William V. Baxter III

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  William Baxter wbaxter@gmail.com
*/

// The following copyright applies to some portions of this code below.
/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Thu Dec 16 1999
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */


module luigi.font;

import luigi.opengl;
import luigi.base;

import std.stdio : writefln;

//--------------------------------------------------------------------------
/** An abstract interface for any font rendering tech that wants to be used by Luigi */
interface Font
{
    /** Draw a character using OpenGL commands at position (0,0,0) in current 
        OpenGL coordinates.
        Return the number of units along the baseline to advance.
    */
    float draw_char(dchar c);

    /** Draw a string using OpenGL commands at position (0,0,0) in current 
        OpenGL coordinates.
        Return the number of units along the baseline to advance.
        A mixin is available to provide a default implementation of this based
        on draw_char.
    */
    float draw_string(char[] str);

    /** Return the bounding box of the string assuming it will be drawn at
        location (0,0).
        Thus the x,y positions tell you about the leading and descender.
                      +--------------------+
                      |       <--w-->      | ^
                      |                    | |
             (0,0)    |                    | h
               +------|--------------------|-|----
                      |(x,y)               | v
                      +--------------------+
    */
    Rect string_rect(char[] str);

    /** Return the basic height of the font */
    float height();

    /** Return width if font is fixed-width, or -1 if the font is proportional */
    float width();

    /** Returns the location of the origin relative to a character's bounding box
     *  top-left corner.  Below this would be something like (ox,oy)==(-5,7).
     *  Put another way, in order for the top left corner to appear at some (x,y)
     *  you should call draw_string with the coordinates set to (x+ox, y+oy)

                 (0,0)+------------+
                      |            |
                      |            |
             (ox,oy)  |            |
               +------|------------|----
                      |            |
                      +------------+
    */                       
    Point origin();

}





// Basic glbitmap font ported from OpenGLUT (GLUT_BITMAP_HELVETICA_12)
class ASCIIBitmapFont : Font
{
    this() {
        m_data = &helvetica_12_data;
    }

    float draw_char(dchar c) {

        if( !m_data || ( 0 > c ) || (255 < c ) )
            return 0;

        alias m_data f;

        /* Find the glyph we want to draw. */
        GLubyte *glyph = f.characters[ c ];

        glPushClientAttrib( GL_CLIENT_PIXEL_STORE_BIT );

        glPixelStorei( GL_UNPACK_SWAP_BYTES,  GL_FALSE );
        glPixelStorei( GL_UNPACK_LSB_FIRST,   GL_FALSE );
        glPixelStorei( GL_UNPACK_ROW_LENGTH,  0        );
        glPixelStorei( GL_UNPACK_SKIP_ROWS,   0        );
        glPixelStorei( GL_UNPACK_SKIP_PIXELS, 0        );
        glPixelStorei( GL_UNPACK_ALIGNMENT,   1        );
        
        _update_raster_pos();

        glBitmap(
            glyph[ 0 ], f.height,       /* The bitmap's width and height  */
            f.xorig, f.yorig,           /* The origin in the font glyph   */
            glyph[0], 0.0,              /* The raster advance -- inc. x,y */
            &glyph[1]                   /* The packed bitmap data...      */
            );
        glPopClientAttrib( );
        return glyph[0];
    }

    float draw_string(char[] str) {
        float totalw = 0;
        foreach(dchar c; str) {
            float dx = draw_char(c);
            totalw += dx;
            glTranslatef(dx,0,0);
        }
        glTranslatef(-totalw,0,0);
        return totalw;
    }
    
    Rect string_rect(char [] str) {
        Rect r; r.set(0,0,0,0);
        r.h = m_data.height;
        r.y = m_data.yorig;
        r.x = m_data.xorig;
        foreach(dchar c; str) {
            if (0<=c && 255>=c) {
                r.w += m_data.characters[c][0];
            }
        }
        return r;
    }
    
    float height() {
        return m_data.height;
    }

    /** Return if the font is proportional (vs fixed width) */
    float width() {
        return m_data.width;
    }

    Point origin() { return Point(-m_data.xorig,-m_data.yorig+m_data.height); }

    /** Set raster pos to the current gl 0,0,0 point */
    void _update_raster_pos()
    {
        // GL maintains this separately, but we just use the current 0,0 point
        glRasterPos2f(0,0);
    }

    BitmapFontData *m_data = null;
}

static GLubyte Helvetica12_Character_000[33] = [cast(GLubyte)9,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_001[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_002[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_003[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_004[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_005[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_006[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_007[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_008[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_009[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_010[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_011[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_012[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_013[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_014[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_015[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_016[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_017[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_018[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_019[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_020[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_021[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_022[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_023[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_024[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_025[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_026[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_027[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_028[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_029[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_030[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_031[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_032[17] = [  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_033[17] = [  3,  0,  0,  0,  0, 64,  0, 64, 64, 64, 64, 64, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_034[17] = [  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 80, 80, 80,  0,  0,  0];
static GLubyte Helvetica12_Character_035[17] = [  7,  0,  0,  0,  0, 80, 80, 80,252, 40,252, 40, 40,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_036[17] = [  7,  0,  0,  0, 16, 56, 84, 84, 20, 56, 80, 84, 56, 16,  0,  0,  0];
static GLubyte Helvetica12_Character_037[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 17,128, 10, 64, 10, 64,  9,128,  4,  0, 52,  0, 74,  0, 74,  0, 49,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_038[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 57,  0, 70,  0, 66,  0, 69,  0, 40,  0, 24,  0, 36,  0, 36,  0, 24,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_039[17] = [  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 64, 32, 96,  0,  0,  0];
static GLubyte Helvetica12_Character_040[17] = [  4,  0, 16, 32, 32, 64, 64, 64, 64, 64, 64, 32, 32, 16,  0,  0,  0];
static GLubyte Helvetica12_Character_041[17] = [  4,  0,128, 64, 64, 32, 32, 32, 32, 32, 32, 64, 64,128,  0,  0,  0];
static GLubyte Helvetica12_Character_042[17] = [  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 80, 32, 80,  0,  0,  0];
static GLubyte Helvetica12_Character_043[17] = [  7,  0,  0,  0,  0,  0, 16, 16,124, 16, 16,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_044[33] = [  4,  0,  0, 64, 32, 32,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_045[33] = [  8,  0,  0,  0,  0,  0,  0,  0,124,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_046[33] = [  3,  0,  0,  0,  0, 64,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_047[33] = [  4,  0,  0,  0,  0,128,128, 64, 64, 64, 32, 32, 16, 16,  0,  0,  0];
static GLubyte Helvetica12_Character_048[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 68, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_049[33] = [  7,  0,  0,  0,  0, 16, 16, 16, 16, 16, 16, 16,112, 16,  0,  0,  0];
static GLubyte Helvetica12_Character_050[33] = [  7,  0,  0,  0,  0,124, 64, 64, 32, 16,  8,  4, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_051[33] = [  7,  0,  0,  0,  0, 56, 68, 68,  4,  4, 24,  4, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_052[33] = [  7,  0,  0,  0,  0,  8,  8,252,136, 72, 40, 40, 24,  8,  0,  0,  0];
static GLubyte Helvetica12_Character_053[33] = [  7,  0,  0,  0,  0, 56, 68, 68,  4,  4,120, 64, 64,124,  0,  0,  0];
static GLubyte Helvetica12_Character_054[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68,100, 88, 64, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_055[33] = [  7,  0,  0,  0,  0, 32, 32, 16, 16, 16,  8,  8,  4,124,  0,  0,  0];
static GLubyte Helvetica12_Character_056[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 56, 68, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_057[33] = [  7,  0,  0,  0,  0, 56, 68,  4,  4, 60, 68, 68, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_058[33] = [  3,  0,  0,  0,  0, 64,  0,  0,  0,  0, 64,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_059[33] = [  3,  0,  0,128, 64, 64,  0,  0,  0,  0, 64,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_060[33] = [  7,  0,  0,  0,  0,  0, 12, 48,192, 48, 12,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_061[33] = [  7,  0,  0,  0,  0,  0,  0,124,  0,124,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_062[33] = [  7,  0,  0,  0,  0,  0, 96, 24,  6, 24, 96,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_063[33] = [  7,  0,  0,  0,  0, 16,  0, 16, 16,  8,  8, 68, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_064[33] = [ 12,  0,  0,  0,  0,  0,  0, 31,  0, 32,  0, 77,128, 83, 64, 81, 32, 81, 32, 73, 32, 38,160, 48, 64, 15,128,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_065[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0, 20,  0,  8,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_066[33] = [  8,  0,  0,  0,  0,124, 66, 66, 66,124, 66, 66, 66,124,  0,  0,  0];
static GLubyte Helvetica12_Character_067[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,  0, 64,  0, 64,  0, 64,  0, 64,  0, 33,  0, 30,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_068[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0,124,  0, 66,  0, 65,  0, 65,  0, 65,  0, 65,  0, 65,  0, 66,  0,124,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_069[33] = [  8,  0,  0,  0,  0,126, 64, 64, 64,126, 64, 64, 64,126,  0,  0,  0];
static GLubyte Helvetica12_Character_070[33] = [  8,  0,  0,  0,  0, 64, 64, 64, 64,124, 64, 64, 64,126,  0,  0,  0];
static GLubyte Helvetica12_Character_071[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 29,  0, 35,  0, 65,  0, 65,  0, 71,  0, 64,  0, 64,  0, 33,  0, 30,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_072[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 65,  0,127,  0, 65,  0, 65,  0, 65,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_073[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_074[33] = [  7,  0,  0,  0,  0, 56, 68, 68,  4,  4,  4,  4,  4,  4,  0,  0,  0];
static GLubyte Helvetica12_Character_075[33] = [  8,  0,  0,  0,  0, 65, 66, 68, 72,112, 80, 72, 68, 66,  0,  0,  0];
static GLubyte Helvetica12_Character_076[33] = [  7,  0,  0,  0,  0,124, 64, 64, 64, 64, 64, 64, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_077[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 68, 64, 68, 64, 74, 64, 74, 64, 81, 64, 81, 64, 96,192, 96,192, 64, 64,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_078[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 67,  0, 69,  0, 69,  0, 73,  0, 81,  0, 81,  0, 97,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_079[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,128, 64,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_080[33] = [  8,  0,  0,  0,  0, 64, 64, 64, 64,124, 66, 66, 66,124,  0,  0,  0];
static GLubyte Helvetica12_Character_081[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,128, 33,  0, 66,128, 68,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_082[33] = [  8,  0,  0,  0,  0, 66, 66, 66, 68,124, 66, 66, 66,124,  0,  0,  0];
static GLubyte Helvetica12_Character_083[33] = [  8,  0,  0,  0,  0, 60, 66, 66,  2, 12, 48, 64, 66, 60,  0,  0,  0];
static GLubyte Helvetica12_Character_084[33] = [  7,  0,  0,  0,  0, 16, 16, 16, 16, 16, 16, 16, 16,254,  0,  0,  0];
static GLubyte Helvetica12_Character_085[33] = [  8,  0,  0,  0,  0, 60, 66, 66, 66, 66, 66, 66, 66, 66,  0,  0,  0];
static GLubyte Helvetica12_Character_086[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0,  8,  0,  8,  0, 20,  0, 20,  0, 34,  0, 34,  0, 34,  0, 65,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_087[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 17,  0, 17,  0, 17,  0, 42,128, 42,128, 36,128, 68, 64, 68, 64, 68, 64,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_088[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 34,  0, 34,  0, 20,  0,  8,  0, 20,  0, 34,  0, 34,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_089[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0,  8,  0,  8,  0,  8,  0,  8,  0, 20,  0, 34,  0, 34,  0, 65,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_090[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0,127,  0, 64,  0, 32,  0, 16,  0,  8,  0,  4,  0,  2,  0,  1,  0,127,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_091[33] = [  3,  0, 96, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 96,  0,  0,  0];
static GLubyte Helvetica12_Character_092[33] = [  4,  0,  0,  0,  0, 16, 16, 32, 32, 32, 64, 64,128,128,  0,  0,  0];
static GLubyte Helvetica12_Character_093[33] = [  3,  0,192, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,192,  0,  0,  0];
static GLubyte Helvetica12_Character_094[33] = [  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,136, 80, 32,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_095[33] = [  7,  0,  0,254,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_096[33] = [  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,192,128, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_097[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_098[33] = [  7,  0,  0,  0,  0, 88,100, 68, 68, 68,100, 88, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_099[33] = [  7,  0,  0,  0,  0, 56, 68, 64, 64, 64, 68, 56,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_100[33] = [  7,  0,  0,  0,  0, 52, 76, 68, 68, 68, 76, 52,  4,  4,  0,  0,  0];
static GLubyte Helvetica12_Character_101[33] = [  7,  0,  0,  0,  0, 56, 68, 64,124, 68, 68, 56,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_102[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64,224, 64, 48,  0,  0,  0];
static GLubyte Helvetica12_Character_103[33] = [  7,  0, 56, 68,  4, 52, 76, 68, 68, 68, 76, 52,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_104[33] = [  7,  0,  0,  0,  0, 68, 68, 68, 68, 68,100, 88, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_105[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64,  0, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_106[33] = [  3,  0,128, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_107[33] = [  6,  0,  0,  0,  0, 68, 72, 80, 96, 96, 80, 72, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_108[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_109[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 73,  0, 73,  0, 73,  0, 73,  0, 73,  0,109,  0, 82,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_110[33] = [  7,  0,  0,  0,  0, 68, 68, 68, 68, 68,100, 88,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_111[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 56,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_112[33] = [  7,  0, 64, 64, 64, 88,100, 68, 68, 68,100, 88,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_113[33] = [  7,  0,  4,  4,  4, 52, 76, 68, 68, 68, 76, 52,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_114[33] = [  4,  0,  0,  0,  0, 64, 64, 64, 64, 64, 96, 80,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_115[33] = [  6,  0,  0,  0,  0, 48, 72,  8, 48, 64, 72, 48,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_116[33] = [  3,  0,  0,  0,  0, 96, 64, 64, 64, 64, 64,224, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_117[33] = [  7,  0,  0,  0,  0, 52, 76, 68, 68, 68, 68, 68,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_118[33] = [  7,  0,  0,  0,  0, 16, 16, 40, 40, 68, 68, 68,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_119[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 34,  0, 34,  0, 85,  0, 73,  0, 73,  0,136,128,136,128,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_120[33] = [  6,  0,  0,  0,  0,132,132, 72, 48, 48, 72,132,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_121[33] = [  7,  0, 64, 32, 16, 16, 40, 40, 72, 68, 68, 68,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_122[33] = [  6,  0,  0,  0,  0,120, 64, 32, 32, 16,  8,120,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_123[33] = [  4,  0, 48, 64, 64, 64, 64, 64,128, 64, 64, 64, 64, 48,  0,  0,  0];
static GLubyte Helvetica12_Character_124[33] = [  3,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_125[33] = [  4,  0,192, 32, 32, 32, 32, 32, 16, 32, 32, 32, 32,192,  0,  0,  0];
static GLubyte Helvetica12_Character_126[33] = [  7,  0,  0,  0,  0,  0,  0,  0,152,100,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_127[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_128[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_129[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_130[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_131[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_132[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_133[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_134[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_135[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_136[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_137[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_138[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_139[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_140[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_141[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_142[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_143[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_144[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_145[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_146[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_147[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_148[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_149[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_150[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_151[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_152[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_153[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_154[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_155[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_156[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_157[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_158[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_159[33] = [ 12,  0,  0,  0,  0,  0,  0,  0,  0, 85,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 65,  0,  0,  0, 85,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_160[33] = [  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_161[33] = [  3,  0, 64, 64, 64, 64, 64, 64, 64, 64,  0, 64,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_162[33] = [  7,  0,  0,  0, 32, 56,100, 80, 80, 80, 84, 56,  8,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_163[33] = [  7,  0,  0,  0,  0, 88, 36, 16, 16,120, 32, 32, 36, 24,  0,  0,  0];
static GLubyte Helvetica12_Character_164[33] = [  7,  0,  0,  0,  0,  0,132,120, 72, 72,120,132,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_165[33] = [  7,  0,  0,  0,  0, 16, 16,124, 16,124, 16, 40, 68, 68,  0,  0,  0];
static GLubyte Helvetica12_Character_166[33] = [  3,  0,  0, 64, 64, 64, 64,  0,  0,  0, 64, 64, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_167[33] = [  6,  0,112,136,  8, 48, 72,136,136,144, 96,128,136,112,  0,  0,  0];
static GLubyte Helvetica12_Character_168[33] = [  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,160,  0,  0,  0];
static GLubyte Helvetica12_Character_169[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 31,  0, 32,128, 78, 64, 81, 64, 80, 64, 81, 64, 78, 64, 32,128, 31,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_170[33] = [  5,  0,  0,  0,  0,  0,  0,  0,  0,112,  0, 80, 16,112,  0,  0,  0];
static GLubyte Helvetica12_Character_171[33] = [  7,  0,  0,  0,  0,  0, 20, 40, 80, 40, 20,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_172[33] = [  8,  0,  0,  0,  0,  0,  0,  2,  2,  2,126,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_173[33] = [  5,  0,  0,  0,  0,  0,  0,  0,240,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_174[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 31,  0, 32,128, 74, 64, 74, 64, 76, 64, 74, 64, 78, 64, 32,128, 31,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_175[33] = [  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,240,  0,  0,  0];
static GLubyte Helvetica12_Character_176[33] = [  5,  0,  0,  0,  0,  0,  0,  0,  0, 96,144,144, 96,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_177[33] = [  7,  0,  0,  0,  0,124,  0, 16, 16,124, 16, 16,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_178[33] = [  4,  0,  0,  0,  0,  0,  0,  0,240, 64, 32,144, 96,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_179[33] = [  4,  0,  0,  0,  0,  0,  0,  0,192, 32, 64, 32,224,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_180[33] = [  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,128, 64,  0,  0];
static GLubyte Helvetica12_Character_181[33] = [  7,  0, 64, 64, 64,116, 76, 68, 68, 68, 68, 68,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_182[33] = [  7,  0, 40, 40, 40, 40, 40, 40,104,232,232,232,104, 60,  0,  0,  0];
static GLubyte Helvetica12_Character_183[33] = [  3,  0,  0,  0,  0,  0,  0,  0, 64,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_184[33] = [  3,  0,192, 32, 32, 64,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_185[33] = [  4,  0,  0,  0,  0,  0,  0,  0, 32, 32, 32, 96, 32,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_186[33] = [  5,  0,  0,  0,  0,  0,  0,  0,  0,112,  0,112, 80,112,  0,  0,  0];
static GLubyte Helvetica12_Character_187[33] = [  7,  0,  0,  0,  0,  0, 80, 40, 20, 40, 80,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_188[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 39,128, 21,  0, 19,  0, 73,  0, 68,  0, 68,  0,194,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_189[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 71,128, 34,  0, 17,  0, 20,128, 75,  0, 72,  0, 68,  0,194,  0, 65,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_190[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 33,  0, 23,128, 21,  0, 11,  0,201,  0, 36,  0, 68,  0, 34,  0,225,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_191[33] = [  7,  0, 56, 68, 68, 32, 32, 16, 16,  0, 16,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_192[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0,  8,  0,  8,  0,  0,  0,  8,  0, 16,  0];
static GLubyte Helvetica12_Character_193[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0,  8,  0,  8,  0,  0,  0,  8,  0,  4,  0];
static GLubyte Helvetica12_Character_194[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0,  8,  0,  8,  0,  0,  0, 20,  0,  8,  0];
static GLubyte Helvetica12_Character_195[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0,  8,  0,  8,  0,  0,  0, 20,  0, 10,  0];
static GLubyte Helvetica12_Character_196[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0,  8,  0,  8,  0,  0,  0, 20,  0,  0,  0];
static GLubyte Helvetica12_Character_197[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 65,  0, 65,  0, 62,  0, 34,  0, 34,  0, 20,  0,  8,  0,  8,  0,  8,  0, 20,  0,  8,  0];
static GLubyte Helvetica12_Character_198[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 71,192, 68,  0, 68,  0, 60,  0, 39,192, 36,  0, 20,  0, 20,  0, 15,192,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_199[33] = [  9,  0,  0, 24,  0,  4,  0,  4,  0, 30,  0, 33,  0, 64,  0, 64,  0, 64,  0, 64,  0, 64,  0, 33,  0, 30,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_200[33] = [  8,  0,  0,  0,  0,126, 64, 64, 64,126, 64, 64, 64,126,  0,  8, 16];
static GLubyte Helvetica12_Character_201[33] = [  8,  0,  0,  0,  0,126, 64, 64, 64,126, 64, 64, 64,126,  0,  8,  4];
static GLubyte Helvetica12_Character_202[33] = [  8,  0,  0,  0,  0,126, 64, 64, 64,126, 64, 64, 64,126,  0, 20,  8];
static GLubyte Helvetica12_Character_203[33] = [  8,  0,  0,  0,  0,126, 64, 64, 64,126, 64, 64, 64,126,  0, 20,  0];
static GLubyte Helvetica12_Character_204[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0, 64,128];
static GLubyte Helvetica12_Character_205[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0, 64, 32];
static GLubyte Helvetica12_Character_206[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0,160, 64];
static GLubyte Helvetica12_Character_207[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64, 64, 64,  0,160,  0];
static GLubyte Helvetica12_Character_208[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0,124,  0, 66,  0, 65,  0, 65,  0,241,  0, 65,  0, 65,  0, 66,  0,124,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_209[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0, 65,  0, 67,  0, 69,  0, 69,  0, 73,  0, 81,  0, 81,  0, 97,  0, 65,  0,  0,  0, 20,  0, 10,  0];
static GLubyte Helvetica12_Character_210[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,128, 64,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0,  4,  0,  8,  0];
static GLubyte Helvetica12_Character_211[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,128, 64,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0,  4,  0,  2,  0];
static GLubyte Helvetica12_Character_212[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,128, 64,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0, 10,  0,  4,  0];
static GLubyte Helvetica12_Character_213[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,128, 64,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0, 20,  0, 10,  0];
static GLubyte Helvetica12_Character_214[33] = [ 10,  0,  0,  0,  0,  0,  0,  0,  0, 30,  0, 33,  0, 64,128, 64,128, 64,128, 64,128, 64,128, 33,  0, 30,  0,  0,  0, 18,  0,  0,  0];
static GLubyte Helvetica12_Character_215[33] = [  7,  0,  0,  0,  0,  0, 68, 40, 16, 40, 68,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_216[33] = [ 10,  0,  0,  0,  0,  0,  0,128,  0, 94,  0, 33,  0, 80,128, 72,128, 68,128, 68,128, 66,128, 33,  0, 30,128,  0, 64,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_217[33] = [  8,  0,  0,  0,  0, 60, 66, 66, 66, 66, 66, 66, 66, 66,  0,  8, 16];
static GLubyte Helvetica12_Character_218[33] = [  8,  0,  0,  0,  0, 60, 66, 66, 66, 66, 66, 66, 66, 66,  0,  8,  4];
static GLubyte Helvetica12_Character_219[33] = [  8,  0,  0,  0,  0, 60, 66, 66, 66, 66, 66, 66, 66, 66,  0, 20,  8];
static GLubyte Helvetica12_Character_220[33] = [  8,  0,  0,  0,  0, 60, 66, 66, 66, 66, 66, 66, 66, 66,  0, 36,  0];
static GLubyte Helvetica12_Character_221[33] = [  9,  0,  0,  0,  0,  0,  0,  0,  0,  8,  0,  8,  0,  8,  0,  8,  0, 20,  0, 34,  0, 34,  0, 65,  0, 65,  0,  0,  0,  8,  0,  4,  0];
static GLubyte Helvetica12_Character_222[33] = [  8,  0,  0,  0,  0, 64, 64,124, 66, 66, 66,124, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_223[33] = [  7,  0,  0,  0,  0, 88, 68, 68, 68, 88, 68, 68, 68, 56,  0,  0,  0];
static GLubyte Helvetica12_Character_224[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56,  0,  8, 16,  0,  0];
static GLubyte Helvetica12_Character_225[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56,  0, 16,  8,  0,  0];
static GLubyte Helvetica12_Character_226[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56,  0, 40, 16,  0,  0];
static GLubyte Helvetica12_Character_227[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56,  0, 40, 20,  0,  0];
static GLubyte Helvetica12_Character_228[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56,  0, 40,  0,  0,  0];
static GLubyte Helvetica12_Character_229[33] = [  7,  0,  0,  0,  0, 58, 68, 68, 60,  4, 68, 56, 24, 36, 24,  0,  0];
static GLubyte Helvetica12_Character_230[33] = [ 11,  0,  0,  0,  0,  0,  0,  0,  0, 59,128, 68, 64, 68,  0, 63,192,  4, 64, 68, 64, 59,128,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_231[33] = [  7,  0, 48,  8, 16, 56, 68, 64, 64, 64, 68, 56,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_232[33] = [  7,  0,  0,  0,  0, 56, 68, 64,124, 68, 68, 56,  0, 16, 32,  0,  0];
static GLubyte Helvetica12_Character_233[33] = [  7,  0,  0,  0,  0, 56, 68, 64,124, 68, 68, 56,  0, 16,  8,  0,  0];
static GLubyte Helvetica12_Character_234[33] = [  7,  0,  0,  0,  0, 56, 68, 64,124, 68, 68, 56,  0, 40, 16,  0,  0];
static GLubyte Helvetica12_Character_235[33] = [  7,  0,  0,  0,  0, 56, 68, 64,124, 68, 68, 56,  0, 40,  0,  0,  0];
static GLubyte Helvetica12_Character_236[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64,  0, 64,128,  0,  0];
static GLubyte Helvetica12_Character_237[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64,  0, 64, 32,  0,  0];
static GLubyte Helvetica12_Character_238[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64,  0,160, 64,  0,  0];
static GLubyte Helvetica12_Character_239[33] = [  3,  0,  0,  0,  0, 64, 64, 64, 64, 64, 64, 64,  0,160,  0,  0,  0];
static GLubyte Helvetica12_Character_240[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 60,  4, 40, 24, 52,  0,  0];
static GLubyte Helvetica12_Character_241[33] = [  7,  0,  0,  0,  0, 68, 68, 68, 68, 68,100, 88,  0, 40, 20,  0,  0];
static GLubyte Helvetica12_Character_242[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 56,  0, 16, 32,  0,  0];
static GLubyte Helvetica12_Character_243[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 56,  0, 16,  8,  0,  0];
static GLubyte Helvetica12_Character_244[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 56,  0, 40, 16,  0,  0];
static GLubyte Helvetica12_Character_245[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 56,  0, 40, 20,  0,  0];
static GLubyte Helvetica12_Character_246[33] = [  7,  0,  0,  0,  0, 56, 68, 68, 68, 68, 68, 56,  0, 40,  0,  0,  0];
static GLubyte Helvetica12_Character_247[33] = [  7,  0,  0,  0,  0,  0, 16,  0,124,  0, 16,  0,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_248[33] = [  7,  0,  0,  0,  0,184, 68,100, 84, 76, 68, 58,  0,  0,  0,  0,  0];
static GLubyte Helvetica12_Character_249[33] = [  7,  0,  0,  0,  0, 52, 76, 68, 68, 68, 68, 68,  0, 16, 32,  0,  0];
static GLubyte Helvetica12_Character_250[33] = [  7,  0,  0,  0,  0, 52, 76, 68, 68, 68, 68, 68,  0, 16,  8,  0,  0];
static GLubyte Helvetica12_Character_251[33] = [  7,  0,  0,  0,  0, 52, 76, 68, 68, 68, 68, 68,  0, 40, 16,  0,  0];
static GLubyte Helvetica12_Character_252[33] = [  7,  0,  0,  0,  0, 52, 76, 68, 68, 68, 68, 68,  0, 40,  0,  0,  0];
static GLubyte Helvetica12_Character_253[33] = [  7,  0, 64, 32, 16, 16, 40, 40, 72, 68, 68, 68,  0, 16,  8,  0,  0];
static GLubyte Helvetica12_Character_254[33] = [  7,  0, 64, 64, 64, 88,100, 68, 68, 68,100, 88, 64, 64,  0,  0,  0];
static GLubyte Helvetica12_Character_255[33] = [  7,  0, 96, 16, 16, 16, 24, 40, 40, 36, 68, 68,  0, 40,  0,  0,  0];

/* The font characters mapping: */
static GLubyte*[256] Helvetica12_Character_Map = [
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_033.ptr,
    Helvetica12_Character_034.ptr, Helvetica12_Character_035.ptr,
    Helvetica12_Character_036.ptr, Helvetica12_Character_037.ptr,
    Helvetica12_Character_038.ptr, Helvetica12_Character_039.ptr,
    Helvetica12_Character_040.ptr, Helvetica12_Character_041.ptr,
    Helvetica12_Character_042.ptr, Helvetica12_Character_043.ptr,
    Helvetica12_Character_044.ptr, Helvetica12_Character_045.ptr,
    Helvetica12_Character_046.ptr, Helvetica12_Character_047.ptr,
    Helvetica12_Character_048.ptr, Helvetica12_Character_049.ptr,
    Helvetica12_Character_050.ptr, Helvetica12_Character_051.ptr,
    Helvetica12_Character_052.ptr, Helvetica12_Character_053.ptr,
    Helvetica12_Character_054.ptr, Helvetica12_Character_055.ptr,
    Helvetica12_Character_056.ptr, Helvetica12_Character_057.ptr,
    Helvetica12_Character_058.ptr, Helvetica12_Character_059.ptr,
    Helvetica12_Character_060.ptr, Helvetica12_Character_061.ptr,
    Helvetica12_Character_062.ptr, Helvetica12_Character_063.ptr,
    Helvetica12_Character_064.ptr, Helvetica12_Character_065.ptr,
    Helvetica12_Character_066.ptr, Helvetica12_Character_067.ptr,
    Helvetica12_Character_068.ptr, Helvetica12_Character_069.ptr,
    Helvetica12_Character_070.ptr, Helvetica12_Character_071.ptr,
    Helvetica12_Character_072.ptr, Helvetica12_Character_073.ptr,
    Helvetica12_Character_074.ptr, Helvetica12_Character_075.ptr,
    Helvetica12_Character_076.ptr, Helvetica12_Character_077.ptr,
    Helvetica12_Character_078.ptr, Helvetica12_Character_079.ptr,
    Helvetica12_Character_080.ptr, Helvetica12_Character_081.ptr,
    Helvetica12_Character_082.ptr, Helvetica12_Character_083.ptr,
    Helvetica12_Character_084.ptr, Helvetica12_Character_085.ptr,
    Helvetica12_Character_086.ptr, Helvetica12_Character_087.ptr,
    Helvetica12_Character_088.ptr, Helvetica12_Character_089.ptr,
    Helvetica12_Character_090.ptr, Helvetica12_Character_091.ptr,
    Helvetica12_Character_092.ptr, Helvetica12_Character_093.ptr,
    Helvetica12_Character_094.ptr, Helvetica12_Character_095.ptr,
    Helvetica12_Character_096.ptr, Helvetica12_Character_097.ptr,
    Helvetica12_Character_098.ptr, Helvetica12_Character_099.ptr,
    Helvetica12_Character_100.ptr, Helvetica12_Character_101.ptr,
    Helvetica12_Character_102.ptr, Helvetica12_Character_103.ptr,
    Helvetica12_Character_104.ptr, Helvetica12_Character_105.ptr,
    Helvetica12_Character_106.ptr, Helvetica12_Character_107.ptr,
    Helvetica12_Character_108.ptr, Helvetica12_Character_109.ptr,
    Helvetica12_Character_110.ptr, Helvetica12_Character_111.ptr,
    Helvetica12_Character_112.ptr, Helvetica12_Character_113.ptr,
    Helvetica12_Character_114.ptr, Helvetica12_Character_115.ptr,
    Helvetica12_Character_116.ptr, Helvetica12_Character_117.ptr,
    Helvetica12_Character_118.ptr, Helvetica12_Character_119.ptr,
    Helvetica12_Character_120.ptr, Helvetica12_Character_121.ptr,
    Helvetica12_Character_122.ptr, Helvetica12_Character_123.ptr,
    Helvetica12_Character_124.ptr, Helvetica12_Character_125.ptr,
    Helvetica12_Character_126.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_032.ptr, Helvetica12_Character_032.ptr,
    Helvetica12_Character_160.ptr, Helvetica12_Character_161.ptr,
    Helvetica12_Character_162.ptr, Helvetica12_Character_163.ptr,
    Helvetica12_Character_164.ptr, Helvetica12_Character_165.ptr,
    Helvetica12_Character_166.ptr, Helvetica12_Character_167.ptr,
    Helvetica12_Character_168.ptr, Helvetica12_Character_169.ptr,
    Helvetica12_Character_170.ptr, Helvetica12_Character_171.ptr,
    Helvetica12_Character_172.ptr, Helvetica12_Character_173.ptr,
    Helvetica12_Character_174.ptr, Helvetica12_Character_175.ptr,
    Helvetica12_Character_176.ptr, Helvetica12_Character_177.ptr,
    Helvetica12_Character_178.ptr, Helvetica12_Character_179.ptr,
    Helvetica12_Character_180.ptr, Helvetica12_Character_181.ptr,
    Helvetica12_Character_182.ptr, Helvetica12_Character_183.ptr,
    Helvetica12_Character_184.ptr, Helvetica12_Character_185.ptr,
    Helvetica12_Character_186.ptr, Helvetica12_Character_187.ptr,
    Helvetica12_Character_188.ptr, Helvetica12_Character_189.ptr,
    Helvetica12_Character_190.ptr, Helvetica12_Character_191.ptr,
    Helvetica12_Character_192.ptr, Helvetica12_Character_193.ptr,
    Helvetica12_Character_194.ptr, Helvetica12_Character_195.ptr,
    Helvetica12_Character_196.ptr, Helvetica12_Character_197.ptr,
    Helvetica12_Character_198.ptr, Helvetica12_Character_199.ptr,
    Helvetica12_Character_200.ptr, Helvetica12_Character_201.ptr,
    Helvetica12_Character_202.ptr, Helvetica12_Character_203.ptr,
    Helvetica12_Character_204.ptr, Helvetica12_Character_205.ptr,
    Helvetica12_Character_206.ptr, Helvetica12_Character_207.ptr,
    Helvetica12_Character_208.ptr, Helvetica12_Character_209.ptr,
    Helvetica12_Character_210.ptr, Helvetica12_Character_211.ptr,
    Helvetica12_Character_212.ptr, Helvetica12_Character_213.ptr,
    Helvetica12_Character_214.ptr, Helvetica12_Character_215.ptr,
    Helvetica12_Character_216.ptr, Helvetica12_Character_217.ptr,
    Helvetica12_Character_218.ptr, Helvetica12_Character_219.ptr,
    Helvetica12_Character_220.ptr, Helvetica12_Character_221.ptr,
    Helvetica12_Character_222.ptr, Helvetica12_Character_223.ptr,
    Helvetica12_Character_224.ptr, Helvetica12_Character_225.ptr,
    Helvetica12_Character_226.ptr, Helvetica12_Character_227.ptr,
    Helvetica12_Character_228.ptr, Helvetica12_Character_229.ptr,
    Helvetica12_Character_230.ptr, Helvetica12_Character_231.ptr,
    Helvetica12_Character_232.ptr, Helvetica12_Character_233.ptr,
    Helvetica12_Character_234.ptr, Helvetica12_Character_235.ptr,
    Helvetica12_Character_236.ptr, Helvetica12_Character_237.ptr,
    Helvetica12_Character_238.ptr, Helvetica12_Character_239.ptr,
    Helvetica12_Character_240.ptr, Helvetica12_Character_241.ptr,
    Helvetica12_Character_242.ptr, Helvetica12_Character_243.ptr,
    Helvetica12_Character_244.ptr, Helvetica12_Character_245.ptr,
    Helvetica12_Character_246.ptr, Helvetica12_Character_247.ptr,
    Helvetica12_Character_248.ptr, Helvetica12_Character_249.ptr,
    Helvetica12_Character_250.ptr, Helvetica12_Character_251.ptr,
    Helvetica12_Character_252.ptr, Helvetica12_Character_253.ptr,
    Helvetica12_Character_254.ptr, Helvetica12_Character_255.ptr
];

struct BitmapFontData
{
    char[]          name;         /* The source font name             */
    int             quantity;     /* Number of chars in font          */
    int             height;       /* Height of the characters         */
    int             width;        /* # for fixed width, <=0 for proportional */
    union {
        struct{
            float           xorig, yorig; /* Relative origin of the character */
        }
        Point orig; 
    }
    const GLubyte** characters; /* The character mapping            */
};


/* The font structure: */
static const BitmapFontData helvetica_12_data = { 
    name: "-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1", 
    quantity: 256,
    height: 16,
    width: -1,
    xorig: 0,
    yorig: 4,
    characters: Helvetica12_Character_Map.ptr
};
