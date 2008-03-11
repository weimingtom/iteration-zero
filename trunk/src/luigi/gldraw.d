//---------------------------------------------------------------------
/*
 Copyright:

  luigi/gldraw.d -- OpenGL drawing utilities 'luigi' user interface library.

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
module luigi.gldraw;

import luigi.gui;
import luigi.opengl;
import luigi.base;
import std.math : PI,sin,cos;

class GLException : Exception
{
    this(char[] msg) { super(msg); }
}


void translate(float x, float y)
{
    glTranslatef(x,y,0);
}
void translate(Point p)
{
    glTranslatef(p.x,p.y,0);
}
void untranslate(float x, float y)
{
    glTranslatef(-x,-y,0);
}
void untranslate(Point p)
{
    glTranslatef(-p.x,-p.y,0);
}

void push_clip_rect(Rect r)
{
    // Rect is in user coords & needs to be projected to window coords for glscissor
    glPushAttrib(GL_SCISSOR_BIT);
    glEnable(GL_SCISSOR_TEST);
    float[16] M; glGetFloatv(GL_MODELVIEW_MATRIX, &M[0]);
    float[16] P;  glGetFloatv(GL_PROJECTION_MATRIX, &P[0]);
    int[4] V;  glGetIntegerv(GL_VIEWPORT, &V[0]);

    // This is basically gluProject, but we only care about the 2D
    // part so we can shortcut the math a bit.

    // Transform point r.pos  [r.pos.x, r.pos.y, 0, 1]
    void xform(inout Point p)
    {
        float x = M[0] * p.x  + M[4] * p.y + M[12];
        float y = M[1] * p.x  + M[5] * p.y + M[13];
        float z = M[2] * p.x  + M[6] * p.y + M[14];
        float w = M[3] * p.x  + M[7] * p.y + M[15];
        p.x = P[0] * x + P[4] * y + P[8] * z + P[12] * w;
        p.y = P[1] * x + P[5] * y + P[9] * z + P[13] * w;
        w   = P[3] * x + P[7] * y + P[11]* z + P[15] * w;
        p.x /= w;
        p.y /= w;
    }

    Point p1 = r.pos;
    Point p2 = r.pos; p2 += r.size; 
    xform(p1);
    xform(p2);

    int ix,iy,ix2,iy2;
    ix  = V[0] + cast(int)((1 + p1.x) * V[2] / 2);
    iy  = V[1] + cast(int)((1 + p1.y) * V[3] / 2);
    ix2 = V[0] + cast(int)((1 + p2.x) * V[2] / 2);
    iy2 = V[1] + cast(int)((1 + p2.y) * V[3] / 2);
    int ih = iy2-iy;
    
    if (ih>0) {
        glScissor(ix, iy, ix2-ix, iy2-iy);
    }
    else {
        glScissor(ix, iy2, ix2-ix, -ih);
    }
}
void pop_clip_rect()
{
    glPopAttrib();
}

void fill_rect(Rect r) {
    glBegin(GL_QUADS);
    glVertex2f(r.x1,r.y1);
    glVertex2f(r.x2,r.y1);
    glVertex2f(r.x2,r.y2);
    glVertex2f(r.x1,r.y2);
    glEnd();
}

void fill_rect(Rect r, float[4] tc) {
    glBegin(GL_QUADS);
    glTexCoord2f(tc[0],tc[1]); glVertex2f(r.x1,r.y1);
    glTexCoord2f(tc[2],tc[1]); glVertex2f(r.x2,r.y1);
    glTexCoord2f(tc[2],tc[3]); glVertex2f(r.x2,r.y2);
    glTexCoord2f(tc[0],tc[3]); glVertex2f(r.x1,r.y2);
    glEnd();
}

void fill_rect(Rect r, int[4] tc) {
    glBegin(GL_QUADS);
    glTexCoord2i(tc[0],tc[1]); glVertex2f(r.x1,r.y1);
    glTexCoord2i(tc[2],tc[1]); glVertex2f(r.x2,r.y1);
    glTexCoord2i(tc[2],tc[3]); glVertex2f(r.x2,r.y2);
    glTexCoord2i(tc[0],tc[3]); glVertex2f(r.x1,r.y2);
    glEnd();
}

void stroke_rect(Rect r) {
    glBegin(GL_LINE_LOOP);
    glVertex2f(r.x1,r.y1);
    glVertex2f(r.x2,r.y1);
    glVertex2f(r.x2,r.y2);
    glVertex2f(r.x1,r.y2);
    glEnd();
}

void fill_circle(float x, float y, float radius, int slices=16)
{
    glTranslatef(x,y,0);
    glBegin(GL_TRIANGLE_FAN);
    glVertex2f(0,0);
    float astep = 2*PI/slices;
    for(int i=0; i<slices+1; i++)
    {
        float a = i*astep;
        float c = radius*cos(a);
        float s = radius*sin(a);
        glVertex2f(c,s);
    }
    glEnd();
    glTranslatef(-x,-y,0);
}


void stroke_circle(float x, float y, float radius=1, int slices=16)
{
    glTranslatef(x,y,0);
    glBegin(GL_LINE_LOOP);
    float astep = 2*PI/slices;
    for(int i=0; i<slices+1; i++)
    {
        float a = i*astep;
        float c = radius*cos(a);
        float s = radius*sin(a);
        glVertex2f(c,s);
    }
    glEnd();
    glTranslatef(-x,-y,0);
}

void fill_arc(float x, float y, float radius, float start, float radians, int slices=16)
{
    glTranslatef(x,y,0);
    glBegin(GL_TRIANGLE_FAN);
    glVertex2f(0,0);
    float astep = radians/slices;
    for(int i=0; i<slices+1; i++)
    {
        float a = start+i*astep;
        float c = radius*cos(a);
        float s = -radius*sin(a);
        glVertex2f(c,s);
    }
    glEnd();
    glTranslatef(-x,-y,0);
}

void stroke_arc(float x, float y, float radius, float start, float radians, int slices=16)
{
    glTranslatef(x,y,0);
    glBegin(GL_LINE_LOOP);
    glVertex2f(0,0);
    float astep = radians/slices;
    for(int i=0; i<slices+1; i++)
    {
        float a = start+i*astep;
        float c = radius*cos(a);
        float s = -radius*sin(a);
        glVertex2f(c,s);
    }
    glEnd();
    glTranslatef(-x,-y,0);
}


void fill_rounded_rect(Rect r, float radius, int slices=8)
{
    Rect ir = r; ir.inset(radius);
    if (2*radius >= r.width) { radius = r.width/2; }
    if (2*radius >= r.height) { radius = r.height/2; }
    glBegin(GL_QUADS);
    glVertex2f(ir.x1, r.y1);
    glVertex2f(ir.x2, r.y1);
    glVertex2f(ir.x2, r.y2);
    glVertex2f(ir.x1, r.y2);

    glVertex2f(r.x1,  ir.y1);
    glVertex2f(ir.x2, ir.y1);
    glVertex2f(ir.x2, ir.y2);
    glVertex2f(r.x1,  ir.y2);

    glVertex2f(ir.x2, ir.y1);
    glVertex2f(r.x2,  ir.y1);
    glVertex2f(r.x2,  ir.y2);
    glVertex2f(ir.x2, ir.y2);
    glEnd();

    fill_arc(ir.x1,ir.y1,radius, PI/2,PI/2,slices);
    fill_arc(ir.x2,ir.y1,radius, 0,PI/2,slices);
    fill_arc(ir.x1,ir.y2,radius, PI,PI/2,slices);
    fill_arc(ir.x2,ir.y2,radius, -PI/2,PI/2,slices);
}

void stroke_rounded_rect(Rect r, float radius, int slices=8)
{
    Rect ir = r; ir.inset(radius);
    if (2*radius >= r.width) { radius = r.width/2; }
    if (2*radius >= r.height) { radius = r.height/2; }

    void _arc(float x, float y, float start, float radians, int slices=16)
    {
        glTranslatef(x,y,0);
        float astep = radians/slices;
        for(int i=0; i<slices+1; i++)
        {
            float a = start+i*astep;
            float c = radius*cos(a);
            float s = -radius*sin(a);
            glVertex2f(c,s);
        }
        glTranslatef(-x,-y,0);
    }

    glBegin(GL_LINE_LOOP);
    _arc(ir.x1,ir.y1, PI/2, PI/2);
    _arc(ir.x2,ir.y1, 0,    PI/2);
    _arc(ir.x1,ir.y2, PI,   PI/2);
    _arc(ir.x2,ir.y2, -PI/2,PI/2);
    glEnd();

}

void stroke_raised_rect( Rect r, Color bkg, Color dark, Color medium, Color light)
{
    r.inset(0.5);

    with (r) {
        translate(0.5,0.5); scope(exit) untranslate(0.5,0.5);
        glColor4ubv( bkg.ptr );
        glBegin( GL_LINE_LOOP );
        glVertex2f( x+1, y+1 );  glVertex2f( x2-1, y+1 );
        glVertex2f( x2-1, y2-1 );  glVertex2f( x+1, y2-1 );
        glEnd();

        glColor4ubv( light.ptr );
        glBegin( GL_LINE_STRIP );
        glVertex2f( x, y2 );  glVertex2f( x, y );  glVertex2f( x2, y );
        glEnd();

        glColor4ubv( dark.ptr );
        glBegin( GL_LINE_STRIP );
        glVertex2f( x2, y );  glVertex2f( x2, y2 );  glVertex2f( x, y2 );
        glEnd();

        glColor4ubv( medium.ptr );
        glBegin( GL_LINE_STRIP );
        glVertex2f( x2-1, y+1 );  glVertex2f( x2-1, y2-1 );  glVertex2f( x+1, y2-1 );
        glEnd();
    }
}

void stroke_sunken_rect( Rect r, Color bkg, Color dark, Color medium, Color light)
{
    r.inset(0.5);
    
    with(r) {
        // inner loop
        glBegin( GL_LINE_STRIP );
        glColor4ubv( medium.ptr );
        glVertex2f( x +1, y2-1 );
        glVertex2f( x +1, y +1 );
        glVertex2f( x2-1, y +1 );

        glColor4ubv( bkg.ptr );
        glVertex2f( x2-1, y +1 );
        glVertex2f( x2-1, y2-1 );
        glVertex2f( x +1, y2-1 );
        glEnd();

        // outer loop
        glBegin( GL_LINE_STRIP );
        glColor4ubv( dark.ptr );
        glVertex2f( x , y2 );
        glVertex2f( x , y  );
        glVertex2f( x2, y  );

        glColor4ubv( light.ptr );
        glVertex2f( x2, y  );
        glVertex2f( x2, y2 );
        glVertex2f( x , y2 );
        glEnd();
    }
//    stroke_raised_rect(r, bkg, light, medium, dark);
/*
    glColor3ubv(dark.ptr);
    glBegin( GL_LINE_LOOP );
    glVertex2f( r.x, r.y );         glVertex2f( r.x2, r.y );
    glVertex2f( r.x2, r.y2 );       glVertex2f( r.x, r.y2 );
    glEnd();

    glBegin( GL_LINE_LOOP );
    glVertex2f( r.x+1, r.y+1 );       glVertex2f( r.x2-1, r.y+1 );
    glVertex2f( r.x2-1, r.y2-1 );     glVertex2f( r.x+1, r.y2-1 );
    glEnd();
*/

/*
    float x = r.x;
    float y = r.y;
    float x2 = r.x2;
    float y2 = r.y2;

    glColor3ubv( bkgd_color );
    glBegin( GL_LINE_LOOP );
    glVertex2f( x+1, y+1 );         glVertex2f( x2-1, y+1 );
    glVertex2f( x2-1, y2-1 );     glVertex2f( x+1, y2-1 );
    glEnd();

    glColor4ubv( dark );
    glBegin( GL_LINE_STRIP );
    glVertex2f( x, y2 );  glVertex2f( x, y );  glVertex2f( x2, y );
    glEnd();

    glColor4ubv( light );
    glBegin( GL_LINE_STRIP );
    glVertex2f( x2, y );  glVertex2f( x2, y2 );  glVertex2f( x, y2 );
    glEnd();

    glColor4ubv( medium );
    glBegin( GL_LINE_STRIP );
    glVertex2f( x2-1, y+1 );  glVertex2f( x2-1, y2-1 );  glVertex2f( x+1, y2-1 );
    glEnd();
*/
}

char[] getGLErrors(char[] file = null, int line = -1)
{
    GLenum err;
    char[] errstr = null;
    int count = 0;
    while ( (err=glGetError()) != GL_NO_ERROR) 
    {
        if (!errstr) errstr = "OpenGL error: ";
        else errstr ~= std.string.format("\nOpenGL error(%d) : ", ++count);
        if (file) errstr ~= std.string.format("%s(%s): ", file, line);
        else if (line >= 0) errstr ~= std.string.format("(%s): ", line);
        switch(err) {
        case GL_INVALID_ENUM:
            // An unacceptable value is specified for an enumerated
            // argument. The offending command is ignored, having no
            // side effect other than to set the error flag.
            errstr ~= "Invalid enum";
            break;
        case GL_INVALID_VALUE:
            //A numeric argument is out of range. The offending command is
            //ignored, having no side effect other than to set the error
            //flag.
            errstr ~= "Invalid value"; 
            break;
        case GL_INVALID_OPERATION:
            //The specified operation is not allowed in the current
            //state. The offending command is ignored, having no side
            //effect other than to set the error flag.
            errstr ~= "Invalid operation"; 
            break;

        case GL_STACK_OVERFLOW:
            // This command would cause a stack overflow. The
            // offending command is ignored, having no side effect
            // other than to set the error flag.
            errstr ~= "Stack overflow"; 
            break;

        case GL_STACK_UNDERFLOW:
            //  This command would cause a stack underflow. The
            //  offending command is ignored, having no side effect
            //  other than to set the error flag.
            errstr ~= "Stack underflow"; 
            break;

        case GL_OUT_OF_MEMORY:
            //  There is not enough memory left to execute the
            //  command. The state of the GL is undefined, except for
            //  the state of the error flags, after this error is
            //  recorded.
            errstr ~= "Out of memory";
            break;
        default:
            errstr ~= std.string.format("Unknown error code: %s", err);
        }
    }
    return errstr;
}

void checkGLErrors(char[] file = null, int line = -1)
{
    char[] ret = getGLErrors(file,line);
    if (ret) {
        writefln(ret);
    }
}
void throwGLErrors(char[] file = null, int line = -1)
{
    char[] ret = getGLErrors(file,line);
    if (ret) {
        throw new GLException(ret);
    }
}


//----------------------------------------------------------------------------
void push_graphics_state(Rect viewport) {
    glPushAttrib(GL_ENABLE_BIT // depth test, blending, texturing etc
                 | GL_POLYGON_BIT // poly mode, cull face etc
                 //| GL_CURRENT_BIT  // color, rasterpos etc
                 //| GL_DEPTH_BUFFER_BIT  // GL_DEPTH_TEST
                 //| GL_COLOR_BUFFER_BIT // GL_BLEND
                 );
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_CULL_FACE);
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glViewport(0,0,lrint(viewport.w),lrint(viewport.h));
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    // Note! We put the origin at top left to match window sys conventions
    glOrtho(viewport.x, viewport.x2, 
            viewport.y2, viewport.y1,
            -1,1);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
}

void pop_graphics_state() {
    glPopAttrib();
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    // TODO restore viewport?
    version(Debug) {
        checkGLErrors(__FILE__,__LINE__);
    }
}
