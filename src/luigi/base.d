//---------------------------------------------------------------------
/*
 Copyright:

  luigi/base.d -- basic definitions for the 'luigi' user interface library.

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
module luigi.base;

import math = std.math;
import string = std.string;

//import std.math : lrint;  // Error: "not implemented"!!

// Until std.math actually implements it
long lrint(float v) {
    return cast(long)math.floor(v+0.5);
}


typedef Exception GUIException;



/** Rectangle class.  Size and position are stored in floating point.
 */
struct Rect
{
    // This layout lets you use rect either like
    //    (r.x, r.y, r.width, r.height) // primary
    // or like
    //    (r.x1, r.y1, r.x2, r.y2)  // also allowed with restrictions
    // or like
    //    (r.pos, r.size);
    union {
        struct { float x,y; }
        Point pos;
    }
    union {
        struct { float width ,height; }
        Size size;
    }

    alias x x1;
    alias y y1;
    alias width w;
    alias height h;

    float x2() { return x+w; }
    float y2() { return y+h; }
    float x2(float v) { return w=(v-x); }
    float y2(float v) { return h=(v-y); }

    Point center() { return Point(x+width/2,y+height/2); }
    Point minpos() { return Point(x1,y1); }
    Point maxpos() { return Point(x2,y2); }

    /** Rect constructor */
    static Rect opCall(float rx = 0, float ry = 0, float rwidth = 0, float rheight = 0) {
        Rect r;  r.x = rx; r.y = ry; r.w = rwidth; r.h = rheight;
        return r;
    }
    /** Set value using left,top,width,height convention */
    void  set(float xpos, float ypos, float rwidth, float rheight) {
        x = xpos; y = ypos; w = rwidth; h = rheight;
    }
    /** Set value using left,top,right,bottom convention */
    void  setLTRB(float leftx, float topy, float rightx, float bottomy) {
        set(leftx,topy,rightx-leftx,bottomy-topy);
    }
    alias setLTRB setMinMax;

    /** Point-inside-rect test */
    bool contains(float px, float py) {
        return ( x <= px && px < x2 && y <= py && py < y2 );
    }
    bool contains(Point p) {
        return contains(p.x,p.y);
    }

    void inset(float v) { 
        if (2*v > w) { x += w/2; w = 0; }
        else    {         x+=v; w-=2*v; }
        if (2*v > h) { y += h/2; h = 0; }
        else {            y+=v; h-=2*v; }
    }
    void inflate(float v) {
        inset(-v);
    }

    /** Intersection of two rects. */
    //void intersect(Rect o) {
    //    // not implemented
    //}

    /** Grow rect to enclose other rect (union of rects.) */
    void enclose(Rect o) {
        enclose(o.pos);
        enclose(Point(o.x2,o.y2));
    }
    /** Grow rect to enclose point p. */
    void enclose(Point p) {
        if (!valid) {
            x = p.x; y=p.y;
            w = 0; h = 0;
        }
        if (p.x < x) {  w = x2-p.x;  x = p.x; }
        else if (p.x > x2) {  x2 = p.x; }
        if (p.y < y) {  h = y2-p.y;  y = p.y; }
        else if (p.y > y2) {  y2 = p.y; }
    }


    bool valid() {
        return !(math.isnan(x)||math.isnan(y)||
                 math.isnan(w)||math.isnan(h));
    }

    char[] toString() {
        return string.format("Rect(%0.1f,%0.1f,%0.1f,%0.1f)",x,y,width,height);
    }
}

/** Size is used to describe the width and height of a box.
    Width is given by size.width, and height by size.height
    Several synonyms are provided for the .w and .h properties. 
    .width is equivalent to .w or .x.
    .height is equivalent to .h or .y.
 */
struct Size
{
    float width = 0;
    float height = 0;
    alias width x;
    alias height y;
    alias width w;
    alias height h;

    /** Constructor */
    static Size opCall(float _w, float _h) {
        Size s; s.width = _w;  s.height = _h; return s;
    }

    /** Allows Sizes to be added to using s1 += s2. */
    void opAddAssign(Size o) { w+=o.w; h+=o.h; }

    /** Allows Sizes to be subtracted from using s1 += s2. */
    void opSubAssign(Size o) { w-=o.w; h-=o.h; }

    /** Allows Sizes to be multiplied by a scalar */
    void opMulAssign(float d) { w*=d; h*=d; }

    /** Return a printable representation of the Size */
    char[] toString() {
        return string.format("(%0.1f,%0.1f)",w,h);
    }

    /** Return a pointer to the first element */
    float *ptr() { return &x; }
}

/** Point can be used as an alias for Size.  
    This is not a typedef so there is no compiler enforcement of the distinction,
    but you can use Point vs Size as the situation dictates to indicate the
    intent of the variable, be it a location or a displacement.
 */
alias Size Point;


/** Color represents a standard 4-byte RGBA color value.
    It is the type used to represent colors througout Luigi.
    Color components can be accessed via .r, .g, .b, and .a properties.
    Alternatively, one can use numerical indexing, e.g. c[0],c[1],c[2],c[3].
 */
struct Color
{
    union {
        struct {
            ubyte r=0,g=0,b=0,a=255;
        }
        ubyte[4] v;
    }

    /** Constructor using integral values in the range 0--255 */
    static Color opCall(int r_, int g_, int b_, int a_=255) {
        Color c; c.set(r_,g_,b_,a_); return c;
    }
    /** Constructor using floating point  values in the range 0.0--1.0 */
    static Color opCall(float r_, float g_, float b_, float a_=1.0) {
        Color c; c.set(r_,g_,b_,a_); return c;
    }

    /** Setter using integral values in the range 0--255 */
    void set(int r_, int g_, int b_, int a_=255) {
        r=r_, g=g_, b=b_, a=a_;
    }

    /** Setter using floating point  values in the range 0.0--1.0 */
    void set(float r_, float g_, float b_, float a_=1.0) {
        r=lrint(r_*255), g=lrint(g_*255), b=lrint(b_*255), a=lrint(a_*255);
    }

    /** Linearly interpolate between the two colors.
        Params:
            c1 = the first color 
            c2 = the second color
            t  = the interpolation parameter between 0 and 1
        Returns: the blended color 
                 If t==0 then the result is c1
                 If t==1 then the result is c2
    */
    static Color lerp(Color c1, Color c2, float t) {
        Color ret;
        ret.r = lrint( (1-t)*c1.r + t*c2.r );
        ret.g = lrint( (1-t)*c1.g + t*c2.g );
        ret.b = lrint( (1-t)*c1.b + t*c2.b );
        ret.a = lrint( (1-t)*c1.a + t*c2.a );
        return ret;
    }

    /** Allows a color to be indexed with brackets [].
        R,G,B,A components correspond to indexes 0,1,2,3.
     */
    ubyte opIndex(int i) { return v[i]; }
    ubyte opIndexAssign(int i, ubyte val) { return v[i]=val; }

    /** Return a pointer to the beginning of the 4 bytes that
     *  make up this color.  Bytes are ordered R,G,B,A.
     */
    ubyte* ptr() { return &r; }
}


//----------------------------------------------------------------------------

/**
 * Returns the largest of the two supplied types, or the first type if
 * the sizes are identical.  If either type is an object then both must
 * be objects of either the same type or where one is a base class of
 * the other.  Interfaces are not supported.  Used by min() and max() templates.
 */
template largestOrConvertible( T, U )
{
    static if( is( T : Object ) || is( U : Object ) )
    {
        static assert( is( T : Object ) && is( U : Object ),
                       "Types incompatible." );

        static if( is(T : U) )
            alias T largestOrConvertible;
        else static if( is(U : T) )
            alias U largestOrConvertible;
        else static assert( false, "Object types must be related." );
    }
    else static if( is( T : Interface ) || is( U : Interface ) )
        static assert( false, "Interface types not yet supported." );
    else static if( T.sizeof < U.sizeof )
        alias U largestOf;  // concrete type, U larger
    else alias T largestOf; // concrete type, T larger or equal
}

/** Returns the maximum of two values */
template max( T, U )
{
    largestOrConvertible!(T,U).largestOf max( T t, U u )
    {
        return t > u ? t : u;
    }
}

/** Returns the minimum of two values */
template min( T, U )
{
    largestOrConvertible!(T,U).largestOf min( T t, U u )
    {
        return t < u ? t : u;
    }
} 
unittest {
    int ismall=1, ibig=5;
    byte bsmall=1, bbig=5;
    float fsmall=1, fbig=5;
    double dsmall=1, dbig=5;
    assert(max(ibig,ismall)==ibig);
    assert(max(ismall,ibig)==ibig);
    assert(min(ibig,ismall)==ismall);
    assert(min(ismall,ibig)==ismall);

    assert(max(fbig,fsmall)==fbig);
    assert(max(fsmall,fbig)==fbig);
    assert(min(fbig,fsmall)==fsmall);
    assert(min(fsmall,fbig)==fsmall);

    assert(min(dsmall,fbig)==dsmall);
    assert(max(dsmall,fbig)==fbig);
    assert(min(dbig,fsmall)==fsmall);
    assert(max(dbig,fsmall)==dbig);

    assert( is(typeof(min(dsmall,fbig)) == double) );
    assert( is(typeof(max(dsmall,fbig)) == double) );
    assert( is(typeof(min(dbig,fsmall)) == double) );
    assert( is(typeof(max(dbig,fsmall)) == double) );

    assert( is(typeof(min(bsmall,ibig))==int) );
    assert( is(typeof(max(bsmall,ibig))==int) );
    assert( is(typeof(min(bbig,ismall))==int) );
    assert( is(typeof(max(bbig,ismall))==int) );
}



//----------------------------------------------------------------------------


/* Some simple array manipulators that should be in the standard library but aren't 
 * Adapted mostly from Cashew.utils
 */

const size_t NOT_FOUND = size_t.max ;


/**
 *  Remove an item from an array by index, and return it.
 */
T drop (T) (inout T[] haystack, size_t index)
in {
  assert(index < haystack.length, ".drop() called with index greater than array length");
}
body {
  T      result = haystack[index]     ;
  size_t max    = haystack.length - 1 ;

  for (; index < max; ++index) {
    haystack[index] = haystack[index + 1];
  }
  haystack.length = max;
  return result;
}
unittest {
  //_begin(r".drop(N)"c);
  int[] foo = [0, 1, 2, 3, 4, 5] ;
  int   elm = foo.drop(3U)       ;
  assert(foo == [0, 1, 2, 4, 5]);
  assert(elm == 3              );
  //_end;
}

/**
 *  Remove an item from an array by value, and return it.
 *  (modified from Cashew.utils)
 */
T drop_item (T) (inout T[] haystack, T item)
body {
    size_t index = haystack.find_item(item);
    assert(index!=NOT_FOUND, ".drop_item() called for an item not in the array");
    return haystack.drop(index);
}
unittest {
    //_begin(r".drop(N)"c);
    int[] foo = [0, 1, 2, 3, 4, 5] ;
    int   elm = foo.drop_item(3)       ;
    assert(foo == [0, 1, 2, 4, 5]);
    assert(elm == 3              );
    //_end;
}

/**
 * Remove a range of elements from an array in place.  
 * It is not an error for the range to be empty or for start to be greater than end.
 * If so, the array is not modified.
 * Out of bounds checks performed only in debug mode.
 * Returns: the array, which (is modified in-place).
 * Note: This is an O(n) operation.
 */
template drop_range(T)
{
  T[] drop_range( inout T[] arr, int start, int end )
  {
    debug if ( start>=arr.length || end > arr.length || start<0 || end<0)
        throw new Exception(string.format("Attempt to drop range %s,%s from size %s",start,end,arr.length));
    if (start>=end) return arr;
    size_t len = end-start;
    size_t max = arr.length-len;
    for (; start < max; start++ ) arr[start]=arr[start+len];
    arr.length= max;
    return arr;
  }
} 

/***********************************************************************************
 * Lookup the index of the first element of an array that satisfies a delegate.
 * Returns: the index of the value or NOT_FOUND if not found.
 */
size_t find (T) (T[] haystack, bool delegate (T) dg)
body {
    foreach (index, needle; haystack) {
        if (dg(needle)) {
            return index;
        }
    }
    return NOT_FOUND;
}
unittest {
    //_begin(r".find(Dlg)"c);
    int[] foo = [1, 2, 4, 8, 16, 32];
    size_t idx = foo.find((int x) {return x > 10;});
    assert(idx == 4U);
    //_end;
}

/***********************************************************************************
 * Lookup the index of the first element of an array that equals a given value
 * Returns: the index of the value or NOT_FOUND if not found.
 */
size_t find_item (T) (T[] haystack, T cmp)
body {
  foreach (index, needle; haystack) {
    if (needle==cmp) {
      return index;
    }
  }
  return NOT_FOUND;
}
unittest {
    //_begin(r".find_item(Dlg)"c);
    int[] foo = [1, 2, 4, 8, 16, 32];
    size_t idx = foo.find_item(4);
    assert(idx == 2U);
  //_end;
}

/***********************************************************************************
 * Return whether a particular item is in an array 
 */
bool contains(T) (T[] haystack, T cmp)
body {
    return haystack.find_item(cmp) != NOT_FOUND;
}
unittest {
    //_begin(r".contains(Dlg)"c);
    int[] foo = [1, 3, 4, 8, 16, 32];
    assert(foo.contains(3));
    assert(!foo.contains(42));
    //_end;
}

