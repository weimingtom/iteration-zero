// Utility and Math functions
//    Copyright (c) kei mesuda 2007
//    (http://www.yomogi.sakura.ne.jp/~si)
//    Modified klaus blindert 2008
//------------------------------------------------------------------------------------------

module util;

import std.c.string;

//--------------------------------------------------------------------------------
// Log2
//--------------------------------------------------------------------------------
int nextPowerOfTwo(int i)
{
    int n;
    for (n=1; n<i;) { n<<=1; }
    return n;
}


int calcLog2(int i)
{
    int n;
    for (n=-1; i!=0; n++) { i>>=1; }
    return n;
}

char[] to_string(char* s)
{
    char[] r;
    r.length = strlen(s);
    for(int i=0; i != r.length; ++i)
        r[i] = s[i];
    return r;
}

//--------------------------------------------------------------------------------
// constant
//--------------------------------------------------------------------------------
const float  fPI=3.141593f;
const float  fRD=6.2831853f;
const double dPI=3.141592653589793;
const double dRD=6.2831853071795865;


//--------------------------------------------------------------------------------
// Log2
//--------------------------------------------------------------------------------
int calc2N(int i)
{
    int n;
    for (n=1; n<i;) { n<<=1; }
    return n;
}

//--------------------------------------------------------------------------------
// Exchange
//--------------------------------------------------------------------------------
float ubyte2float(ubyte b) { return cast(float)(b*0.0039215686f); }
ubyte float2ubyte(float f) { return cast(ubyte)(f*255.0f+0.5f); }

double ubyte2double(ubyte  b) { return cast(double)(b*0.0039215686); }
ubyte  double2ubyte(double f) { return cast(ubyte)(f*255.0+0.5); }

double degree2radian(double d) { return d * 0.01745329251994330; }
double radian2degree(double d) { return d * 57.295779513082321; }

//--------------------------------------------------------------------------------
// max/min
//--------------------------------------------------------------------------------
T abs(T)(T t) { return (t<0)?-t:t; }
T min(T)(T t0, T t1) { return (t0<t1)?t0:t1; }
T max(T)(T t0, T t1) { return (t0>t1)?t0:t1; }
T limit(T)(T t0, T tmin, T tmax) { return (t0>tmin)?(t0<tmax)?t0:tmax:tmin; }
