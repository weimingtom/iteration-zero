//--------------------------------------------------------------------------------
// Vector/Matrix Classes
//    Copyright (c) kei mesuda 2007
//    (http://www.yomogi.sakura.ne.jp/~si)
//--------------------------------------------------------------------------------
module vector;

private import std.math;
private import std.string;


template TVector(T) {
//--------------------------------------------------------------------------------
// Vector structure
//--------------------------------------------------------------------------------
struct vector2 {
    union {
        struct { T x=0, y=0; };
        T[2]    e;
    }
}

struct vector3 {
    union {
        struct { T x=0, y=0, z=0; };
        T[3]    e;
        vector2 v2;
    }
}

struct vector4 {
    union {
        struct { T x=0, y=0, z=0, w=1; };
        struct { T r, g, b, a; };
        T[4]    e;
        vector2 v2;
        vector3 v3;
    }
}

struct matrix2 {
    union {
        struct { T a00=1, a01=0, a10=0, a11=1; };
        T[4]        e;
        vector2[2]  v;
    }
}

struct matrix4 {
    union {
        struct { T a00=1, a01=0, a02=0, a03=0,
                   a10=0, a11=1, a12=0, a13=0,
                   a20=0, a21=0, a22=1, a23=0,
                   a30=0, a31=0, a32=0, a33=1; };
        T[16]       e;
        vector4[4]  v;
    }
}




//--------------------------------------------------------------------------------
// 2D vector calculation
//--------------------------------------------------------------------------------
vector2* operate(vector2* v, void delegate(inout T) fn) { fn(v.x); fn(v.y); return v; }
vector2* operate(vector2* dst, vector2* src, void delegate(inout T, T) fn) { fn(dst.x,src.x); fn(dst.y,src.y); return dst; }
vector2* operate(vector2* ans, vector2* dst, vector2* src, T delegate(T, T) fn) { ans.x=fn(dst.x,src.x); ans.y=fn(dst.y,src.y); return ans; }
vector2* set(vector2* v, T x, T y) { v.x=x; v.y=y; return v; }
vector2* setAngle(vector2* v, T r) { v.x=cos(r);   v.y=sin(r); return v; }
vector2* setRound(vector2* v, T r, T l) { v.x=cos(r)*l; v.y=sin(r)*l; return v; }
vector2* copy(vector2* dst, vector2* src) { dst.x=src.x; dst.y=src.y; return dst; }
vector2* copyMin(vector2* ans, vector2* v0, vector2* v1) { ans.x=(v0.x<v1.x)?v0.x:v1.x; ans.y=(v0.y<v1.y)?v0.y:v1.y; return ans; }
vector2* copyMax(vector2* ans, vector2* v0, vector2* v1) { ans.x=(v0.x>v1.x)?v0.x:v1.x; ans.y=(v0.y>v1.y)?v0.y:v1.y; return ans; }
vector2* copyLimit(vector2* ans, vector2* val, vector2* vmin, vector2* vmax) { return copyMax(ans, copyMin(ans, val, vmax), vmin); }
vector2* copyLimit01(vector2* ans, vector2* v) { ans.x=(v.x>=1)?1:(v.x>0)?v.x:0; ans.y=(v.y>=1)?1:(v.y>0)?v.y:0; return ans; }
bool isZero(vector2* v) { return (v.x==0) && (v.y==0); }
bool isNormalized(vector2* v) { return (length2(v)==1); }
bool isEqual(vector2* v0, vector2* v1) { return (v0.x==v1.x) && (v0.y==v1.y); }

vector2* neg(vector2* v) { v.x=-v.x; v.y=-v.y; return v; }
vector2* add(vector2* dst, vector2* src) { dst.x+=src.x; dst.y+=src.y; return dst; }
vector2* add(vector2* dst, T x, T y) { dst.x+=x; dst.y+=y; return dst; }
vector2* add(vector2* pos, vector2* dir, T l) { pos.x+=dir.x*l; pos.y+=dir.y*l; return pos; }
vector2* add(vector2* dst, T t0, vector2* src, T t1) { dst.x*=t0; dst.x+=src.x*t1; dst.y*=t0; dst.y+=src.y*t1; return dst; }
vector2* blend(vector2* dst, vector2* src, T t) { return add(dst, 1-t, src, t); }
vector2* scale(vector2* dst, T s) { dst.x*=s; dst.y*=s; return dst; }
vector2* scale(vector2* dst, T x, T y) { dst.x*=x; dst.y*=y; return dst; }
vector2* scale(vector2* dst, vector2* src) { dst.x*=src.x; dst.y*=src.y; return dst; }
vector2* scaleinv(vector2* dst, T x, T y) { dst.x/=x; dst.y/=y; return dst; }
vector2* scaleinv(vector2* dst, vector2* src) { dst.x/=src.x; dst.y/=src.y; return dst; }
vector2* sub(vector2* dst, vector2* src) { dst.x-=src.x; dst.y-=src.y; return dst; }
vector2* mul(vector2* v, vector2* r) { T x_=v.x; v.x=v.x*r.x+v.y*r.y; v.y=-x_*r.y+v.y*r.x; return v; }
vector2* div(vector2* v, vector2* r) { T l=length2(r); if (l==0) return v; l=1/l; T x_=v.x; v.x=(v.x*r.x-v.y*r.y)*l; v.y=(x_*r.y+v.y*r.x)*l; return v; }
vector2* mod(vector2* dst, vector2* src) { dst.x-=floor(dst.x/src.x)*src.x; dst.y-=floor(dst.y/src.y)*src.y; return dst; }
vector2* mod1(vector2* dst) { dst.x-=floor(dst.x); dst.y-=floor(dst.y); return dst; }
vector2* normalize(vector2* v, T l=1) { T x=length2(v); if (x>0) scale(v,l/sqrt(x)); return v; }
vector2* rotate(vector2* v, T r) { T s=sin(r); T c=cos(r); T x_=v.x; v.x=v.x*c+v.y*s; v.y=-x_*s+v.y*c; return v; }
vector2* affine(vector2* v, vector2* affx, vector2* affy) { T x_=v.x; v.x=affx.x*v.x+affy.x*v.y; v.y=affx.y*x_+affy.y*v.y; return v; }

vector2* neg(vector2* ans, vector2* v) { ans.x=-v.x; ans.y=-v.y; return ans; }
vector2* add(vector2* ans, vector2* dst, vector2* src) { ans.x=dst.x+src.x; ans.y=dst.y+src.y; return ans; }
vector2* add(vector2* ans, vector2* dst, T x, T y) { ans.x=dst.x+x; ans.x=dst.y+y; return ans; }
vector2* add(vector2* ans, vector2* pos, vector2* dir, T l) { ans.x=pos.x+dir.x*l; ans.y=pos.y+dir.y*l; return ans; }
vector2* add(vector2* ans, vector2* v0, T t0, vector2* v1, T t1) { ans.x=v0.x*t0+v1.x*t1; ans.y=v0.y*t0+v1.y*t1; return ans; }
vector2* blend(vector2* ans, vector2* v0, vector2* v1, T t) { return add(ans, v0, 1-t, v1, t); }
vector2* scale(vector2* ans, vector2* dst, T s) { ans.x=dst.x*s; ans.y=dst.y*s; return ans; }
vector2* scale(vector2* ans, vector2* dst, T x, T y) { ans.x=dst.x*x; ans.y=dst.y*y; return ans; }
vector2* scale(vector2* ans, vector2* dst, vector2* src) { ans.x=dst.x*src.x; ans.y=dst.y*src.y; return ans; }
vector2* scaleinv(vector2* ans, vector2* dst, T x, T y) { ans.x=dst.x/x; ans.y=dst.y/y; return ans; }
vector2* scaleinv(vector2* ans, vector2* dst, vector2* src) { ans.x=dst.x/src.x; ans.y=dst.y/src.y; return ans; }
vector2* sub(vector2* ans, vector2* dst, vector2* src) { ans.x=dst.x-src.x; ans.y=dst.y-src.y; return ans; }
vector2* mul(vector2* ans, vector2* v, vector2* r) { ans.x=v.x*r.x+v.y*r.y; ans.y=-v.x*r.y+v.y*r.x; return ans; }
vector2* div(vector2* ans, vector2* v, vector2* r) { T l=length2(r); if (l==0) return v; l=1/l; ans.x=(v.x*r.x-v.y*r.y)*l; ans.y=(v.x*r.y+v.y*r.x)*l; return ans; }
vector2* mod(vector2* ans, vector2* dst, vector2* src) { ans.x=dst.x-floor(dst.x/src.x)*src.x; ans.y=dst.y-floor(dst.y/src.y)*src.y; return ans; }
vector2* mod1(vector2* ans, vector2* dst) { ans.x=dst.x-floor(dst.x); ans.y=dst.y-floor(dst.y); return ans; }
vector2* normalize(vector2* ans, vector2* v, T l=1) { T x=length2(v); if (x>0) scale(ans,v,l/sqrt(x)); return ans; }
vector2* rotate(vector2* ans, vector2* v, T r) { T s=sin(r); T c=cos(r); ans.x=v.x*c+v.y*s; ans.y=-v.x*s+v.y*c; return ans; }
vector2* affine(vector2* ans, vector2* v, vector2* affx, vector2* affy) { ans.x=affx.x*v.x+affy.x*v.y; ans.y=affx.y*v.x+affy.y*v.y; return ans; }

T length(vector2* v) { return sqrt(v.x*v.x+v.y*v.y); }
T length2(vector2* v) { return v.x*v.x+v.y*v.y; }
T atan (vector2* v) { return atan2(v.y, v.x); }
T dot(vector2* v0, vector2* v1) { return v0.x*v1.x+v0.y*v1.y; }
T cross(vector2* v0, vector2* v1) { return v0.x*v1.y-v0.y*v1.x; }




//--------------------------------------------------------------------------------
// 3D vector calculation for vector3
//--------------------------------------------------------------------------------
vector3* operate(vector3* v, void delegate(inout T) fn) { fn(v.x); fn(v.y); fn(v.z); return v; }
vector3* operate(vector3* dst, vector3* src, void delegate(inout T, T) fn) { fn(dst.x,src.x); fn(dst.y,src.y); fn(dst.z,src.z); return dst; }
vector3* operate(vector3* ans, vector3* dst, vector3* src, T delegate(T, T) fn) { ans.x=fn(dst.x,src.x); ans.y=fn(dst.y,src.y); ans.z=fn(dst.z,src.z); return ans; }
vector3* set(vector3* v, T x, T y)      { v.x=x; v.y=y; return v; }
vector3* set(vector3* v, T x, T y, T z) { v.x=x; v.y=y; v.z=z; return v; }
vector3* setAngleX(vector3* v, T r)      { v.y=cos(r);   v.z=sin(r); return v; }
vector3* setAngleY(vector3* v, T r)      { v.z=cos(r);   v.x=sin(r); return v; }
vector3* setAngleZ(vector3* v, T r)      { v.x=cos(r);   v.y=sin(r); return v; }
vector3* setRoundX(vector3* v, T r, T l) { v.y=cos(r)*l; v.z=sin(r)*l; return v; }
vector3* setRoundY(vector3* v, T r, T l) { v.z=cos(r)*l; v.x=sin(r)*l; return v; }
vector3* setRoundZ(vector3* v, T r, T l) { v.x=cos(r)*l; v.y=sin(r)*l; return v; }
vector3* copy(vector3* dst, vector2* src) { dst.x=src.x; dst.y=src.y; return dst; }
vector3* copy(vector3* dst, vector3* src) { dst.x=src.x; dst.y=src.y; dst.z=src.z; return dst; }
vector3* copyMin(vector3* ans, vector3* v0, vector3* v1) { ans.x=(v0.x<v1.x)?v0.x:v1.x; ans.y=(v0.y<v1.y)?v0.y:v1.y; ans.z=(v0.z<v1.z)?v0.z:v1.z; return ans; }
vector3* copyMax(vector3* ans, vector3* v0, vector3* v1) { ans.x=(v0.x>v1.x)?v0.x:v1.x; ans.y=(v0.y>v1.y)?v0.y:v1.y; ans.z=(v0.z>v1.z)?v0.z:v1.z; return ans; }
vector3* copyLimit(vector3* ans, vector3* val, vector3* vmin, vector3* vmax) { return copyMax(ans, copyMin(ans, val, vmax), vmin); }
vector3* copyLimit01(vector3* ans, vector3* v) { ans.x=(v.x>=1)?1:(v.x>0)?v.x:0; ans.y=(v.y>=1)?1:(v.y>0)?v.y:0; ans.z=(v.z>=1)?1:(v.z>0)?v.z:0; return ans; }
bool isZero(vector3* v) { return (v.x==0) && (v.y==0) && (v.z==0); }
bool isNormalized(vector3* v) { return (length2(v)==1); }
bool isEqual(vector3* v0, vector3* v1) { return (v0.x==v1.x) && (v0.y==v1.y) && (v0.z==v1.z); }

vector3* neg(vector3* v) { v.x=-v.x; v.y=-v.y; v.z=-v.z; return v; }
vector3* add(vector3* dst, vector3* src) { dst.x+=src.x; dst.y+=src.y; dst.z+=src.z; return dst;}
vector3* add(vector3* dst, T x, T y, T z) { dst.x+=x; dst.y+=y; dst.z+=z; return dst;}
vector3* add(vector3* pos, vector3* dir, T l) { pos.x+=dir.x*l; pos.y+=dir.y*l; pos.z+=dir.z*l; return pos; }
vector3* add(vector3* dst, T t0, vector3* src, T t1) { dst.x*=t0; dst.x+=src.x*t1; dst.y*=t0; dst.y+=src.y*t1; dst.z*=t0; dst.z+=src.z*t1; return dst; }
vector3* blend(vector3* dst, vector3* src, T t) { return add(dst, 1-t, src, t); }
vector3* sub(vector3* dst, vector3* src) { dst.x-=src.x; dst.y-=src.y; dst.z-=src.z; return dst; }
vector3* scale(vector3* dst, T s) { dst.x*=s; dst.y*=s; dst.z*=s; return dst; }
vector3* scale(vector3* dst, T x, T y, T z) { dst.x*=x; dst.y*=y; dst.z*=z; return dst; }
vector3* scale(vector3* dst, vector3* src) { dst.x*=src.x; dst.y*=src.y; dst.z*=src.z; return dst; }
vector3* scaleinv(vector3* dst, T x, T y, T z) { dst.x/=x; dst.y/=y; dst.z/=z; return dst; }
vector3* scaleinv(vector3* dst, vector3* src) { dst.x/=src.x; dst.y/=src.y; dst.z/=src.z; return dst; }
vector3* mulX(vector3* v, vector2* r) { T y_=v.y; v.y=v.y*r.x+v.z*r.y; v.z=-y_*r.y+v.z*r.x; return v; }
vector3* mulY(vector3* v, vector2* r) { T z_=v.z; v.z=v.z*r.x+v.x*r.y; v.x=-z_*r.y+v.x*r.x; return v; }
vector3* mulZ(vector3* v, vector2* r) { T x_=v.x; v.x=v.x*r.x+v.y*r.y; v.y=-x_*r.y+v.y*r.x; return v; }
vector3* mod(vector3* dst, vector3* src) { dst.x-=floor(dst.x/src.x)*src.x; dst.y-=floor(dst.y/src.y)*src.y; dst.z-=floor(dst.z/src.z)*src.z; return dst; }
vector3* mod1(vector3* dst) { dst.x-=floor(dst.x); dst.y-=floor(dst.y); dst.z-=floor(dst.z); return dst; }
vector3* normalize(vector3* v, T l=1) { T x=length2(v); if (x>0) scale(v,l/sqrt(x)); return v; }
vector3* rotateX(vector3* v, T r) { T s=sin(r); T c=cos(r); T y_=v.y; v.y=v.y*c+v.z*s; v.z=-y_*s+v.z*c; return v; }
vector3* rotateY(vector3* v, T r) { T s=sin(r); T c=cos(r); T z_=v.z; v.z=v.z*c+v.x*s; v.x=-z_*s+v.x*c; return v; }
vector3* rotateZ(vector3* v, T r) { T s=sin(r); T c=cos(r); T x_=v.x; v.x=v.x*c+v.y*s; v.y=-x_*s+v.y*c; return v; }
vector3* affine(vector3* v, vector2* affx, vector2* affy) { T x_=v.x; v.x=affx.x*v.x+affy.x*v.y; v.y=affx.y*x_+affy.y*v.y; return v; }
vector3* cross(vector3* v0, vector3* v1) { vector3 v; return copy(v0, cross(&v, v0, v1)); }

vector3* neg(vector3* ans, vector3* v) { ans.x=-v.x; ans.y=-v.y; ans.z=-v.z; return ans; }
vector3* add(vector3* ans, vector3* dst, vector3* src) { ans.x=dst.x+src.x; ans.y=dst.y+src.y; ans.z=dst.z+src.z; return ans; }
vector3* add(vector3* ans, vector3* dst, T x, T y, T z) { ans.x=dst.x+x; ans.y=dst.y+y; ans.z=dst.z+z; return ans; }
vector3* add(vector3* ans, vector3* pos, vector3* dir, T l) { ans.x=pos.x+dir.x*l; ans.y=pos.y+dir.y*l; ans.z=pos.z+dir.z*l; return ans; }
vector3* add(vector3* ans, vector3* v0, T t0, vector3* v1, T t1) { ans.x=v0.x*t0+v1.x*t1; ans.y=v0.y*t0+v1.y*t1; ans.z=v0.z*t0+v1.z*t1; return ans; }
vector3* blend(vector3* ans, vector3* v0, vector3* v1, T t) { return add(ans, v0, 1-t, v1, t); }
vector3* sub(vector3* ans, vector3* dst, vector3* src) { ans.x=dst.x-src.x; ans.y=dst.y-src.y; ans.z=dst.z-src.z; return ans; }
vector3* scale(vector3* ans, vector3* dst, T s) { ans.x=dst.x*s; ans.y=dst.y*s; ans.z=dst.z*s; return ans; }
vector3* scale(vector3* ans, vector3* dst, T x, T y, T z) { ans.x=dst.x*x; ans.y=dst.y*y; ans.z=dst.z*z; return ans; }
vector3* scale(vector3* ans, vector3* dst, vector3* src) { ans.x=dst.x*src.x; ans.y=dst.y*src.y; ans.z=dst.z*src.z; return ans; }
vector3* scaleinv(vector3* ans, vector3* dst, T x, T y, T z) { ans.x=dst.x/x; ans.y=dst.y/y; ans.z=dst.z/z; return ans; }
vector3* scaleinv(vector3* ans, vector3* dst, vector3* src) { ans.x=dst.x/src.x; ans.y=dst.y/src.y; ans.z=dst.z/src.z; return ans; }
vector3* mulX(vector3* ans, vector3* v, vector2* r) { ans.y=v.y*r.x+v.z*r.y; ans.z=-v.y*r.y+v.z*r.x; ans.x=v.x; return ans; }
vector3* mulY(vector3* ans, vector3* v, vector2* r) { ans.z=v.z*r.x+v.x*r.y; ans.x=-v.z*r.y+v.x*r.x; ans.y=v.y; return ans; }
vector3* mulZ(vector3* ans, vector3* v, vector2* r) { ans.x=v.x*r.x+v.y*r.y; ans.y=-v.x*r.y+v.y*r.x; ans.z=v.z; return ans; }
vector3* mod(vector3* ans, vector3* dst, vector3* src) { ans.x=dst.x-floor(dst.x/src.x)*src.x; ans.y=dst.y-floor(dst.y/src.y)*src.y; ans.z=dst.z-floor(dst.z/src.z)*src.z; return ans; }
vector3* mod1(vector3* ans, vector3* dst) { ans.x=dst.x-floor(dst.x); ans.y=dst.y-floor(dst.y); ans.z=dst.z-floor(dst.z); return ans; }
vector3* normalize(vector3* ans, vector3* v, T l=1) { T x=length2(v); if (x>0) scale(ans,v,l/sqrt(x)); return ans; }
vector3* rotateX(vector3* ans, vector3* v, T r) { T s=sin(r); T c=cos(r); ans.y=v.y*c+v.z*s; ans.z=-v.y*s+v.z*c; return ans; }
vector3* rotateY(vector3* ans, vector3* v, T r) { T s=sin(r); T c=cos(r); ans.z=v.z*c+v.x*s; ans.x=-v.z*s+v.x*c; return ans; }
vector3* rotateZ(vector3* ans, vector3* v, T r) { T s=sin(r); T c=cos(r); ans.x=v.x*c+v.y*s; ans.y=-v.x*s+v.y*c; return ans; }
vector3* affine(vector3* ans, vector3* v, vector2* affx, vector2* affy) { ans.x=affx.x*v.x+affy.x*v.y; ans.y=affx.y*v.x+affy.y*v.y; return ans; }
vector3* cross(vector3* ans, vector3* v0, vector3* v1) { ans.x=v0.y*v1.z-v0.z*v1.y; ans.y=v0.z*v1.x-v0.x*v1.z; ans.z=v0.x*v1.y-v0.y*v1.x; return ans; }

T length(vector3* v) { return sqrt(v.x*v.x+v.y*v.y+v.z*v.z); }
T length2(vector3* v) { return v.x*v.x+v.y*v.y+v.z*v.z; }
T atanX(vector3* v) { return atan2(v.z, v.y); }
T atanY(vector3* v) { return atan2(v.x, v.z); }
T atanZ(vector3* v) { return atan2(v.y, v.x); }
T dot(vector3* v0, vector3* v1) { return v0.x*v1.x+v0.y*v1.y+v0.z*v1.z; }




//--------------------------------------------------------------------------------
// 4 dimension vector parallel calculation
//--------------------------------------------------------------------------------
vector4* operate(vector4* v, void delegate(inout T) fn) { fn(v.x); fn(v.y); fn(v.z); fn(v.w); return v; }
vector4* operate(vector4* dst, vector4* src, void delegate(inout T, T) fn) { fn(dst.x,src.x); fn(dst.y,src.y); fn(dst.z,src.z); fn(dst.w,src.w); return dst; }
vector4* operate(vector4* ans, vector4* dst, vector4* src, T delegate(T, T) fn) { ans.x=fn(dst.x,src.x); ans.y=fn(dst.y,src.y); ans.z=fn(dst.z,src.z); ans.z=fn(dst.w,src.w); return ans; }
vector4* set(vector4* v, T x, T y)           { v.x=x; v.y=y; return v; }
vector4* set(vector4* v, T x, T y, T z)      { v.x=x; v.y=y; v.z=z; return v; }
vector4* set(vector4* v, T x, T y, T z, T w) { v.x=x; v.y=y; v.z=z; v.w=w; return v; }
vector4* copy(vector4* dst, vector2* src) { dst.x=src.x; dst.y=src.y; return dst; }
vector4* copy(vector4* dst, vector3* src) { dst.x=src.x; dst.y=src.y; dst.z=src.z; return dst; }
vector4* copy(vector4* dst, vector4* src) { dst.x=src.x; dst.y=src.y; dst.z=src.z; dst.w=src.w; return dst; }
vector4* copyMin(vector4* ans, vector4* v0, vector4* v1) { ans.x=(v0.x<v1.x)?v0.x:v1.x; ans.y=(v0.y<v1.y)?v0.y:v1.y; ans.z=(v0.z<v1.z)?v0.z:v1.z; ans.w=(v0.w<v1.w)?v0.w:v1.w; return ans; }
vector4* copyMax(vector4* ans, vector4* v0, vector4* v1) { ans.x=(v0.x>v1.x)?v0.x:v1.x; ans.y=(v0.y>v1.y)?v0.y:v1.y; ans.z=(v0.z>v1.z)?v0.z:v1.z; ans.w=(v0.w>v1.w)?v0.w:v1.w; return ans; }
vector4* copyLimit(vector4* ans, vector4* val, vector4* vmin, vector4* vmax) { return copyMax(ans, copyMin(ans, val, vmax), vmin); }
vector4* copyLimit01(vector4* ans, vector4* v) { ans.x=(v.x>=1)?1:(v.x>0)?v.x:0; ans.y=(v.y>=1)?1:(v.y>0)?v.y:0; ans.z=(v.z>=1)?1:(v.z>0)?v.z:0; ans.w=(v.w>=1)?1:(v.w>0)?v.w:0; return ans; }
bool pisZero(vector4* v) { return (v.x==0) && (v.y==0) && (v.z==0) && (v.w==0); }
bool pisNormalize(vector4* v) { return (length2Quart(v)==0); }
bool pisEqual(vector4* v0, vector4* v1) { return (v0.x==v1.x) && (v0.y==v1.y) && (v0.z==v1.z) && (v0.w==v1.w); }

version(LittleEndian) {
vector4* setUint(vector4* v, uint ui) { const T s=0.00392156862745098; v.x=(ui&255)*s; v.y=((ui>>8)&255)*s; v.z=((ui>>16)&255)*s; v.w=((ui>>24)&255)*s; return v; }
uint getUint(vector4* v) { return cast(uint)((cast(ubyte)(v.x*255))|((cast(ubyte)(v.y*255))<<8)|((cast(ubyte)(v.z*255))<<16)|((cast(ubyte)(v.w*255))<<24)); }
}
version(BigEndian) {
vector4* setUint(vector4* v, uint ui) { const T s=0.00392156862745098; v.x=((ui>>24)&255)*s; v.y=((ui>>16)&255)*s; v.z=((ui>>8)&255)*s; v.w=(ui&255)*s; return v; }
uint getUint(vector4* v) { return cast(uint)(((cast(ubyte)(v.x*255))<<24)|((cast(ubyte)(v.y*255))<<16)|((cast(ubyte)(v.z*255))<<8)|(cast(ubyte)(v.w*255))); }
}

vector4* neg(vector4* v) { v.x=-v.x; v.y=-v.y; v.z=-v.z; v.w=-v.w; return v; }
vector4* add(vector4* dst, vector4* src) { dst.x+=src.x; dst.y+=src.y; dst.z+=src.z; dst.w+=src.w; return dst; }
vector4* add(vector4* dst, T x, T y, T z, T w) { dst.x+=x; dst.y+=y; dst.z+=z; dst.w+=w; return dst;}
vector4* add(vector4* dst, vector4* src, T t) { dst.x+=src.x*t; dst.y+=src.y*t; dst.z+=src.z*t; dst.w+=src.w*t; return dst; }
vector4* add(vector4* dst, T t0, vector4* src, T t1) { dst.x*=t0; dst.x+=src.x*t1; dst.y*=t0; dst.y+=src.y*t1; dst.z*=t0; dst.z+=src.z*t1; dst.w*=t0; dst.w+=src.w*t1; return dst; }
vector4* blend(vector4* v0, vector4* v1, T t) { return add(v0, 1-t, v1, t); }
vector4* sub(vector4* dst, vector4* src) { dst.x-=src.x; dst.y-=src.y; dst.z-=src.z; dst.w-=src.w; return dst; }
vector4* scale(vector4* dst, T s) { dst.x*=s; dst.y*=s; dst.z*=s; dst.w*=s; return dst; }
vector4* scale(vector4* dst, vector4* src) { dst.x*=src.x; dst.y*=src.y; dst.z*=src.z; dst.w*=src.w; return dst; }
vector4* scale(vector4* dst, T x, T y, T z, T w) { dst.x*=x; dst.y*=y; dst.z*=z; dst.w*=w; return dst; }
vector4* scaleinv(vector4* dst, vector4* src) { dst.x/=src.x; dst.y/=src.y; dst.z/=src.z; dst.w/=src.w; return dst; }
vector4* scaleinv(vector4* dst, T x, T y, T z, T w) { dst.x/=x; dst.y/=y; dst.z/=z; dst.w/=w; return dst; }
vector4* mod(vector4* dst, vector4* src) { dst.x-=floor(dst.x/src.x)*src.x; dst.y-=floor(dst.y/src.y)*src.y; dst.z-=floor(dst.z/src.z)*src.z; dst.w-=floor(dst.w/src.w)*src.w; return dst; }
vector4* mod1(vector4* dst) { dst.x-=floor(dst.x); dst.y-=floor(dst.y); dst.z-=floor(dst.z); dst.w-=floor(dst.w); return dst; }

vector4* neg(vector4* ans, vector4* v) { ans.x=-v.x; ans.y=-v.y; ans.z=-v.z; ans.w=-v.w; return ans; }
vector4* add(vector4* ans, vector4* dst, vector4* src) { ans.x=dst.x+src.x; ans.y=dst.y+src.y; ans.z=dst.z+src.z; ans.w=dst.w+src.w; return ans; }
vector4* add(vector4* ans, vector4* dst, T x, T y, T z, T w) { ans.x=dst.x+x; ans.y=dst.y+y; ans.z=dst.z+z; ans.w=dst.w+w; return ans; }
vector4* add(vector4* ans, vector4* dst, vector4* src, T t) { ans.x=dst.x+src.x*t; ans.y=dst.y+src.y*t; ans.z=dst.z+src.z*t; ans.w=dst.w+src.w*t; return ans; }
vector4* add(vector4* ans, vector4* v0, T t0, vector4* v1, T t1) {  ans.x=v0.x*t0+v1.x*t1; ans.y=v0.y*t0+v1.y*t1; ans.z=v0.z*t0+v1.z*t1; ans.w=v0.w*t0+v1.w*t1; return ans; }
vector4* blend(vector4* ans, vector4* v0, vector4* v1, T t) { return add(ans, v0, 1-t, v1, t); }
vector4* sub(vector4* ans, vector4* dst, vector4* src) { ans.x=dst.x-src.x; ans.y=dst.y-src.y; ans.z=dst.z-src.z; ans.w=dst.w-src.w; return ans; }
vector4* scale(vector4* ans, vector4* dst, T s) { ans.x=dst.x*s; ans.y=dst.y*s; ans.z=dst.z*s; ans.w=dst.w*s; return ans; }
vector4* scale(vector4* ans, vector4* dst, vector4* src) { ans.x=dst.x*src.x; ans.y=dst.y*src.y; ans.z=dst.z*src.z; ans.w=dst.w*src.w; return ans; }
vector4* scale(vector4* ans, vector4* dst, T x, T y, T z, T w) { ans.x=dst.x*x; ans.y=dst.y*y; ans.z=dst.z*z; ans.w=dst.w*w; return ans; }
vector4* scaleinv(vector4* ans, vector4* dst, vector4* src) { ans.x=dst.x/src.x; ans.y=dst.y/src.y; ans.z=dst.z/src.z; ans.w=dst.w/src.w; return ans; }
vector4* scaleinv(vector4* ans, vector4* dst, T x, T y, T z, T w) { ans.x=dst.x/x; ans.y=dst.y/y; ans.z=dst.z/z; ans.w=dst.w/w; return ans; }
vector4* mod(vector4* ans, vector4* dst, vector4* src) { ans.x=dst.x-floor(dst.x/src.x)*src.x; ans.y=dst.y-floor(dst.y/src.y)*src.y; ans.z=dst.z-floor(dst.z/src.z)*src.z; ans.w=dst.w-floor(dst.w/src.w)*src.w; return ans; }
vector4* mod1(vector4* ans, vector4* dst) { ans.x=dst.x-floor(dst.x); ans.y=dst.y-floor(dst.y); ans.z=dst.z-floor(dst.z); ans.w=dst.w-floor(dst.w); return ans; }




//--------------------------------------------------------------------------------
// quarternion calculation
//--------------------------------------------------------------------------------
// load quarternion by axis and angle
vector4* setQuart(vector4* q, vector3* ax, T r)   { r*=0.5; scale(normalize(cast(vector3*)q,ax),sin(r)); q.w=cos(r); return q; }
vector4* setQuart(vector4* q, T x, T y, T z, T r) { vector3 ax; return setQuart(q, set(&ax,x,y,z),r); }

// load FPS posture quarternion multiplyed already (bank(z-axis) -> pitch(x-axis) -> head(y-axis))
vector4* setQuartFPS(vector4* q, T h, T p, T b)
{
    h*=0.5f; p*=0.5f; b*=0.5f; T sh=sin(h); T sp=sin(p); T sb=sin(b); T ch=cos(h); T cp=cos(p); T cb=cos(b);
    return set(q, cb*sp*ch-sb*cp*sh, cb*cp*sh+sb*sp*ch, sb*cp*ch-cb*sp*sh, cb*cp*ch+sb*sp*sh);
}

// load top view posture quarternion multiplyed already (bank(y-axis) -> pitch(x-axis) -> head(z-axis))
vector4* setQuartTopview(vector4* q, T h, T p, T b)
{
    h*=0.5f; p*=0.5f; b*=0.5f; T sh=sin(h); T sp=sin(p); T sb=sin(b); T ch=cos(h); T cp=cos(p); T cb=cos(b);
    return set(q, cb*sp*ch-sb*cp*sh, sb*cp*ch-cb*sp*sh, cb*cp*sh+sb*sp*ch, cb*cp*ch+sb*sp*sh);
}

// quarternion interpolation
vector4* blendQuart(vector4* q, vector4* q0, vector4* q1, T t)
{
    T qr = q0.x*q1.x + q0.y*q1.y + q0.z*q1.z + q0.w*q1.w;
    T ss = 1 - qr*qr;
    if (ss==0) {
        return copy(q,q0);
    } else {
        T isp = 1/sqrt(ss);
        T ph = acos(qr);
        T pt = ph * t;
        return add(q,q0,sin(pt)*isp,q1,sin(ph-pt)*isp);
    }
}

// quarternion multiply
vector4* mul(vector4* qL,  vector4* qR) { vector4 v; return copy(qL, mul(&v, qL, qR)); }
vector4* mul(vector4* ans, vector4* qL, vector4* qR)
{
    ans.x = qL.w * qR.x + qL.x * qR.w + qL.y * qR.z - qL.z * qR.y;
    ans.y = qL.w * qR.y + qL.y * qR.w + qL.z * qR.x - qL.x * qR.z;
    ans.z = qL.w * qR.z + qL.z * qR.w + qL.x * qR.y - qL.y * qR.x;
    ans.w = qL.w * qR.w - dot(cast(vector3*)qL,cast(vector3*)qR);
    return ans;
}

// quarternion inverting multiply
vector4* div(vector4* qL,  vector4* qR) { vector4 v; return copy(qL, div(&v, qL, qR)); }
vector4* div(vector4* ans, vector4* qL, vector4* qR)
{
    T l=length2Quart(qR); if (l==0) return copy(ans, qL); l=1/l;
    ans.x = (- qL.w * qR.x + qL.x * qR.w - qL.y * qR.z + qL.z * qR.y) * l;
    ans.y = (- qL.w * qR.y + qL.y * qR.w - qL.z * qR.x + qL.x * qR.z) * l;
    ans.z = (- qL.w * qR.z + qL.z * qR.w - qL.x * qR.y + qL.y * qR.x) * l;
    ans.w = (qL.w * qR.w + dot(cast(vector3*)qL,cast(vector3*)qR)) * l;
    return ans;
}

// 4 dimension length and normalize
T lengthQuart(vector4* q) { return sqrt(q.x*q.x+q.y*q.y+q.z*q.z+q.w*q.w); }
T length2Quart(vector4* q) { return q.x*q.x+q.y*q.y+q.z*q.z+q.w*q.w; }
vector4* normalizeQuart(vector4* q) { T x=length2Quart(q); if (x!=0) return scale(q,1/sqrt(x)); return q; }
vector4* normalizeQuart(vector4* ans, vector4* q) { T x=length2Quart(q); if (x!=0) return scale(q,1/sqrt(x)); return q; }

// rotation by quarternion
vector3* rotate(vector3* ans, vector3* v, vector4* q) {
    T xx=q.x*q.x*2; T yy=q.y*q.y*2; T zz=q.z*q.z*2; T xy=q.x*q.y*2; T yz=q.y*q.z*2; T zx=q.z*q.x*2; T wx=q.w*q.x*2; T wy=q.w*q.y*2; T wz=q.w*q.z*2;
    return set(ans, (1-(yy+zz))*v.x+(xy-wz)*v.y+(zx+wy)*v.z, (xy+wz)*v.x+(1-(xx+zz))*v.y+(yz-wx)*v.z, (zx-wy)*v.x+(yz+wx)*v.y+(1-(xx+yy))*v.z);
}
vector3* rotate(vector3* v, vector4* q) { vector3 b; return copy(v, rotate(&b,v,q)); }
vector4* rotate(vector4* v, vector4* q) { vector4 b; return copy(v, rotate(&b,v,q)); }
vector4* rotate(vector4* ans, vector4* v, vector4* q) { rotate(cast(vector3*)ans, cast(vector3*)v, q); return ans; }





//--------------------------------------------------------------------------------
// interpolating calculation
//--------------------------------------------------------------------------------
// linear interpolation, set start-point x0 and end-point x1.
vector4* interLinear(vector4* v, T x0, T x1) { return set(v, 0, 0, x1-x0, x0); }

// 2nd-dimension bezier interpolation, set start x0, end x1 and control-point p.
vector4* interBezier(vector4* v, T x0, T x1, T p) { return set(v, 0, x0+x1-p*2, (p-x0)*2, x0); }

// 3rd-dimension bezier interpolation, set start x0, end x1 and control-point p0, p1.
vector4* interBezier(vector4* v, T x0, T x1, T p0, T p1) { return set(v, -x0+(p0-p1)*3+x1, (x0-p0*2+p1)*3, (-x1+p1)*3, x1); }

// ferguson-coons interpolation, set position x0, x1 and velocity v0, v1.
vector4* interFergusonCoons(vector4* v, T x0, T x1, T v0, T v1)
{
    v.x=(x0-x1)*2+v0+v1;  v.y=-x0+x1-v0-v.x;  v.z=v0; v.w=x0;  return v;
}

// 3nd-dimension lagrangian interpolation, interpolate between x1 and x2 by 4 points.
vector4* interLagrange(vector4* v, T x0, T x1, T x2, T x3)
{
    v.x=x3-x2-x0+x1;  v.y=x0-x1-v.x;  v.z=x2-x0;  v.w=x1;  return v;
}

// catmull-rom interpolation, interpolate between x1 and x2 by 4 points, keeping velocity of each point.
// This calculation is expansion of ferguson-coons interpolation.
vector4* interCatmullRom(vector4* v, T x0, T x1, T x2, T x3)
{
    return set(v, (-x0+x1-x2+x3)*0.5+x1-x2, x0+(x2-x1)*2-(x1+x3)*0.5, (-x0+x2)*0.5, x1);
}

// catmull-rom interpolation for starting point.
vector4* interCatmullRomStart(vector4* v, T x1, T x2, T x3) { return set(v, 0, (x1+x3)*0.5-x2, (x2-x1)*2+(x1-x3)*0.5, x1); }

// catmull-rom interpolation for ending point.
vector4* interCatmullRomEnd(vector4* v, T x0, T x1, T x2) { return set(v, 0, (x0+x2)*0.5-x1, (x2-x0)*0.5, x1); }

// calculate interpolation
T        inter(T t,        vector4* i) { return (((i.x*t+i.y)*t+i.z)*t)+i.w; }
vector2* inter(vector2* v, vector4* i) { return set(v, inter(v.x,i), inter(v.y,i)); }
vector3* inter(vector3* v, vector4* i) { return set(v, inter(v.x,i), inter(v.y,i), inter(v.z,i)); }
vector4* inter(vector4* v, vector4* i) { return set(v, inter(v.x,i), inter(v.y,i), inter(v.z,i), inter(v.w,i)); }




//--------------------------------------------------------------------------------
// 2D matrix calculation
//--------------------------------------------------------------------------------
matrix2* set(matrix2* m, T x00, T x01, T x10, T x11) { m.a00=x00;  m.a01=x01; m.a10=x10;  m.a11=x11; return m; }
matrix2* copy(matrix2* dst, matrix2* src) { if (src==null) return null; *dst=*src; return dst; }
T prop(matrix2* m) { return m.a00*m.a11 - m.a01*m.a10; }


matrix2* loadZero(matrix2* m)     { return set(m,0,0,0,0); }
matrix2* loadIdentity(matrix2* m) { return set(m,1,0,0,1); }
matrix2* loadScale(matrix2* m, vector2* v) { return set(m,v.x,0,0,v.y); }
matrix2* loadScale(matrix2* m, T x, T y)   { return set(m,x,0,0,y); }
matrix2* loadRotate(matrix2* m, T r) { T s=sin(r); T c=cos(r); return set(m,c,-s,s,c); }
matrix2* loadAffine(matrix2* m, vector2* vx, vector2* vy) { return set(m,vx.x,vx.y,vy.x,vy.y); }

matrix2* invert(matrix2* m) { matrix2 b; return copy(m,invert(&b,m)); }
matrix2* invert(matrix2* ans, matrix2* m) {
    T p=prop(m); if (p==0) { return null; } p=1/p;
    ans.a00= m.a11*p;    ans.a01=-m.a01*p;
    ans.a10=-m.a10*p;    ans.a11= m.a00*p;
    return ans;
}

matrix2* mul(matrix2* dst, matrix2* src) { matrix2 b; return copy(dst, mul(&b, dst, src)); }
matrix2* mul(matrix2* ans, matrix2* mL, matrix2* mR) {
    ans.a00=mL.a00*mR.a00 + mL.a01*mR.a10;    ans.a01=mL.a00*mR.a01 + mL.a01*mR.a11;
    ans.a10=mL.a10*mR.a00 + mL.a11*mR.a10;    ans.a11=mL.a10*mR.a01 + mL.a11*mR.a11;
    return ans;
}

matrix2* scale(matrix2* m, vector2* v) { m.a00*=v.x; m.a01*=v.y; m.a10*=v.x; m.a11*=v.y; return m; }
matrix2* scale(matrix2* m, T x, T y)   { m.a00*=x; m.a01*=y; m.a10*=x; m.a11*=y; return m; }
matrix2* scale(matrix2* ans, matrix2* m, vector2* v) { return scale(copy(ans, m), v); }
matrix2* scale(matrix2* ans, matrix2* m, T x, T y)   { return scale(copy(ans, m), x, y); }

matrix2* rotate(matrix2* m, T r) { matrix2 b; return copy(m,rotate(&b,m,r)); }
matrix2* rotate(matrix2* ans, matrix2* m, T r) {
    T s=sin(r); T c=cos(r);
    ans.a00= m.a00*c + m.a01*s;    ans.a01=-m.a00*s + m.a01*c;
    ans.a10= m.a10*c + m.a11*s;    ans.a11=-m.a10*s + m.a11*c;
    return ans;
}

matrix2* affine(matrix2* m, vector2* vx, vector2* vy) { matrix2 b; return copy(m,affine(&b,m,vx,vy)); }
matrix2* affine(matrix2* ans, matrix2* m, vector2* vx, vector2* vy) {
    ans.a00=m.a00*vx.x + m.a01*vy.x;    ans.a01=m.a00*vx.y + m.a01*vy.y;
    ans.a10=m.a10*vx.x + m.a11*vy.x;    ans.a11=m.a10*vx.y + m.a11*vy.y;
    return ans;
}


vector2* mul(vector2* v, matrix2* m) { T x_=v.x; v.x=m.a00*v.x+m.a10*v.y; v.y=m.a01*x_+m.a11*v.y; return v; }
vector3* mul(vector3* v, matrix2* m) { T x_=v.x; v.x=m.a00*v.x+m.a10*v.y; v.y=m.a01*x_+m.a11*v.y; return v; }
vector4* mul(vector4* v, matrix2* m) { T x_=v.x; v.x=m.a00*v.x+m.a10*v.y; v.y=m.a01*x_+m.a11*v.y; return v; }
vector2* mul(vector2* ans, matrix2* mL, vector2* vR) { ans.x=mL.a00*vR.x+mL.a10*vR.y; ans.y=mL.a01*vR.x+mL.a11*vR.y; return ans; }
vector3* mul(vector3* ans, matrix2* mL, vector3* vR) { ans.x=mL.a00*vR.x+mL.a10*vR.y; ans.y=mL.a01*vR.x+mL.a11*vR.y; return ans; }
vector4* mul(vector4* ans, matrix2* mL, vector4* vR) { ans.x=mL.a00*vR.x+mL.a10*vR.y; ans.y=mL.a01*vR.x+mL.a11*vR.y; return ans; }




//--------------------------------------------------------------------------------
// 3D matrix calculation
//--------------------------------------------------------------------------------
matrix4* set(matrix4* m, T[] x) { m.e[]=x[]; return m; }
matrix4* copy(matrix4* dst, matrix4* src) { if (src==null) return null; *dst=*src; return dst; }

T prop(matrix4* m) {
    T a20a31_a21a30 = m.a20*m.a31 - m.a21*m.a30;
    T a20a32_a22a30 = m.a20*m.a32 - m.a22*m.a30;
    T a20a33_a23a30 = m.a20*m.a33 - m.a23*m.a30;
    T a21a32_a22a31 = m.a21*m.a32 - m.a22*m.a31;
    T a21a33_a23a31 = m.a21*m.a33 - m.a23*m.a31;
    T a22a33_a23a32 = m.a22*m.a33 - m.a23*m.a32;
    return   m.a00* (-m.a11* a22a33_a23a32 + m.a12* a21a33_a23a31 - m.a13* a21a32_a22a31)
           + m.a01* ( m.a10* a22a33_a23a32 - m.a12* a20a33_a23a30 + m.a13* a20a32_a22a30)
           + m.a02* (-m.a10* a21a33_a23a31 + m.a11* a20a33_a23a30 - m.a13* a20a31_a21a30)
           + m.a03* ( m.a10* a21a32_a22a31 - m.a11* a20a32_a22a30 + m.a12* a20a31_a21a30);
}

// proper value for m.a03=0, m.a13=0, m.a23=0
T prop_3(matrix4* m) {
    return (m.a00*(m.a12*m.a21-m.a11*m.a22) + m.a01*(m.a10*m.a22-m.a20*m.a12) + m.a02*(m.a11*m.a20-m.a10*m.a21))*m.a33;
}


matrix4* loadZero(matrix4* m)     { m.e[]=0; return m; }
matrix4* loadIdentity(matrix4* m) { m.e[]=0; m.a00=1; m.a11=1; m.a22=1; m.a33=1; return m; }
matrix4* loadTranslate(matrix4* m, vector3* v)    { loadIdentity(m); m.a30=v.x; m.a31=v.y; m.a32=v.z; return m; }
matrix4* loadTranslate(matrix4* m, T x, T y, T z) { loadIdentity(m); m.a30=x;   m.a31=y;   m.a32=z; return m; }
matrix4* loadScale(matrix4* m, vector3* v)    { m.e[]=0; m.a00=v.x; m.a11=v.y; m.a22=v.z; m.a33=1; return m; }
matrix4* loadScale(matrix4* m, T x, T y, T z) { m.e[]=0; m.a00=x;   m.a11=y;   m.a22=z;   m.a33=1; return m; }
matrix4* loadRotateX(matrix4* m, T r) { T s=sin(r); T c=cos(r); m.e[]=0; m.a11=c; m.a12=-s; m.a21=s; m.a22=c; m.a00=1; m.a33=1; return m; }
matrix4* loadRotateY(matrix4* m, T r) { T s=sin(r); T c=cos(r); m.e[]=0; m.a22=c; m.a20=-s; m.a02=s; m.a00=c; m.a11=1; m.a33=1; return m; }
matrix4* loadRotateZ(matrix4* m, T r) { T s=sin(r); T c=cos(r); m.e[]=0; m.a00=c; m.a01=-s; m.a10=s; m.a11=c; m.a22=1; m.a33=1; return m; }

// rotation by quarternion
matrix4* loadRotate(matrix4* m, vector4* q)
{
    T xx=q.x*q.x*2;    T yy=q.y*q.y*2;    T zz=q.z*q.z*2;
    T xy=q.x*q.y*2;    T yz=q.y*q.z*2;    T zx=q.z*q.x*2;
    T wx=q.w*q.x*2;    T wy=q.w*q.y*2;    T wz=q.w*q.z*2;
    m.a00 = 1-(yy+zz);   m.a01 = xy+wz;       m.a02 = zx-wy;
    m.a10 = xy-wz;       m.a11 = 1-(xx+zz);   m.a12 = yz+wx;
    m.a20 = zx+wy;       m.a21 = yz-wx;       m.a22 = 1-(xx+yy);
    m.a03=0;  m.a13=0;  m.a23=0;  m.a30=0;  m.a31=0;  m.a32=0;  m.a33=1;
    return m;
}

// rotation by axis & angle
matrix4* loadRotate(matrix4* m, vector3* ax, T r) {
    vector3 v; normalize(&v, ax);
    T s=sin(r); T c=cos(r); T ic=1-c;
    m.a00 = v.x*v.x*ic + c;      m.a01 = v.x*v.y*ic + v.z*s;  m.a02 = v.x*v.z*ic - v.y*s;
    m.a10 = v.y*v.x*ic - v.z*s;  m.a11 = v.y*v.y*ic + c;      m.a12 = v.y*v.z*ic + v.x*s;
    m.a20 = v.z*v.x*ic + v.y*s;  m.a21 = v.z*v.y*ic - v.x*s;  m.a22 = v.z*v.z*ic + c;
    m.a03=0;  m.a13=0;  m.a23=0;  m.a30=0;  m.a31=0;  m.a32=0;  m.a33=1;
    return m;
}

// rotation by axis & angle
matrix4* loadRotate(matrix4* m, T x, T y, T z, T r) {
    vector3 ax; return loadRotate(m, set(&ax, x, y, z), r);
}

// frustum_matrix(left, right, bottom, top, near, far)
matrix4* loadFrustum(matrix4* m, T l, T r, T b, T t, T n, T f)
{
    m.a00=(2*n)/(r-l);  m.a10=0;            m.a20=(r+l)/(r-l);  m.a30=0;
    m.a01=0;            m.a11=(2*n)/(t-b);  m.a21=(t+b)/(t-b);  m.a31=0;
    m.a02=0;            m.a12=0;            m.a22=-(f+n)/(f-n); m.a32=-(2*f*n)/(f-n);
    m.a03=0;            m.a13=0;            m.a23=-1;           m.a33=0;
    return m;
}

// perspective_matrix(fov, aspect, near, far)
matrix4* loadPerspective(matrix4* m, T fov, T asp, T n, T f)
{
    fov = cos(fov*0.5)/sin(fov*0.5);
    m.a00 = fov/asp;    m.a10 = 0;      m.a20 = 0;            m.a30 = 0;
    m.a01 = 0;          m.a11 = fov;    m.a21 = 0;            m.a31 = 0;
    m.a02 = 0;          m.a12 = 0;      m.a22 = -(f+n)/(f-n); m.a32 = -(2*f*n)/(f-n);
    m.a03 = 0;          m.a13 = 0;      m.a23 = -1;           m.a33 = 0;
    return m;
}

// invert(perspective_matrix(fov, aspect, near, far))
matrix4* loadPerspectiveInvert(matrix4* m, T fov, T asp, T n, T f)
{
    fov = sin(fov*0.5)/cos(fov*0.5);
    m.a00 = -asp*fov;   m.a10 = 0;      m.a20 = 0;             m.a30 = 0;
    m.a01 = 0;          m.a11 = -fov;   m.a21 = 0;             m.a31 = 0;
    m.a02 = 0;          m.a12 = 0;      m.a22 = 0;             m.a32 = 1;
    m.a03 = 0;          m.a13 = 0;      m.a23 = (f-n)/(2*f*n); m.a33 = -(f+n)/(2*f*n);
    return m;
}

// matrix2 -> matrix4
matrix4* loadMatrixX(matrix4* m, matrix2* m2d)
{
    m.a11=m2d.a00;  m.a12=m2d.a01;  m.a21=m2d.a10;  m.a22=m2d.a11;  m.a31=0;  m.a32=0;
    m.a00=1;  m.a01=0;  m.a02=0;  m.a10=0;  m.a20=0;
    m.a03=0;  m.a13=0;  m.a23=0;  m.a30=0;  m.a33=1;
    return m;
}
matrix4* loadMatrixY(matrix4* m, matrix2* m2d)
{
    m.a22=m2d.a00;  m.a20=m2d.a01;  m.a02=m2d.a10;  m.a00=m2d.a11;  m.a32=0;  m.a30=0;
    m.a11=1;  m.a01=0;  m.a21=0;  m.a10=0;  m.a12=0;
    m.a03=0;  m.a13=0;  m.a23=0;  m.a31=0;  m.a33=1;
    return m;
}
matrix4* loadMatrixZ(matrix4* m, matrix2* m2d)
{
    m.a00=m2d.a00;  m.a01=m2d.a01;  m.a10=m2d.a10;  m.a11=m2d.a11;  m.a30=0;  m.a31=0;
    m.a22=1;  m.a02=0;  m.a12=0;  m.a20=0;  m.a21=0;
    m.a03=0;  m.a13=0;  m.a23=0;  m.a32=0;  m.a33=1;
    return m;
}

// matrix4 -> matrix2
matrix2* getMatrixX(matrix2* m2d, matrix4* m) { return set(m2d, m.a11, m.a12, m.a21, m.a22); }
matrix2* getMatrixY(matrix2* m2d, matrix4* m) { return set(m2d, m.a22, m.a20, m.a02, m.a00); }
matrix2* getMatrixZ(matrix2* m2d, matrix4* m) { return set(m2d, m.a00, m.a01, m.a10, m.a11); }


matrix4* transpose(matrix4* m) { matrix4 b; return copy(m, transpose(&b, m)); }
matrix4* transpose(matrix4* ans, matrix4* m)
{
    ans.a00=m.a00;  ans.a01=m.a10;  ans.a02=m.a20;  ans.a03=m.a30;
    ans.a10=m.a01;  ans.a11=m.a11;  ans.a12=m.a21;  ans.a13=m.a31;
    ans.a20=m.a02;  ans.a21=m.a12;  ans.a22=m.a22;  ans.a23=m.a32;
    ans.a30=m.a03;  ans.a31=m.a13;  ans.a32=m.a23;  ans.a33=m.a33;
    return ans;
}

matrix4* invert(matrix4* m) { matrix4 b; return copy(m, invert(&b, m)); }
matrix4* invert(matrix4* ans, matrix4* m)
{
    T p=prop(m); if (p==0) { return null; } p=1/p;
    T a01a12_a02a11 = m.a01*m.a12 - m.a02*m.a11;
    T a01a13_a03a11 = m.a01*m.a13 - m.a03*m.a11;
    T a02a13_a03a12 = m.a02*m.a13 - m.a03*m.a12;
    T a21a32_a22a31 = m.a21*m.a32 - m.a22*m.a31;
    T a21a33_a23a31 = m.a21*m.a33 - m.a23*m.a31;
    T a22a33_a23a32 = m.a22*m.a33 - m.a23*m.a32;
    ans.a00 = (-m.a11* a22a33_a23a32 + m.a12* a21a33_a23a31 - m.a13* a21a32_a22a31) * p;
    ans.a01 = ( m.a01* a22a33_a23a32 - m.a02* a21a33_a23a31 + m.a03* a21a32_a22a31) * p;
    ans.a02 = (-m.a31* a02a13_a03a12 + m.a32* a01a13_a03a11 - m.a33* a01a12_a02a11) * p;
    ans.a03 = ( m.a21* a02a13_a03a12 - m.a22* a01a13_a03a11 + m.a23* a01a12_a02a11) * p;

    T a00a11_a01a10 = m.a00*m.a11 - m.a01*m.a10;
    T a00a12_a02a10 = m.a00*m.a12 - m.a02*m.a10;
    T a00a13_a03a10 = m.a00*m.a13 - m.a03*m.a10;
    T a20a31_a21a30 = m.a20*m.a31 - m.a21*m.a30;
    T a20a32_a22a30 = m.a20*m.a32 - m.a22*m.a30;
    T a20a33_a23a30 = m.a20*m.a33 - m.a23*m.a30;
    ans.a10 = ( m.a10* a22a33_a23a32 - m.a12* a20a33_a23a30 + m.a13* a20a32_a22a30) * p;
    ans.a11 = (-m.a00* a22a33_a23a32 + m.a02* a20a33_a23a30 - m.a03* a20a32_a22a30) * p;
    ans.a12 = ( m.a30* a02a13_a03a12 - m.a32* a00a13_a03a10 + m.a33* a00a12_a02a10) * p;
    ans.a13 = (-m.a20* a02a13_a03a12 + m.a22* a00a13_a03a10 - m.a23* a00a12_a02a10) * p;

    ans.a20 = (-m.a10* a21a33_a23a31 + m.a11* a20a33_a23a30 - m.a13* a20a31_a21a30) * p;
    ans.a21 = ( m.a00* a21a33_a23a31 - m.a01* a20a33_a23a30 + m.a03* a20a31_a21a30) * p;
    ans.a22 = (-m.a30* a01a13_a03a11 + m.a31* a00a13_a03a10 - m.a33* a00a11_a01a10) * p;
    ans.a23 = ( m.a20* a01a13_a03a11 - m.a21* a00a13_a03a10 + m.a23* a00a11_a01a10) * p;

    ans.a30 = ( m.a10* a21a32_a22a31 - m.a11* a20a32_a22a30 + m.a12* a20a31_a21a30) * p;
    ans.a31 = (-m.a00* a21a32_a22a31 + m.a01* a20a32_a22a30 - m.a02* a20a31_a21a30) * p;
    ans.a32 = ( m.a30* a01a12_a02a11 - m.a31* a00a12_a02a10 + m.a32* a00a11_a01a10) * p;
    ans.a33 = (-m.a20* a01a12_a02a11 + m.a21* a00a12_a02a10 - m.a22* a00a11_a01a10) * p;
    return ans;
}

// invert for m.a03=0, m.a13=0, m.a23=0
matrix4* invert_3(matrix4* m) { matrix4 b; return copy(m, invert_3(&b, m)); }
matrix4* invert_3(matrix4* ans, matrix4* m)
{
    T p=prop_3(m); if (p==0) { return null; } p=1/p; T mp=m.a33*p;
    ans.a00 = (m.a12*m.a21 - m.a11*m.a22) * mp;
    ans.a01 = (m.a01*m.a22 - m.a02*m.a21) * mp;
    ans.a02 = (m.a02*m.a11 - m.a01*m.a12) * mp;
    ans.a10 = (m.a10*m.a22 - m.a20*m.a12) * mp;
    ans.a11 = (m.a02*m.a20 - m.a00*m.a22) * mp;
    ans.a12 = (m.a00*m.a12 - m.a10*m.a02) * mp;
    ans.a20 = (m.a11*m.a20 - m.a10*m.a21) * mp;
    ans.a21 = (m.a00*m.a21 - m.a01*m.a20) * mp;
    ans.a22 = (m.a01*m.a10 - m.a00*m.a11) * mp;
    T a20a31_a21a30 = m.a20*m.a31 - m.a21*m.a30;
    T a20a32_a22a30 = m.a20*m.a32 - m.a22*m.a30;
    T a21a32_a22a31 = m.a21*m.a32 - m.a22*m.a31;
    T a00a11_a01a10 = m.a00*m.a11 - m.a01*m.a10;
    T a00a12_a02a10 = m.a00*m.a12 - m.a02*m.a10;
    T a01a12_a02a11 = m.a01*m.a12 - m.a02*m.a11;
    ans.a30 = ( m.a10* a21a32_a22a31 - m.a11* a20a32_a22a30 + m.a12* a20a31_a21a30) * p;
    ans.a31 = (-m.a00* a21a32_a22a31 + m.a01* a20a32_a22a30 - m.a02* a20a31_a21a30) * p;
    ans.a32 = ( m.a30* a01a12_a02a11 - m.a31* a00a12_a02a10 + m.a32* a00a11_a01a10) * p;
    ans.a33 = (-m.a20* a01a12_a02a11 + m.a21* a00a12_a02a10 - m.a22* a00a11_a01a10) * p;
    ans.a03 = 0; ans.a13 = 0; ans.a23 = 0;
    return ans;
}

matrix4* mul(matrix4* dst, matrix4* mR) { matrix4 b; return copy(dst, mul(&b, dst, mR)); }
matrix4* mul(matrix4* ans, matrix4* mL, matrix4* mR)
{
    ans.a00 = mR.a00 * mL.a00 + mR.a01 * mL.a10 + mR.a02 * mL.a20 + mR.a03 * mL.a30;
    ans.a01 = mR.a00 * mL.a01 + mR.a01 * mL.a11 + mR.a02 * mL.a21 + mR.a03 * mL.a31;
    ans.a02 = mR.a00 * mL.a02 + mR.a01 * mL.a12 + mR.a02 * mL.a22 + mR.a03 * mL.a32;
    ans.a03 = mR.a00 * mL.a03 + mR.a01 * mL.a13 + mR.a02 * mL.a23 + mR.a03 * mL.a33;
    ans.a10 = mR.a10 * mL.a00 + mR.a11 * mL.a10 + mR.a12 * mL.a20 + mR.a13 * mL.a30;
    ans.a11 = mR.a10 * mL.a01 + mR.a11 * mL.a11 + mR.a12 * mL.a21 + mR.a13 * mL.a31;
    ans.a12 = mR.a10 * mL.a02 + mR.a11 * mL.a12 + mR.a12 * mL.a22 + mR.a13 * mL.a32;
    ans.a13 = mR.a10 * mL.a03 + mR.a11 * mL.a13 + mR.a12 * mL.a23 + mR.a13 * mL.a33;
    ans.a20 = mR.a20 * mL.a00 + mR.a21 * mL.a10 + mR.a22 * mL.a20 + mR.a23 * mL.a30;
    ans.a21 = mR.a20 * mL.a01 + mR.a21 * mL.a11 + mR.a22 * mL.a21 + mR.a23 * mL.a31;
    ans.a22 = mR.a20 * mL.a02 + mR.a21 * mL.a12 + mR.a22 * mL.a22 + mR.a23 * mL.a32;
    ans.a23 = mR.a20 * mL.a03 + mR.a21 * mL.a13 + mR.a22 * mL.a23 + mR.a23 * mL.a33;
    ans.a30 = mR.a30 * mL.a00 + mR.a31 * mL.a10 + mR.a32 * mL.a20 + mR.a33 * mL.a30;
    ans.a31 = mR.a30 * mL.a01 + mR.a31 * mL.a11 + mR.a32 * mL.a21 + mR.a33 * mL.a31;
    ans.a32 = mR.a30 * mL.a02 + mR.a31 * mL.a12 + mR.a32 * mL.a22 + mR.a33 * mL.a32;
    ans.a33 = mR.a30 * mL.a03 + mR.a31 * mL.a13 + mR.a32 * mL.a23 + mR.a33 * mL.a33;
    return ans;
}

// multply for m.a03=0, m.a13=0, m.a23=0
matrix4* mul_3(matrix4* dst, matrix4* mR) { matrix4 b; return copy(dst, mul_3(&b, dst, mR)); }
matrix4* mul_3(matrix4* ans, matrix4* mL, matrix4* mR)
{
    ans.a00 = mR.a00 * mL.a00 + mR.a01 * mL.a10 + mR.a02 * mL.a20;
    ans.a01 = mR.a00 * mL.a01 + mR.a01 * mL.a11 + mR.a02 * mL.a21;
    ans.a02 = mR.a00 * mL.a02 + mR.a01 * mL.a12 + mR.a02 * mL.a22;
    ans.a03 = 0;
    ans.a10 = mR.a10 * mL.a00 + mR.a11 * mL.a10 + mR.a12 * mL.a20;
    ans.a11 = mR.a10 * mL.a01 + mR.a11 * mL.a11 + mR.a12 * mL.a21;
    ans.a12 = mR.a10 * mL.a02 + mR.a11 * mL.a12 + mR.a12 * mL.a22;
    ans.a13 = 0;
    ans.a20 = mR.a20 * mL.a00 + mR.a21 * mL.a10 + mR.a22 * mL.a20;
    ans.a21 = mR.a20 * mL.a01 + mR.a21 * mL.a11 + mR.a22 * mL.a21;
    ans.a22 = mR.a20 * mL.a02 + mR.a21 * mL.a12 + mR.a22 * mL.a22;
    ans.a23 = 0;
    ans.a30 = mR.a30 * mL.a00 + mR.a31 * mL.a10 + mR.a32 * mL.a20 + mR.a33 * mL.a30;
    ans.a31 = mR.a30 * mL.a01 + mR.a31 * mL.a11 + mR.a32 * mL.a21 + mR.a33 * mL.a31;
    ans.a32 = mR.a30 * mL.a02 + mR.a31 * mL.a12 + mR.a32 * mL.a22 + mR.a33 * mL.a32;
    ans.a33 = mR.a33 * mL.a33;
    return ans;
}

matrix4* translate(matrix4* m, vector3* v)
{
    m.a30 += v.x * m.a00 + v.y * m.a10 + v.z * m.a20;
    m.a31 += v.x * m.a01 + v.y * m.a11 + v.z * m.a21;
    m.a32 += v.x * m.a02 + v.y * m.a12 + v.z * m.a22;
    m.a33 += v.x * m.a03 + v.y * m.a13 + v.z * m.a23;
    return m;
}
matrix4* translate(matrix4* m, T x, T y, T z) { vector3 v; return translate(m, set(&v, x, y, z)); }
matrix4* translate(matrix4* ans, matrix4* m, vector3* v) { return translate(copy(ans, m), v); }
matrix4* translate(matrix4* ans, matrix4* m, T x, T y, T z) { return translate(copy(ans, m), x, y, z); }

matrix4* scale(matrix4* m, vector3* v)    { scale(&m.v[0], v.x); scale(&m.v[1], v.y); scale(&m.v[2], v.z); return m; }
matrix4* scale(matrix4* m, T x, T y, T z) { scale(&m.v[0], x);   scale(&m.v[1], y);   scale(&m.v[2], z);   return m; }
matrix4* scale(matrix4* ans, matrix4* m, vector3* v)    { return scale(copy(ans, m), v); }
matrix4* scale(matrix4* ans, matrix4* m, T x, T y, T z) { return scale(copy(ans, m), x, y, z); }

matrix4* rotate(matrix4* m, vector4* q)         { matrix4 b; return copy(m, rotate(&b, m, q)); }
matrix4* rotate(matrix4* m, vector3* ax, T r)   { matrix4 b; return copy(m, rotate(&b, m, ax, r)); }
matrix4* rotate(matrix4* m, T x, T y, T z, T r) { matrix4 b; return copy(m, rotate(&b, m, x, y, z, r)); }
matrix4* rotate(matrix4* ans, matrix4* m, vector4* q)         { matrix4 b; return mul(ans, m, loadRotate(&b, q)); }
matrix4* rotate(matrix4* ans, matrix4* m, vector3* ax, T r)   { matrix4 b; return mul(ans, m, loadRotate(&b, ax, r)); }
matrix4* rotate(matrix4* ans, matrix4* m, T x, T y, T z, T r) { matrix4 b; return mul(ans, m, loadRotate(&b, z, y, z, r)); }
matrix4* rotateX(matrix4* m, T r) { matrix4 b; return copy(m, rotateX(&b, m, r)); }
matrix4* rotateX(matrix4* ans, matrix4* m, T r)
{
    T s = sin(r);    T c = cos(r);
    ans.a10 =  c * m.a10 + s * m.a20;
    ans.a11 =  c * m.a11 + s * m.a21;
    ans.a12 =  c * m.a12 + s * m.a22;
    ans.a13 =  c * m.a13 + s * m.a23;
    ans.a20 = -s * m.a10 + c * m.a20;
    ans.a21 = -s * m.a11 + c * m.a21;
    ans.a22 = -s * m.a12 + c * m.a22;
    ans.a23 = -s * m.a13 + c * m.a23;
    ans.a00 = m.a00;  ans.a01 = m.a01;  ans.a02 = m.a02;  ans.a03 = m.a03;
    ans.a30 = m.a30;  ans.a31 = m.a31;  ans.a32 = m.a32;  ans.a33 = m.a33;
    return ans;
}
matrix4* rotateY(matrix4* m, T r) { matrix4 b; return copy(m, rotateY(&b, m, r)); }
matrix4* rotateY(matrix4* ans, matrix4* m, T r)
{
    T s = sin(r);    T c = cos(r);
    ans.a00 = c * m.a00 - s * m.a20;
    ans.a01 = c * m.a01 - s * m.a21;
    ans.a02 = c * m.a02 - s * m.a22;
    ans.a03 = c * m.a03 - s * m.a23;
    ans.a20 = s * m.a00 + c * m.a20;
    ans.a21 = s * m.a01 + c * m.a21;
    ans.a22 = s * m.a02 + c * m.a22;
    ans.a23 = s * m.a03 + c * m.a23;
    ans.a10 = m.a10;  ans.a11 = m.a11;  ans.a12 = m.a12;  ans.a13 = m.a13;
    ans.a30 = m.a30;  ans.a31 = m.a31;  ans.a32 = m.a32;  ans.a33 = m.a33;
    return ans;
}
matrix4* rotateZ(matrix4* m, T r) { matrix4 b; return copy(m, rotateZ(&b, m, r)); }
matrix4* rotateZ(matrix4* ans, matrix4* m, T r)
{
    T s = sin(r);    T c = cos(r);
    ans.a00 =  c * m.a00 + s * m.a10;
    ans.a01 =  c * m.a01 + s * m.a11;
    ans.a02 =  c * m.a02 + s * m.a12;
    ans.a03 =  c * m.a03 + s * m.a13;
    ans.a10 = -s * m.a00 + c * m.a10;
    ans.a11 = -s * m.a01 + c * m.a11;
    ans.a12 = -s * m.a02 + c * m.a12;
    ans.a13 = -s * m.a03 + c * m.a13;
    ans.a20 = m.a20;  ans.a21 = m.a21;  ans.a22 = m.a22;  ans.a23 = m.a23;
    ans.a30 = m.a30;  ans.a31 = m.a31;  ans.a32 = m.a32;  ans.a33 = m.a33;
    return ans;
}


vector3* mul(vector3* v,   matrix4* m) { vector3 b; return copy(v, mul(&b, m, v)); }
vector3* mul(vector3* ans, matrix4* mL, vector3* vR)
{
    return set(ans, mL.a00*vR.x + mL.a10*vR.y + mL.a20*vR.z + mL.a30,
                    mL.a01*vR.x + mL.a11*vR.y + mL.a21*vR.z + mL.a31,
                    mL.a02*vR.x + mL.a12*vR.y + mL.a22*vR.z + mL.a32);
}

vector3* mul_w0(vector3* v,   matrix4* m) { vector3 b; return copy(v, mul_w0(&b, m, v)); }
vector3* mul_w0(vector3* ans, matrix4* mL, vector3* vR)
{
    return set(ans, mL.a00*vR.x + mL.a10*vR.y + mL.a20*vR.z,
                    mL.a01*vR.x + mL.a11*vR.y + mL.a21*vR.z,
                    mL.a02*vR.x + mL.a12*vR.y + mL.a22*vR.z);
}

vector4* mul(vector4* v,   matrix4* m) { vector4 b; return copy(v, mul(&b, m, v)); }
vector4* mul(vector4* ans, matrix4* mL, vector4* vR)
{
    return set(ans, mL.a00*vR.x + mL.a10*vR.y + mL.a20*vR.z + mL.a30*vR.w,
                    mL.a01*vR.x + mL.a11*vR.y + mL.a21*vR.z + mL.a31*vR.w,
                    mL.a02*vR.x + mL.a12*vR.y + mL.a22*vR.z + mL.a32*vR.w,
                    mL.a03*vR.x + mL.a13*vR.y + mL.a23*vR.z + mL.a33*vR.w);
}

}




mixin TVector!(float);




//--------------------------------------------------------------------------------
// utilities
//--------------------------------------------------------------------------------
// calculate distance
float calcDistance(vector2* v0, vector2* v1) {
    vector2 v;
    return length(sub(&v, v0, v1));
}

// calculate distance^2
float calcDistance2(vector2* v0, vector2* v1) {
    vector2 v;
    return length2(sub(&v, v0, v1));
}

// calculate distance
float calcDistance(vector3* v0, vector3* v1) {
    vector3 v;
    return length(sub(&v, v0, v1));
}

// calculate distance^2
float calcDistance2(vector3* v0, vector3* v1) {
    vector3 v;
    return length2(sub(&v, v0, v1));
}

// calculate direction
vector2* calcDirection(vector2* ans, vector2* v0, vector2* v1) {
    return normalize(sub(ans, v0, v1));
}

vector3* calcDirection(vector3* ans, vector3* v0, vector3* v1) {
    return normalize(sub(ans, v0, v1));
}

// calculate normal vector
vector3* calcNormal(vector3* ans, vector3* v0, vector3* v1) {
    return normalize(cross(ans, v0, v1));
}

vector3* calcNormal(vector3* ans, vector3* v0, vector3* v1, vector3* v2) {
    vector3 d1, d2;
    return normalize(cross(ans, sub(&d1, v1, v0), sub(&d2, v2, v0)));
}

