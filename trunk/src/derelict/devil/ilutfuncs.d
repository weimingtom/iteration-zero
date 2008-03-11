/*
 * Copyright (c) 2004-2007 Derelict Developers
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the names 'Derelict', 'DerelictILUT', nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
module derelict.devil.ilutfuncs;


private
{
    import derelict.devil.iltypes;
    import derelict.devil.iluttypes;
    import derelict.util.loader;
    import derelict.opengl.gltypes;
}


package void loadILUT(SharedLib lib)
{
    bindFunc(ilutDisable)("ilutDisable", lib);
    bindFunc(ilutEnable)("ilutEnable", lib);
    bindFunc(ilutGetBoolean)("ilutGetBoolean", lib);
    bindFunc(ilutGetBooleanv)("ilutGetBooleanv", lib);
    bindFunc(ilutGetInteger)("ilutGetInteger", lib);
    bindFunc(ilutGetIntegerv)("ilutGetIntegerv", lib);
    bindFunc(ilutGetString)("ilutGetString", lib);
    bindFunc(ilutInit)("ilutInit", lib);
    bindFunc(ilutIsDisabled)("ilutIsDisabled", lib);
    bindFunc(ilutIsEnabled)("ilutIsEnabled", lib);
    bindFunc(ilutPopAttrib)("ilutPopAttrib", lib);
    bindFunc(ilutPushAttrib)("ilutPushAttrib", lib);
    bindFunc(ilutSetInteger)("ilutSetInteger", lib);
    bindFunc(ilutRenderer)("ilutRenderer", lib);
    bindFunc(ilutGLBindTexImage)("ilutGLBindTexImage", lib);
    bindFunc(ilutGLBindMipmaps)("ilutGLBindMipmaps", lib);
    bindFunc(ilutGLBuildMipmaps)("ilutGLBuildMipmaps", lib);
    bindFunc(ilutGLLoadImage)("ilutGLLoadImage", lib);
    bindFunc(ilutGLScreen)("ilutGLScreen", lib);
    bindFunc(ilutGLScreenie)("ilutGLScreenie", lib);
    bindFunc(ilutGLSaveImage)("ilutGLSaveImage", lib);
    bindFunc(ilutGLSetTex)("ilutGLSetTex", lib);
    bindFunc(ilutGLTexImage)("ilutGLTexImage", lib);
    //bindFunc(ilutConvertToSDLSurface)("ilutConvertToSDLSurface", lib);
    //bindFunc(ilutSDLSurfaceLoadImage)("ilutSDLSurfaceLoadImage", lib);
    //bindFunc(ilutSDLSurfaceFromBitmap)("ilutSDLSurfaceFromBitmap", lib);

    version (Windows)
    {
        bindFunc(ilutConvertToHBitmap)("ilutConvertToHBitmap", lib);
        bindFunc(ilutFreePaddedData)("ilutFreePaddedData", lib);
        bindFunc(ilutGetBmpInfo)("ilutGetBmpInfo", lib);
        bindFunc(ilutGetHPal)("ilutGetHPal", lib);
        bindFunc(ilutGetPaddedData)("ilutGetPaddedData", lib);
        bindFunc(ilutGetWinClipboard)("ilutGetWinClipboard", lib);
        bindFunc(ilutLoadResource)("ilutLoadResource", lib);
        bindFunc(ilutSetHBitmap)("ilutSetHBitmap", lib);
        bindFunc(ilutSetHPal)("ilutSetHPal", lib);
        bindFunc(ilutSetWinClipboard)("ilutSetWinClipboard", lib);
        bindFunc(ilutWinLoadImage)("ilutWinLoadImage", lib);
        bindFunc(ilutWinLoadUrl)("ilutWinLoadUrl", lib);
        bindFunc(ilutWinPrint)("ilutWinPrint", lib);
        bindFunc(ilutWinSaveImage)("ilutWinSaveImage", lib);
    }
}


GenericLoader DerelictILUT;
static this() {
    DerelictILUT.setup(
        "ilut.dll",
        "libILUT.so",
        "",
        &loadILUT
    );
}


private const char[] Funcs =
"
    typedef ILboolean        function(ILenum Mode) pfilutDisable;
    typedef ILboolean        function(ILenum Mode) pfilutEnable;
    typedef ILboolean        function(ILenum Mode) pfilutGetBoolean;
    typedef ILvoid       function(ILenum Mode, ILboolean *Param) pfilutGetBooleanv;
    typedef ILint        function(ILenum Mode) pfilutGetInteger;
    typedef ILvoid       function(ILenum Mode, ILint *Param) pfilutGetIntegerv;
    typedef ILstring     function(ILenum StringName) pfilutGetString;
    typedef ILvoid       function() pfilutInit;
    typedef ILboolean        function(ILenum Mode) pfilutIsDisabled;
    typedef ILboolean        function(ILenum Mode) pfilutIsEnabled;
    typedef ILvoid       function() pfilutPopAttrib;
    typedef ILvoid       function(ILuint Bits) pfilutPushAttrib;
    typedef ILvoid       function(ILenum Mode, ILint Param) pfilutSetInteger;
    typedef ILboolean    function(ILenum Renderer) pfilutRenderer;
    typedef GLuint   function() pfilutGLBindTexImage;
    typedef GLuint   function() pfilutGLBindMipmaps;
    typedef ILboolean    function() pfilutGLBuildMipmaps;
    typedef GLuint   function( ILstring FileName) pfilutGLLoadImage;
    typedef ILboolean    function() pfilutGLScreen;
    typedef ILboolean    function() pfilutGLScreenie;
    typedef ILboolean    function( ILstring FileName, GLuint TexID) pfilutGLSaveImage;
    typedef ILboolean    function(GLuint TexID) pfilutGLSetTex;
    typedef ILboolean    function(GLuint Level) pfilutGLTexImage;
";

version(Windows)
{
    extern(Windows): mixin(Funcs);
}
else
{
    extern(C): mixin(Funcs);
}

pfilutDisable ilutDisable;
pfilutEnable ilutEnable;
pfilutGetBoolean ilutGetBoolean;
pfilutGetBooleanv ilutGetBooleanv;
pfilutGetInteger ilutGetInteger;
pfilutGetIntegerv ilutGetIntegerv;
pfilutGetString ilutGetString;
pfilutInit ilutInit;
pfilutIsDisabled ilutIsDisabled;
pfilutIsEnabled ilutIsEnabled;
pfilutPopAttrib ilutPopAttrib;
pfilutPushAttrib ilutPushAttrib;
pfilutSetInteger ilutSetInteger;
pfilutRenderer ilutRenderer;
pfilutGLBindTexImage ilutGLBindTexImage;
pfilutGLBindMipmaps ilutGLBindMipmaps;
pfilutGLBuildMipmaps ilutGLBuildMipmaps;
pfilutGLLoadImage ilutGLLoadImage;
pfilutGLScreen ilutGLScreen;
pfilutGLScreenie ilutGLScreenie;
pfilutGLSaveImage ilutGLSaveImage;
pfilutGLSetTex ilutGLSetTex;
pfilutGLTexImage ilutGLTexImage;

// ImageLib Utility Toolkit's SDL Functions
//typedef SDL_Surface*   function(uint flags) pfilutConvertToSDLSurface;
//typedef SDL_Surface*   function( ILstring FileName) pfilutSDLSurfaceLoadImage;
//typedef ILboolean      function(SDL_Surface *Bitmap) pfilutSDLSurfaceFromBitmap;
//pfilutConvertToSDLSurface ilutConvertToSDLSurface;
//pfilutSDLSurfaceLoadImage ilutSDLSurfaceLoadImage;
//pfilutSDLSurfaceFromBitmap ilutSDLSurfaceFromBitmap;


// ImageLib Utility Toolkit's Win32 GDI Functions
version(Windows)
{
    version(Tango)
    {
        private import tango.sys.Common;
    }
    else
    {
        private import std.c.windows.windows;
    }

    extern(Windows):
    typedef HBITMAP function(HDC hDC) pfilutConvertToHBitmap;
    typedef ILvoid  function(ILubyte *Data) pfilutFreePaddedData;
    typedef ILvoid  function(BITMAPINFO *Info) pfilutGetBmpInfo;
    typedef HPALETTE    function() pfilutGetHPal;
    typedef ILubyte*    function() pfilutGetPaddedData;
    typedef ILboolean   function() pfilutGetWinClipboard;
    typedef ILboolean   function(HINSTANCE hInst, ILint ID,  ILstring ResourceType, ILenum Type) pfilutLoadResource;
    typedef ILboolean   function(HBITMAP Bitmap) pfilutSetHBitmap;
    typedef ILboolean   function(HPALETTE Pal) pfilutSetHPal;
    typedef ILboolean   function() pfilutSetWinClipboard;
    typedef HBITMAP function( ILstring FileName, HDC hDC) pfilutWinLoadImage;
    typedef ILboolean   function( ILstring Url) pfilutWinLoadUrl;
    typedef ILboolean function(ILuint XPos, ILuint YPos, ILuint Width, ILuint Height, HDC hDC) pfilutWinPrint;
    typedef ILboolean   function( ILstring FileName, HBITMAP Bitmap) pfilutWinSaveImage;
    pfilutConvertToHBitmap ilutConvertToHBitmap;
    pfilutFreePaddedData ilutFreePaddedData;
    pfilutGetBmpInfo ilutGetBmpInfo;
    pfilutGetHPal ilutGetHPal;
    pfilutGetPaddedData ilutGetPaddedData;
    pfilutGetWinClipboard ilutGetWinClipboard;
    pfilutLoadResource ilutLoadResource;
    pfilutSetHBitmap ilutSetHBitmap;
    pfilutSetHPal ilutSetHPal;
    pfilutSetWinClipboard ilutSetWinClipboard;
    pfilutWinLoadImage ilutWinLoadImage;
    pfilutWinLoadUrl ilutWinLoadUrl;
    pfilutWinPrint ilutWinPrint;
    pfilutWinSaveImage ilutWinSaveImage;
}
