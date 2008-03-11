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
 * * Neither the names 'Derelict', 'DerelictIL', nor the names of its contributors
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
module derelict.devil.ilfuncs;


private
{
    import derelict.devil.iltypes;
    import derelict.util.loader;
}


package void loadIL(SharedLib lib)
{
    bindFunc(ilActiveImage)("ilActiveImage", lib);
    bindFunc(ilActiveLayer)("ilActiveLayer", lib);
    bindFunc(ilActiveMipmap)("ilActiveMipmap", lib);
    bindFunc(ilApplyPal)("ilApplyPal", lib);
    bindFunc(ilApplyProfile)("ilApplyProfile", lib);
    bindFunc(ilBindImage)("ilBindImage", lib);
    bindFunc(ilBlit)("ilBlit", lib);
    bindFunc(ilClearColour)("ilClearColour", lib);
    bindFunc(ilClearImage)("ilClearImage", lib);
    bindFunc(ilCloneCurImage)("ilCloneCurImage", lib);
    bindFunc(ilCompressFunc)("ilCompressFunc", lib);
    bindFunc(ilConvertImage)("ilConvertImage", lib);
    bindFunc(ilConvertPal)("ilConvertPal", lib);
    bindFunc(ilCopyImage)("ilCopyImage", lib);
    bindFunc(ilCopyPixels)("ilCopyPixels", lib);
    bindFunc(ilCreateSubImage)("ilCreateSubImage", lib);
    bindFunc(ilDefaultImage)("ilDefaultImage", lib);
    bindFunc(ilDeleteImages)("ilDeleteImages", lib);
    bindFunc(ilDisable)("ilDisable", lib);
    bindFunc(ilEnable)("ilEnable", lib);
    bindFunc(ilFormatFunc)("ilFormatFunc", lib);
    bindFunc(ilGenImages)("ilGenImages", lib);
    bindFunc(ilGetAlpha)("ilGetAlpha", lib);
    //bindFunc(ilModAlpha)("ilModAlpha", lib);
    //bindFunc(ilSetAlpha)("ilSetAlpha", lib);
    bindFunc(ilGetBoolean)("ilGetBoolean", lib);
    bindFunc(ilGetBooleanv)("ilGetBooleanv", lib);
    bindFunc(ilGetData)("ilGetData", lib);
    bindFunc(ilGetDXTCData)("ilGetDXTCData", lib);
    bindFunc(ilGetError)("ilGetError", lib);
    bindFunc(ilGetInteger)("ilGetInteger", lib);
    bindFunc(ilGetIntegerv)("ilGetIntegerv", lib);
    bindFunc(ilGetLumpPos)("ilGetLumpPos", lib);
    bindFunc(ilGetPalette)("ilGetPalette", lib);
    bindFunc(ilGetString)("ilGetString", lib);
    bindFunc(ilHint)("ilHint", lib);
    bindFunc(ilInit)("ilInit", lib);
    bindFunc(ilIsDisabled)("ilIsDisabled", lib);
    bindFunc(ilIsEnabled)("ilIsEnabled", lib);
    bindFunc(ilIsImage)("ilIsImage", lib);
    bindFunc(ilIsValid)("ilIsValid", lib);
    bindFunc(ilIsValidF)("ilIsValidF", lib);
    bindFunc(ilIsValidL)("ilIsValidL", lib);
    bindFunc(ilKeyColour)("ilKeyColour", lib);
    bindFunc(ilLoad)("ilLoad", lib);
    bindFunc(ilLoadF)("ilLoadF", lib);
    bindFunc(ilLoadImage)("ilLoadImage", lib);
    bindFunc(ilLoadL)("ilLoadL", lib);
    bindFunc(ilLoadPal)("ilLoadPal", lib);
    bindFunc(ilOriginFunc)("ilOriginFunc", lib);
    bindFunc(ilOverlayImage)("ilOverlayImage", lib);
    bindFunc(ilPopAttrib)("ilPopAttrib", lib);
    bindFunc(ilPushAttrib)("ilPushAttrib", lib);
    bindFunc(ilRegisterFormat)("ilRegisterFormat", lib);
    bindFunc(ilRegisterLoad)("ilRegisterLoad", lib);
    bindFunc(ilRegisterMipNum)("ilRegisterMipNum", lib);
    bindFunc(ilRegisterNumImages)("ilRegisterNumImages", lib);
    bindFunc(ilRegisterOrigin)("ilRegisterOrigin", lib);
    bindFunc(ilRegisterPal)("ilRegisterPal", lib);
    bindFunc(ilRegisterSave)("ilRegisterSave", lib);
    bindFunc(ilRegisterType)("ilRegisterType", lib);
    bindFunc(ilRemoveLoad)("ilRemoveLoad", lib);
    bindFunc(ilRemoveSave)("ilRemoveSave", lib);
    bindFunc(ilResetMemory)("ilResetMemory", lib);
    bindFunc(ilResetRead)("ilResetRead", lib);
    bindFunc(ilResetWrite)("ilResetWrite", lib);
    bindFunc(ilSave)("ilSave", lib);
    bindFunc(ilSaveF)("ilSaveF", lib);
    bindFunc(ilSaveImage)("ilSaveImage", lib);
    bindFunc(ilSetData)("ilSetData", lib);
    bindFunc(ilSetDuration)("ilSetDuration", lib);
    bindFunc(ilSetInteger)("ilSetInteger", lib);
    bindFunc(ilSetMemory)("ilSetMemory", lib);
    bindFunc(ilSetPixels)("ilSetPixels", lib);
    bindFunc(ilSetRead)("ilSetRead", lib);
    bindFunc(ilSetString)("ilSetString", lib);
    bindFunc(ilSetWrite)("ilSetWrite", lib);
    bindFunc(ilShutDown)("ilShutDown", lib);
    bindFunc(ilTexImage)("ilTexImage", lib);
    bindFunc(ilTypeFunc)("ilTypeFunc", lib);
    bindFunc(ilLoadData)("ilLoadData", lib);
    bindFunc(ilLoadDataF)("ilLoadDataF", lib);
    bindFunc(ilLoadDataL)("ilLoadDataL", lib);
    bindFunc(ilSaveData)("ilSaveData", lib);
    bindFunc(ilLoadFromJpegStruct)("ilLoadFromJpegStruct", lib);
    bindFunc(ilSaveFromJpegStruct)("ilSaveFromJpegStruct", lib);
}


GenericLoader DerelictIL;
static this() {
    DerelictIL.setup(
        "devil.dll",
        "libIL.so",
        "",
        &loadIL
    );
}



private const char[] Funcs =
"
    alias void* ILHANDLE;
    alias ILvoid        (*fCloseRProc)(ILHANDLE);
    alias ILboolean (*fEofProc)   (ILHANDLE);
    alias ILint     (*fGetcProc)  (ILHANDLE);
    alias ILHANDLE  (*fOpenRProc) ( ILstring);
    alias ILint     (*fReadProc)  (void*, ILuint, ILuint, ILHANDLE);
    alias ILint     (*fSeekRProc) (ILHANDLE, ILint, ILint);
    alias ILint     (*fTellRProc) (ILHANDLE);
    alias ILvoid        (*fCloseWProc)(ILHANDLE);
    alias ILHANDLE  (*fOpenWProc) ( ILstring);
    alias ILint     (*fPutcProc)  (ILubyte, ILHANDLE);
    alias ILint     (*fSeekWProc) (ILHANDLE, ILint, ILint);
    alias ILint     (*fTellWProc) (ILHANDLE);
    alias ILint     (*fWriteProc) ( void*, ILuint, ILuint, ILHANDLE);
    alias ILvoid*       (*mAlloc)(ILuint);
    alias ILvoid        (*mFree) (ILvoid*);
    alias ILenum        (*IL_LOADPROC)( ILstring);
    alias ILenum        (*IL_SAVEPROC)( ILstring);

    typedef ILboolean       function(ILuint Number) pfilActiveImage;
    typedef ILboolean       function(ILuint Number) pfilActiveLayer;
    typedef ILboolean       function(ILuint Number) pfilActiveMipmap;
    typedef ILboolean       function( ILstring FileName) pfilApplyPal;
    typedef ILboolean       function( ILstring InProfile,  ILstring OutProfile) pfilApplyProfile;
    typedef ILvoid      function(ILuint Image) pfilBindImage;
    typedef ILboolean       function(ILuint Source, ILint DestX, ILint DestY, ILint DestZ, ILuint SrcX, ILuint SrcY, ILuint SrcZ, ILuint Width, ILuint Height, ILuint Depth) pfilBlit;
    typedef ILvoid      function(ILclampf Red, ILclampf Green, ILclampf Blue, ILclampf Alpha) pfilClearColour;
    typedef ILboolean       function() pfilClearImage;
    typedef ILuint      function() pfilCloneCurImage;
    typedef ILboolean       function(ILenum Mode) pfilCompressFunc;
    typedef ILboolean       function(ILenum DestFormat, ILenum DestType) pfilConvertImage;
    typedef ILboolean       function(ILenum DestFormat) pfilConvertPal;
    typedef ILboolean       function(ILuint Src) pfilCopyImage;
    typedef ILuint      function(ILuint XOff, ILuint YOff, ILuint ZOff, ILuint Width, ILuint Height, ILuint Depth, ILenum Format, ILenum Type, ILvoid *Data) pfilCopyPixels;
    typedef ILuint      function(ILenum Type, ILuint Num) pfilCreateSubImage;
    typedef ILboolean       function() pfilDefaultImage;
    typedef ILvoid      function(ILsizei Num,  ILuint *Images) pfilDeleteImages;
    typedef ILboolean       function(ILenum Mode) pfilDisable;
    typedef ILboolean       function(ILenum Mode) pfilEnable;
    typedef ILboolean       function(ILenum Mode) pfilFormatFunc;
    typedef ILvoid      function(ILsizei Num, ILuint *Images) pfilGenImages;
    typedef ILubyte*        function(ILenum Type) pfilGetAlpha;
    //typedef ILvoid            function( ILint AlphaValue ) pfilModAlpha;
    //typedef ILvoid            function( ILuint AlphaValue ) pfilSetAlpha;
    typedef ILboolean       function(ILenum Mode) pfilGetBoolean;
    typedef ILvoid      function(ILenum Mode, ILboolean *Param) pfilGetBooleanv;
    typedef ILubyte*        function() pfilGetData;
    typedef ILuint      function(ILvoid *Buffer, ILuint BufferSize, ILenum DXTCFormat) pfilGetDXTCData;
    typedef ILenum      function() pfilGetError;
    typedef ILint             function(ILenum Mode) pfilGetInteger;
    typedef ILvoid      function(ILenum Mode, ILint *Param) pfilGetIntegerv;
    typedef ILuint      function() pfilGetLumpPos;
    typedef ILubyte*        function() pfilGetPalette;
    typedef ILstring    function(ILenum StringName) pfilGetString;
    typedef ILvoid      function(ILenum Target, ILenum Mode) pfilHint;
    typedef ILvoid      function() pfilInit;
    typedef ILboolean       function(ILenum Mode) pfilIsDisabled;
    typedef ILboolean       function(ILenum Mode) pfilIsEnabled;
    typedef ILboolean       function(ILuint Image) pfilIsImage;
    typedef ILboolean       function(ILenum Type,  ILstring FileName) pfilIsValid;
    typedef ILboolean       function(ILenum Type, ILHANDLE File) pfilIsValidF;
    typedef ILboolean       function(ILenum Type, ILvoid *Lump, ILuint Size) pfilIsValidL;
    typedef ILvoid      function(ILclampf Red, ILclampf Green, ILclampf Blue, ILclampf Alpha) pfilKeyColour;
    typedef ILboolean       function(ILenum Type,  ILstring FileName) pfilLoad;
    typedef ILboolean       function(ILenum Type, ILHANDLE File) pfilLoadF;
    typedef ILboolean       function( ILstring FileName) pfilLoadImage;
    typedef ILboolean       function(ILenum Type, ILvoid *Lump, ILuint Size) pfilLoadL;
    typedef ILboolean       function( ILstring FileName) pfilLoadPal;
    typedef ILboolean       function(ILenum Mode) pfilOriginFunc;
    typedef ILboolean       function(ILuint Source, ILint XCoord, ILint YCoord, ILint ZCoord) pfilOverlayImage;
    typedef ILvoid      function() pfilPopAttrib;
    typedef ILvoid      function(ILuint Bits) pfilPushAttrib;
    typedef ILvoid      function(ILenum Format) pfilRegisterFormat;
    typedef ILboolean       function( ILstring Ext, IL_LOADPROC Load) pfilRegisterLoad;
    typedef ILboolean       function(ILuint Num) pfilRegisterMipNum;
    typedef ILboolean       function(ILuint Num) pfilRegisterNumImages;
    typedef ILvoid      function(ILenum Origin) pfilRegisterOrigin;
    typedef ILvoid      function(ILvoid *Pal, ILuint Size, ILenum Type) pfilRegisterPal;
    typedef ILboolean       function( ILstring Ext, IL_SAVEPROC Save) pfilRegisterSave;
    typedef ILvoid      function(ILenum Type) pfilRegisterType;
    typedef ILboolean       function( ILstring Ext) pfilRemoveLoad;
    typedef ILboolean       function( ILstring Ext) pfilRemoveSave;
    typedef ILvoid      function() pfilResetMemory;
    typedef ILvoid      function() pfilResetRead;
    typedef ILvoid      function() pfilResetWrite;
    typedef ILboolean       function(ILenum Type,  ILstring FileName) pfilSave;
    typedef ILuint      function(ILenum Type, ILHANDLE File) pfilSaveF;
    typedef ILboolean       function( ILstring FileName) pfilSaveImage;
    typedef ILboolean       function(ILvoid *Data) pfilSetData;
    typedef ILboolean       function(ILuint Duration) pfilSetDuration;
    typedef ILvoid      function(ILenum Mode, ILint Param) pfilSetInteger;
    typedef ILvoid      function(mAlloc, mFree) pfilSetMemory;
    typedef ILvoid      function(ILint XOff, ILint YOff, ILint ZOff, ILuint Width, ILuint Height, ILuint Depth, ILenum Format, ILenum Type, ILvoid *Data) pfilSetPixels;
    typedef ILvoid      function(fOpenRProc, fCloseRProc, fEofProc, fGetcProc, fReadProc, fSeekRProc, fTellRProc) pfilSetRead;
    typedef ILvoid      function(ILenum Mode,  char *String) pfilSetString;
    typedef ILvoid      function(fOpenWProc, fCloseWProc, fPutcProc, fSeekWProc, fTellWProc, fWriteProc) pfilSetWrite;
    typedef ILvoid      function() pfilShutDown;
    typedef ILboolean       function(ILuint Width, ILuint Height, ILuint Depth, ILubyte Bpp, ILenum Format, ILenum Type, ILvoid *Data) pfilTexImage;
    typedef ILboolean       function(ILenum Mode) pfilTypeFunc;
    typedef ILboolean       function( ILstring FileName, ILuint Width, ILuint Height, ILuint Depth, ILubyte Bpp) pfilLoadData;
    typedef ILboolean       function(ILHANDLE File, ILuint Width, ILuint Height, ILuint Depth, ILubyte Bpp) pfilLoadDataF;
    typedef ILboolean       function(ILvoid *Lump, ILuint Size, ILuint Width, ILuint Height, ILuint Depth, ILubyte Bpp) pfilLoadDataL;
    typedef ILboolean       function( ILstring FileName) pfilSaveData;
    typedef ILboolean       function(ILvoid* JpegDecompressorPtr) pfilLoadFromJpegStruct;
    typedef ILboolean       function(ILvoid* JpegCompressorPtr) pfilSaveFromJpegStruct;
";

version(Windows)
{
    extern(Windows): mixin(Funcs);
}
else
{
    extern(C): mixin(Funcs);
}

pfilActiveImage ilActiveImage;
pfilActiveLayer ilActiveLayer;
pfilActiveMipmap ilActiveMipmap;
pfilApplyPal ilApplyPal;
pfilApplyProfile ilApplyProfile;
pfilBindImage ilBindImage;
pfilBlit ilBlit;
pfilClearColour ilClearColour;
pfilClearImage ilClearImage;
pfilCloneCurImage ilCloneCurImage;
pfilCompressFunc ilCompressFunc;
pfilConvertImage ilConvertImage;
pfilConvertPal ilConvertPal;
pfilCopyImage ilCopyImage;
pfilCopyPixels ilCopyPixels;
pfilCreateSubImage ilCreateSubImage;
pfilDefaultImage ilDefaultImage;
pfilDeleteImages ilDeleteImages;
pfilDisable ilDisable;
pfilEnable ilEnable;
pfilFormatFunc ilFormatFunc;
pfilGenImages ilGenImages;
pfilGetAlpha ilGetAlpha;
//pfilModAlpha ilModAlpha;
//pfilSetAlpha ilSetAlpha;
pfilGetBoolean ilGetBoolean;
pfilGetBooleanv ilGetBooleanv;
pfilGetData ilGetData;
pfilGetDXTCData ilGetDXTCData;
pfilGetError ilGetError;
pfilGetInteger ilGetInteger;
pfilGetIntegerv ilGetIntegerv;
pfilGetLumpPos ilGetLumpPos;
pfilGetPalette ilGetPalette;
pfilGetString ilGetString;
pfilHint ilHint;
pfilInit ilInit;
pfilIsDisabled ilIsDisabled;
pfilIsEnabled ilIsEnabled;
pfilIsImage ilIsImage;
pfilIsValid ilIsValid;
pfilIsValidF ilIsValidF;
pfilIsValidL ilIsValidL;
pfilKeyColour ilKeyColour;
pfilLoad ilLoad;
pfilLoadF ilLoadF;
pfilLoadImage ilLoadImage;
pfilLoadL ilLoadL;
pfilLoadPal ilLoadPal;
pfilOriginFunc ilOriginFunc;
pfilOverlayImage ilOverlayImage;
pfilPopAttrib ilPopAttrib;
pfilPushAttrib ilPushAttrib;
pfilRegisterFormat ilRegisterFormat;
pfilRegisterLoad ilRegisterLoad;
pfilRegisterMipNum ilRegisterMipNum;
pfilRegisterNumImages ilRegisterNumImages;
pfilRegisterOrigin ilRegisterOrigin;
pfilRegisterPal ilRegisterPal;
pfilRegisterSave ilRegisterSave;
pfilRegisterType ilRegisterType;
pfilRemoveLoad ilRemoveLoad;
pfilRemoveSave ilRemoveSave;
pfilResetMemory ilResetMemory;
pfilResetRead ilResetRead;
pfilResetWrite ilResetWrite;
pfilSave ilSave;
pfilSaveF ilSaveF;
pfilSaveImage ilSaveImage;
pfilSetData ilSetData;
pfilSetDuration ilSetDuration;
pfilSetInteger ilSetInteger;
pfilSetMemory ilSetMemory;
pfilSetPixels ilSetPixels;
pfilSetRead ilSetRead;
pfilSetString ilSetString;
pfilSetWrite ilSetWrite;
pfilShutDown ilShutDown;
pfilTexImage ilTexImage;
pfilTypeFunc ilTypeFunc;
pfilLoadData ilLoadData;
pfilLoadDataF ilLoadDataF;
pfilLoadDataL ilLoadDataL;
pfilSaveData ilSaveData;


pfilLoadFromJpegStruct ilLoadFromJpegStruct;
pfilSaveFromJpegStruct ilSaveFromJpegStruct;

alias ilClearColour     ilClearColor;
alias ilKeyColour       ilKeyColor;
