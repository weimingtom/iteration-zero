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
 * * Neither the names 'Derelict', 'DerelictILU', nor the names of its contributors
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
module derelict.devil.ilufuncs;


private
{
    import derelict.devil.iltypes;
    import derelict.devil.ilutypes;
    import derelict.util.loader;
}


package void loadILU(SharedLib lib)
{
    bindFunc(iluAlienify)("iluAlienify", lib);
    bindFunc(iluBlurAvg)("iluBlurAvg", lib);
    bindFunc(iluBlurGaussian)("iluBlurGaussian", lib);
    bindFunc(iluBuildMipmaps)("iluBuildMipmaps", lib);
    bindFunc(iluColoursUsed)("iluColoursUsed", lib);
    bindFunc(iluCompareImage)("iluCompareImage", lib);
    bindFunc(iluContrast)("iluContrast", lib);
    bindFunc(iluCrop)("iluCrop", lib);
    bindFunc(iluDeleteImage)("iluDeleteImage", lib);
    bindFunc(iluEdgeDetectE)("iluEdgeDetectE", lib);
    bindFunc(iluEdgeDetectP)("iluEdgeDetectP", lib);
    bindFunc(iluEdgeDetectS)("iluEdgeDetectS", lib);
    bindFunc(iluEmboss)("iluEmboss", lib);
    bindFunc(iluEnlargeCanvas)("iluEnlargeCanvas", lib);
    bindFunc(iluEnlargeImage)("iluEnlargeImage", lib);
    bindFunc(iluEqualize)("iluEqualize", lib);
    bindFunc(iluErrorString)("iluErrorString", lib);
    bindFunc(iluFlipImage)("iluFlipImage", lib);
    bindFunc(iluGammaCorrect)("iluGammaCorrect", lib);
    bindFunc(iluGenImage)("iluGenImage", lib);
    bindFunc(iluGetImageInfo)("iluGetImageInfo", lib);
    bindFunc(iluGetInteger)("iluGetInteger", lib);
    bindFunc(iluGetIntegerv)("iluGetIntegerv", lib);
    bindFunc(iluGetString)("iluGetString", lib);
    bindFunc(iluImageParameter)("iluImageParameter", lib);
    bindFunc(iluInit)("iluInit", lib);
    bindFunc(iluInvertAlpha)("iluInvertAlpha", lib);
    bindFunc(iluLoadImage)("iluLoadImage", lib);
    bindFunc(iluMirror)("iluMirror", lib);
    bindFunc(iluNegative)("iluNegative", lib);
    bindFunc(iluNoisify)("iluNoisify", lib);
    bindFunc(iluPixelize)("iluPixelize", lib);
    bindFunc(iluRegionfv)("iluRegionfv", lib);
    bindFunc(iluRegioniv)("iluRegioniv", lib);
    bindFunc(iluReplaceColour)("iluReplaceColour", lib);
    bindFunc(iluRotate)("iluRotate", lib);
    bindFunc(iluRotate3D)("iluRotate3D", lib);
    bindFunc(iluSaturate1f)("iluSaturate1f", lib);
    bindFunc(iluSaturate4f)("iluSaturate4f", lib);
    bindFunc(iluScale)("iluScale", lib);
    bindFunc(iluScaleColours)("iluScaleColours", lib);
    bindFunc(iluSharpen)("iluSharpen", lib);
    bindFunc(iluSwapColours)("iluSwapColours", lib);
    bindFunc(iluWave)("iluWave", lib);
}


GenericLoader DerelictILU;
static this() {
    DerelictILU.setup(
        "ilu.dll",
        "libILU.so",
        "",
        &loadILU
    );
}


private const char[] Funcs =
"
    typedef ILboolean            function() pfiluAlienify;
    typedef ILboolean            function(ILuint Iter) pfiluBlurAvg;
    typedef ILboolean            function(ILuint Iter) pfiluBlurGaussian;
    typedef ILboolean            function() pfiluBuildMipmaps;
    typedef ILuint           function() pfiluColoursUsed;
    typedef ILboolean            function(ILuint Comp) pfiluCompareImage;
    typedef ILboolean            function(ILfloat Contrast) pfiluContrast;
    typedef ILboolean            function(ILuint XOff, ILuint YOff, ILuint ZOff, ILuint Width, ILuint Height, ILuint Depth) pfiluCrop;
    typedef ILvoid           function(ILuint Id) pfiluDeleteImage;
    typedef ILboolean            function() pfiluEdgeDetectE;
    typedef ILboolean            function() pfiluEdgeDetectP;
    typedef ILboolean            function() pfiluEdgeDetectS;
    typedef ILboolean            function() pfiluEmboss;
    typedef ILboolean            function(ILuint Width, ILuint Height, ILuint Depth) pfiluEnlargeCanvas;
    typedef ILboolean            function(ILfloat XDim, ILfloat YDim, ILfloat ZDim) pfiluEnlargeImage;
    typedef ILboolean            function() pfiluEqualize;
    typedef ILstring     function(ILenum Error) pfiluErrorString;
    typedef ILboolean            function() pfiluFlipImage;
    typedef ILboolean            function(ILfloat Gamma) pfiluGammaCorrect;
    typedef ILuint           function() pfiluGenImage;
    typedef ILvoid           function(ILinfo *Info) pfiluGetImageInfo;
    typedef ILint                function(ILenum Mode) pfiluGetInteger;
    typedef ILvoid           function(ILenum Mode, ILint *Param) pfiluGetIntegerv;
    typedef ILstring     function(ILenum StringName) pfiluGetString;
    typedef ILvoid           function(ILenum PName, ILenum Param) pfiluImageParameter;
    typedef ILvoid           function() pfiluInit;
    typedef ILboolean            function() pfiluInvertAlpha;
    typedef ILuint           function( ILstring FileName) pfiluLoadImage;
    typedef ILboolean            function() pfiluMirror;
    typedef ILboolean            function() pfiluNegative;
    typedef ILboolean            function(ILclampf Tolerance) pfiluNoisify;
    typedef ILboolean            function(ILuint PixSize) pfiluPixelize;
    typedef ILvoid           function(ILpointf *Points, ILuint n) pfiluRegionfv;
    typedef ILvoid           function(ILpointi *Points, ILuint n) pfiluRegioniv;
    typedef ILboolean            function(ILubyte Red, ILubyte Green, ILubyte Blue, ILfloat Tolerance) pfiluReplaceColour;
    typedef ILboolean            function(ILfloat Angle) pfiluRotate;
    typedef ILboolean            function(ILfloat x, ILfloat y, ILfloat z, ILfloat Angle) pfiluRotate3D;
    typedef ILboolean            function(ILfloat Saturation) pfiluSaturate1f;
    typedef ILboolean            function(ILfloat r, ILfloat g, ILfloat b, ILfloat Saturation) pfiluSaturate4f;
    typedef ILboolean            function(ILuint Width, ILuint Height, ILuint Depth) pfiluScale;
    typedef ILboolean            function(ILfloat r, ILfloat g, ILfloat b) pfiluScaleColours;
    typedef ILboolean            function(ILfloat Factor, ILuint Iter) pfiluSharpen;
    typedef ILboolean            function() pfiluSwapColours;
    typedef ILboolean            function(ILfloat Angle) pfiluWave;
";

version(Windows)
{
    extern(Windows): mixin(Funcs);
}
else
{
    extern(C): mixin(Funcs);
}

pfiluAlienify iluAlienify;
pfiluBlurAvg iluBlurAvg;
pfiluBlurGaussian iluBlurGaussian;
pfiluBuildMipmaps iluBuildMipmaps;
pfiluColoursUsed iluColoursUsed;
pfiluCompareImage iluCompareImage;
pfiluContrast iluContrast;
pfiluCrop iluCrop;
pfiluDeleteImage iluDeleteImage;
pfiluEdgeDetectE iluEdgeDetectE;
pfiluEdgeDetectP iluEdgeDetectP;
pfiluEdgeDetectS iluEdgeDetectS;
pfiluEmboss iluEmboss;
pfiluEnlargeCanvas iluEnlargeCanvas;
pfiluEnlargeImage iluEnlargeImage;
pfiluEqualize iluEqualize;
pfiluErrorString iluErrorString;
pfiluFlipImage iluFlipImage;
pfiluGammaCorrect iluGammaCorrect;
pfiluGenImage iluGenImage;
pfiluGetImageInfo iluGetImageInfo;
pfiluGetInteger iluGetInteger;
pfiluGetIntegerv iluGetIntegerv;
pfiluGetString iluGetString;
pfiluImageParameter iluImageParameter;
pfiluInit iluInit;
pfiluInvertAlpha iluInvertAlpha;
pfiluLoadImage iluLoadImage;
pfiluMirror iluMirror;
pfiluNegative iluNegative;
pfiluNoisify iluNoisify;
pfiluPixelize iluPixelize;
pfiluRegionfv iluRegionfv;
pfiluRegioniv iluRegioniv;
pfiluReplaceColour iluReplaceColour;
pfiluRotate iluRotate;
pfiluRotate3D iluRotate3D;
pfiluSaturate1f iluSaturate1f;
pfiluSaturate4f iluSaturate4f;
pfiluScale iluScale;
pfiluScaleColours iluScaleColours;
pfiluSharpen iluSharpen;
pfiluSwapColours iluSwapColours;
pfiluWave iluWave;


alias iluColoursUsed    iluColorsUsed;
alias iluSwapColours    iluSwapColors;
alias iluReplaceColour  iluReplaceColor;
//alias iluScaleColour  iluScaleColor;
