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
module derelict.devil.ilutypes;

private import derelict.devil.iltypes;

enum : ILenum
{
    ILU__VERSION_1_6_7=1,
    ILU__VERSION=167,
    ILU__FILTER=0x2600,
    ILU__NEAREST=0x2601,
    ILU__LINEAR=0x2602,
    ILU__BILINEAR=0x2603,
    ILU__SCALE_BOX=0x2604,
    ILU__SCALE_TRIANGLE=0x2605,
    ILU__SCALE_BELL=0x2606,
    ILU__SCALE_BSPLINE=0x2607,
    ILU__SCALE_LANCZOS3=0x2608,
    ILU__SCALE_MITCHELL=0x2609,

    // Error types
    ILU__INVALID_ENUM=0x0501,
    ILU__OUT_OF_MEMORY=0x0502,
    ILU__INTERNAL_ERROR=0x0504,
    ILU__INVALID_VALUE=0x0505,
    ILU__ILLEGAL_OPERATION=0x0506,
    ILU__INVALID_PARAM=0x0509,

    // Values
    ILU__PLACEMENT=0x0700,
    ILU__LOWER_LEFT=0x0701,
    ILU__LOWER_RIGHT=0x0702,
    ILU__UPPER_LEFT=0x0703,
    ILU__UPPER_RIGHT=0x0704,
    ILU__CENTER=0x0705,
    ILU__CONVOLUTION_MATRIX=0x0710,
    ILU__VERSION_NUM=IL_VERSION_NUM,
    ILU__VENDOR=IL_VENDOR,
}

struct ILinfo {
    ILuint  Id;                 // the image's id
    ILubyte *Data;              // the image's data
    ILuint  Width;              // the image's width
    ILuint  Height;             // the image's height
    ILuint  Depth;              // the image's depth
    ILubyte Bpp;                // bytes per pixel (not bits) of the image
    ILuint  SizeOfData;         // the total size of the data (in bytes)
    ILenum  Format;             // image format (in IL style)
    ILenum  Type;               // image type (in IL style)
    ILenum  Origin;             // origin of the image
    ILubyte *Palette;           // the image's palette
    ILenum  PalType;            // palette type
    ILuint  PalSize;            // palette size
    ILenum  CubeFlags;          // flags for what cube map sides are present
    ILuint  NumNext;            // number of images following
    ILuint  NumMips;            // number of mipmaps
    ILuint  NumLayers;          // number of layers
};

struct ILpointf {
    ILfloat x, y;
};

struct ILpointi {
    ILint x, y;
};
