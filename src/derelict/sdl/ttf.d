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
 * * Neither the names 'Derelict', 'DerelictSDLttf', nor the names of its contributors
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
module derelict.sdl.ttf;

private
{
    import derelict.sdl.sdl;
    import derelict.util.loader;
}

//==============================================================================
// Types
//==============================================================================
enum : Uint8
{
    SDL_TTF_MAJOR_VERSION = 2,
    SDL_TTF_MINOR_VERSION = 0,
    SDL_TTF_PATCHLEVEL    = 7,
}
alias SDL_TTF_MAJOR_VERSION TTF_MAJOR_VERSION;
alias SDL_TTF_MINOR_VERSION TTF_MINOR_VERSION;
alias SDL_TTF_PATCHLEVEL TTF_PATCHLEVEL;

enum
{
    UNICODE_BOM_NATIVE = 0xFEFF,
    UNICODE_BOM_SWAPPED = 0xFFFE,
    TTF_STYLE_NORMAL = 0x00,
    TTF_STYLE_BOLD = 0x01,
    TTF_STYLE_ITALIC = 0x02,
    TTF_STYLE_UNDERLINE = 0x04,
}

alias SDL_SetError TTF_SetError;
alias SDL_GetError TTF_GetError;

struct _TTF_Font {}
typedef _TTF_Font TTF_Font;

//==============================================================================
// Macros
//==============================================================================
void SDL_TTF_VERSION(SDL_version* X)
{
    X.major = SDL_TTF_MAJOR_VERSION;
    X.minor = SDL_TTF_MINOR_VERSION;
    X.patch = SDL_TTF_PATCHLEVEL;
}

void TTF_VERSION(SDL_version* X) { SDL_TTF_VERSION(X); }

//==============================================================================
// Functions
//==============================================================================

extern (C)
{

typedef const SDL_version* function() pfTTF_Linked_Version;
typedef void function(int swapped) pfTTF_ByteSwappedUNICODE;
typedef int function() pfTTF_Init;
typedef TTF_Font * function (char *file, int ptsize) pfTTF_OpenFont;
typedef TTF_Font * function (char *file, int ptsize, long index) pfTTF_OpenFontIndex;
typedef TTF_Font * function (SDL_RWops *src, int freesrc, int ptsize) pfTTF_OpenFontRW;
typedef TTF_Font * function (SDL_RWops *src, int freesrc, int ptsize, long index) pfTTF_OpenFontIndexRW;
typedef int function (TTF_Font *font) pfTTF_GetFontStyle;
typedef void function (TTF_Font *font, int style) pfTTF_SetFontStyle;
typedef int function (TTF_Font *font) pfFontPropertyInt;
typedef long function (TTF_Font *font) pfFontPropertyLong;
typedef char* function (TTF_Font *font) pfFontPropertyString;
typedef int function (TTF_Font *font, Uint16 ch,
    int *minx, int *maxx, int *miny, int *maxy, int *advance) pfTTF_GlyphMetrics;
typedef int function (TTF_Font *font, char *text, int *w, int *h) pfTTF_SizeText;
typedef int function (TTF_Font *font, char *text, int *w, int *h) pfTTF_SizeUTF8;
typedef int function (TTF_Font *font, Uint16 *text, int *w, int *h) pfTTF_SizeUNICODE;
typedef SDL_Surface * function (TTF_Font *font, char *text, SDL_Color fg) pfTTF_RenderText_Solid;
typedef SDL_Surface * function (TTF_Font *font, char *text, SDL_Color fg) pfTTF_RenderUTF8_Solid;
typedef SDL_Surface * function (TTF_Font *font, Uint16 *text, SDL_Color fg) pfTTF_RenderUNICODE_Solid;
typedef SDL_Surface * function (TTF_Font *font, Uint16 ch, SDL_Color fg) pfTTF_RenderGlyph_Solid;
typedef SDL_Surface * function (TTF_Font *font, char *text, SDL_Color fg, SDL_Color bg) pfTTF_RenderText_Shaded;
typedef SDL_Surface * function (TTF_Font *font, char *text, SDL_Color fg, SDL_Color bg) pfTTF_RenderUTF8_Shaded;
typedef SDL_Surface * function (TTF_Font *font, Uint16 *text, SDL_Color fg, SDL_Color bg) pfTTF_RenderUNICODE_Shaded;
typedef SDL_Surface * function (TTF_Font *font, Uint16 ch, SDL_Color fg, SDL_Color bg) pfTTF_RenderGlyph_Shaded;
typedef SDL_Surface * function (TTF_Font *font, char *text, SDL_Color fg) pfTTF_RenderText_Blended;
typedef SDL_Surface * function (TTF_Font *font, char *text, SDL_Color fg) pfTTF_RenderUTF8_Blended;
typedef SDL_Surface * function (TTF_Font *font, Uint16 *text, SDL_Color fg) pfTTF_RenderUNICODE_Blended;
typedef SDL_Surface * function (TTF_Font *font, Uint16 ch, SDL_Color fg) pfTTF_RenderGlyph_Blended;
typedef void function (TTF_Font *font) pfTTF_CloseFont;
typedef void function () pfTTF_Quit;
typedef int function () pfTTF_WasInit;
pfTTF_Linked_Version                    TTF_Linked_Version;
pfTTF_ByteSwappedUNICODE                TTF_ByteSwappedUNICODE;
pfTTF_Init                              TTF_Init;
pfTTF_OpenFont                          TTF_OpenFont;
pfTTF_OpenFontIndex                     TTF_OpenFontIndex;
pfTTF_OpenFontRW                        TTF_OpenFontRW;
pfTTF_OpenFontIndexRW                   TTF_OpenFontIndexRW;
pfTTF_GetFontStyle                      TTF_GetFontStyle;
pfTTF_SetFontStyle                      TTF_SetFontStyle;
pfFontPropertyInt                       TTF_FontHeight;
pfFontPropertyInt                       TTF_FontAscent;
pfFontPropertyInt                       TTF_FontDescent;
pfFontPropertyInt                       TTF_FontLineSkip;
pfFontPropertyLong                      TTF_FontFaces;
pfFontPropertyInt                       TTF_FontFaceIsFixedWidth;
pfFontPropertyString                    TTF_FontFaceFamilyName;
pfFontPropertyString                    TTF_FontFaceStyleName;
pfTTF_GlyphMetrics                      TTF_GlyphMetrics;
pfTTF_SizeText                          TTF_SizeText;
pfTTF_SizeUTF8                          TTF_SizeUTF8;
pfTTF_SizeUNICODE                       TTF_SizeUNICODE;
pfTTF_RenderText_Solid                  TTF_RenderText_Solid;
pfTTF_RenderUTF8_Solid                  TTF_RenderUTF8_Solid;
pfTTF_RenderUNICODE_Solid               TTF_RenderUNICODE_Solid;
pfTTF_RenderGlyph_Solid                 TTF_RenderGlyph_Solid;
pfTTF_RenderText_Shaded                 TTF_RenderText_Shaded;
pfTTF_RenderUTF8_Shaded                 TTF_RenderUTF8_Shaded;
pfTTF_RenderUNICODE_Shaded              TTF_RenderUNICODE_Shaded;
pfTTF_RenderGlyph_Shaded                TTF_RenderGlyph_Shaded;
pfTTF_RenderText_Blended                TTF_RenderText_Blended;
pfTTF_RenderUTF8_Blended        TTF_RenderUTF8_Blended;
pfTTF_RenderUNICODE_Blended             TTF_RenderUNICODE_Blended;
pfTTF_RenderGlyph_Blended               TTF_RenderGlyph_Blended;
pfTTF_CloseFont                         TTF_CloseFont;
pfTTF_Quit                              TTF_Quit;
pfTTF_WasInit                           TTF_WasInit;

} // extern(C)

alias TTF_RenderText_Shaded TTF_RenderText;
alias TTF_RenderUTF8_Shaded TTF_RenderUTF8;
alias TTF_RenderUNICODE_Shaded TTF_RenderUNICODE;


//==============================================================================
// Loader
//==============================================================================

private void load(SharedLib lib)
{
    bindFunc(TTF_Linked_Version)("TTF_Linked_Version", lib);
    bindFunc(TTF_ByteSwappedUNICODE)("TTF_ByteSwappedUNICODE", lib);
    bindFunc(TTF_Init)("TTF_Init", lib);
    bindFunc(TTF_OpenFont)("TTF_OpenFont", lib);
    bindFunc(TTF_OpenFontIndex)("TTF_OpenFontIndex", lib);
    bindFunc(TTF_OpenFontRW)("TTF_OpenFontRW", lib);
    bindFunc(TTF_OpenFontIndexRW)("TTF_OpenFontIndexRW", lib);
    bindFunc(TTF_GetFontStyle)("TTF_GetFontStyle", lib);
    bindFunc(TTF_SetFontStyle)("TTF_SetFontStyle", lib);
    bindFunc(TTF_FontHeight)("TTF_FontHeight", lib);
    bindFunc(TTF_FontAscent)("TTF_FontAscent", lib);
    bindFunc(TTF_FontDescent)("TTF_FontDescent", lib);
    bindFunc(TTF_FontLineSkip)("TTF_FontLineSkip", lib);
    bindFunc(TTF_FontFaces)("TTF_FontFaces", lib);
    bindFunc(TTF_FontFaceIsFixedWidth)("TTF_FontFaceIsFixedWidth", lib);
    bindFunc(TTF_FontFaceFamilyName)("TTF_FontFaceFamilyName", lib);
    bindFunc(TTF_FontFaceStyleName)("TTF_FontFaceStyleName", lib);
    bindFunc(TTF_GlyphMetrics)("TTF_GlyphMetrics", lib);
    bindFunc(TTF_SizeText)("TTF_SizeText", lib);
    bindFunc(TTF_SizeUTF8)("TTF_SizeUTF8", lib);
    bindFunc(TTF_SizeUNICODE)("TTF_SizeUNICODE", lib);
    bindFunc(TTF_RenderText_Solid)("TTF_RenderText_Solid", lib);
    bindFunc(TTF_RenderUTF8_Solid)("TTF_RenderUTF8_Solid", lib);
    bindFunc(TTF_RenderUNICODE_Solid)("TTF_RenderUNICODE_Solid", lib);
    bindFunc(TTF_RenderGlyph_Solid)("TTF_RenderGlyph_Solid", lib);
    bindFunc(TTF_RenderText_Shaded)("TTF_RenderText_Shaded", lib);
    bindFunc(TTF_RenderUTF8_Shaded)("TTF_RenderUTF8_Shaded", lib);
    bindFunc(TTF_RenderUNICODE_Shaded)("TTF_RenderUNICODE_Shaded", lib);
    bindFunc(TTF_RenderGlyph_Shaded)("TTF_RenderGlyph_Shaded", lib);
    bindFunc(TTF_RenderText_Blended)("TTF_RenderText_Blended", lib);
    bindFunc(TTF_RenderUTF8_Blended)("TTF_RenderUTF8_Blended", lib);
    bindFunc(TTF_RenderUNICODE_Blended)("TTF_RenderUNICODE_Blended", lib);
    bindFunc(TTF_RenderGlyph_Blended)("TTF_RenderGlyph_Blended", lib);
    bindFunc(TTF_CloseFont)("TTF_CloseFont", lib);
    bindFunc(TTF_Quit)("TTF_Quit", lib);
    bindFunc(TTF_WasInit)("TTF_WasInit", lib);
}


GenericLoader DerelictSDLttf;
static this() {
    DerelictSDLttf.setup(
        "SDL_ttf.dll",
        "libSDL_ttf.so, libSDL_ttf-2.0.so, libSDL_ttf-2.0.so.0",
        "",
        &load
    );
}
