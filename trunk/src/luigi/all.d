//----------------------------------------------------------------------------
// Luigi global import
//
// Note: using this is probably a bad idea.
//
//----------------------------------------------------------------------------
/*
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

module luigi.all;

public {
    import luigi.adapter;
    static import luigi.adapters.sdl;
    version(dsss) {} else {
    static import luigi.adapters.gld;
    static import luigi.adapters.glfw;
    }
    import luigi.arranger;
    import luigi.base;
    import luigi.event;
    import luigi.font;
    import luigi.gldraw;
    import luigi.gui;
    import luigi.opengl;
    import luigi.signalobj;
    import luigi.theme;
    import luigi.themes.std;
    import luigi.themes.bitmaps;
    import luigi.themes.dxut;
}
