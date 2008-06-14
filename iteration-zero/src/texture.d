module texture;

import std.string;
import std.stdio;
import derelict.sdl.sdl;
import derelict.sdl.image;
import derelict.opengl.gl;
import derelict.opengl.glext : ARBTextureNonPowerOfTwo;
import derelict.opengl.glu;

import util;

class Texture
{
    public:
        ubyte[] data = null;            // pixel data
        int     width = 0;              // width
        int     height = 0;             // height
        int     log2width = 0;          // log2(width)
        int     sizebytePixel = 1;      // pixel data size (GL_LUMINANS=1, GL_RGB=3, GL_RGBA=4)
        char[]  nameFile;               // file name
        char[]  nameFileAlpha;          // file name for alpha map

        uint    uidName = 0;            // texture name for openGL (0=un-registerd)
        bool    registered = false;

        bool    isMipmapping = true;                       // mip-mpapping texture ?
        int     flagExpansion = GL_NEAREST_MIPMAP_NEAREST;  // expantion filter flag
        int     flagReduction = GL_NEAREST;                 // reduction filter flag
        int     flagWrapU = GL_REPEAT;                      // u-axis wrapping filter flag
        int     flagWrapV = GL_REPEAT;                      // v-axis wrapping filter flag

        GLenum  format = GL_LUMINANCE;  // pixel data format (GL_LUMINANCE | GL_RGB | GL_RGBA) (auto-selected with uidTexture)
        GLenum  target = GL_TEXTURE_2D; // texture target (GL_TEXTURE_1D | GL_TEXTURE_2D)

public:
    this()
    {
    }


    this(int width_, int height_, int sizebytePixel_, ubyte[] pix=null)
    {
        create(width_, height_, sizebytePixel_, pix);
    }

    this(char[] filename)
    {
        writefln ("Loading Texture: '%s'",filename);
        // read SDL image 
        SDL_Surface* image = IMG_Load(toStringz(filename));

        if (image is null)
        {
            throw new Exception("Failed to load image for texture " ~ filename);
        }
        nameFile = filename;
        // store the original width and height
        Uint32 orig_w = image.w;
        Uint32 orig_h = image.h;

        Uint32 pot_w = nextPowerOfTwo(image.w);
        Uint32 pot_h = nextPowerOfTwo(image.h);

        if( ARBTextureNonPowerOfTwo.isEnabled ) {
            pot_w = image.w;
            pot_h = image.h;
        } else {
            // calculate nearest power-of-two size
            pot_w = nextPowerOfTwo(image.w);
            pot_h = nextPowerOfTwo(image.h);
        }

        // convert image to 32 bit RGBA image of power of two size if needed 
        if ( ((image.format.BitsPerPixel != 32)) || image.w != pot_w || image.h != pot_h)
        {
                // create new surface of pot size with 32 bit RGBA ordering
                SDL_Surface* newsurf =	SDL_CreateRGBSurface (SDL_SWSURFACE,pot_w, pot_h, 32, 0x000000ff,0x0000ff00, 0x00ff0000, 0xff000000);
                // copy old surface data onto the new surface
                SDL_SetAlpha(image, 0, 0); // set alpha to off, so we just copy all the information
                SDL_FillRect(newsurf, null, 0); // fill with transparencity
                SDL_BlitSurface (image, null, newsurf, null);
                // free old surface memory 
                SDL_FreeSurface(image);
                // point the old surface data to the new surface 
                image = newsurf; 
        }

        int bpp = image.format.BytesPerPixel;
        long i = 0;
        _alloc(image.w,image.h,bpp);

        for(long y=0; y != image.h; ++y)
        {
            for(long x=0; x != image.w*bpp; ++x)
                data[(y)*image.w*bpp + x] = (cast(ubyte*)image.pixels)[i++];
        }
        SDL_FreeSurface(image);

    }

    ~this()
    {
        destroy();
    }

    // free bit data
    void destroy(bool withUnregister=true)
    {
        if (withUnregister) unregister();
        _free();
    }

    Texture create(int width_, int height_, int sizebytePixel_, ubyte[] pix=null)
    {
        _alloc(width_, height_, sizebytePixel_);
        if (pix == null) {
            data[] = 0;
	} else {
            if( width == width_ && height == height_ )
            {
                data = pix;
            } else 
            {
                data[] = 0;
                int bpp = sizebytePixel_;
                int linelen = width_*bpp;
                for(long y=0; y != height_; ++y)
                {
                    data[y*width*bpp .. y*width*bpp + linelen] =  pix[y*width_*bpp .. y*width_*bpp + linelen];
                }
            }
	}
        return this;
    }

    // translate bit data to graphic memory
    void register()
    {
        if( registered )
            return;

        if (uidName == 0)
            glGenTextures(1, &uidName);

        bind();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, flagReduction);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, flagExpansion);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, flagWrapU);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, flagWrapV);
        if (target == GL_TEXTURE_1D) {
            if (isMipmapping) gluBuild1DMipmaps(GL_TEXTURE_1D,    sizebytePixel, width,    format, GL_UNSIGNED_BYTE, cast(void*)data);
            else              glTexImage1D     (GL_TEXTURE_1D, 0, sizebytePixel, width, 0, format, GL_UNSIGNED_BYTE, cast(void*)data);
        } else {
            if (isMipmapping) gluBuild2DMipmaps(GL_TEXTURE_2D,    sizebytePixel, width, height,    format, GL_UNSIGNED_BYTE, cast(void*)data);
            else              glTexImage2D     (GL_TEXTURE_2D, 0, sizebytePixel, width, height, 0, format, GL_UNSIGNED_BYTE, cast(void*)data);
        }
        unbind();

        registered = true;
    }


    // free bit data on graphic memory
    void unregister()
    {
        if (uidName == 0)
            return;
        glDeleteTextures(1, &uidName);
        uidName = 0;
        registered = false;
    }

    // translate bit data to graphic memory after register()
    void update()
    {
        if (uidName == 0)
            return;

        bind();
        if (target == GL_TEXTURE_1D) {
            if (isMipmapping) gluBuild1DMipmaps(GL_TEXTURE_1D, sizebytePixel, width, format, GL_UNSIGNED_BYTE, cast(void*)data);
            else              glTexSubImage1D  (GL_TEXTURE_1D, 0, 0,          width, format, GL_UNSIGNED_BYTE, cast(void*)data);
        } else {
            if (isMipmapping) gluBuild2DMipmaps(GL_TEXTURE_2D, sizebytePixel, width, height, format, GL_UNSIGNED_BYTE, cast(void*)data);
            else              glTexSubImage2D  (GL_TEXTURE_2D, 0, 0, 0,       width, height, format, GL_UNSIGNED_BYTE, cast(void*)data);
        }
        unbind();
    }

    // glBindTexture()
    void bind()
    {
        glBindTexture(target, uidName);
    }

    // glBindTexture(0)
    void unbind()
    {
        glBindTexture(target, 0);
    }

    void draw(float x, float y, float z, float scale)
    {
        register();
        bind();
        glEnable(GL_TEXTURE_2D);

        float w = cast(float)height/width;
        float h = 1;

        glBegin(GL_QUADS);
            glTexCoord2f(1,1);
            glVertex3f(x+w,y+h,z);
            glTexCoord2f(1,0);
            glVertex3f(x+w,x,z);
            glTexCoord2f(0,0);
            glVertex3f(x,y,z);
            glTexCoord2f(0,1);
            glVertex3f(x,y+h,z);
        glEnd();
        unbind();
    }

    // glDrawPixels()
    void drawPixels(float x, float y)
    {
        glRasterPos2f(x, y);
        glDrawPixels(width, height, format, GL_UNSIGNED_BYTE, cast(void*)data);
    }


    // glReadPixels()
    void readPixels(int x, int y)
    {
        glPixelStorei(GL_PACK_ALIGNMENT ,4);
        glReadPixels(x, y, width, height, GL_RGBA, GL_UNSIGNED_BYTE, cast(void*)data);
    }

    // system memory operation
    //---------------------------------------------
    protected:
        // allocate bit data area in system memory
        void _alloc(int width_, int height_, int sizebytePixel_)
        {
            switch (sizebytePixel_)
            {
                case 1:  format = GL_LUMINANCE;       break;
                case 2:  format = GL_LUMINANCE_ALPHA; break;
                case 3:  format = GL_RGB;             break;
                case 4:  format = GL_RGBA;            break;
                default: format = GL_RGBA; sizebytePixel_=4; break;
            }

            if( ARBTextureNonPowerOfTwo.isEnabled )
            {
                width  = width_;
                height = height_;
                log2width = calcLog2(width);
            } else
            {
                width  = nextPowerOfTwo(width_);
                height = nextPowerOfTwo(height_);
                log2width = calcLog2(width);
            }

            sizebytePixel = sizebytePixel_;

            data.length = width * height * sizebytePixel;
        }

        // free bit data area in system memory
        void _free()
        {
            format = GL_LUMINANCE;
            width = 0;
            height = 0;
            log2width = 0;
            sizebytePixel = 1;
            nameFile = "";
            nameFileAlpha = "";
            delete(data);
            data = null;
        }

}

class TextureManager
{
    Texture[char[]] _textures;

    Texture load(char[] filename)
    {
        if( (filename in _textures) is null)
        {
            _textures[filename] = new Texture(filename);
        }
        return _textures[filename];
    }
}
