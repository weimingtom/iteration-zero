#ifndef MICRON_TTFONT_H
#define MICRON_TTFONT_H

#include <SDL_ttf.h>
#include <guichan.hpp>
#include <guichan/image.hpp>
#include <guichan/opengl/openglsdlimageloader.hpp>

struct Font_Glyph
{
	gcn::OpenGLImage *img;
	int minx, maxx, miny, maxy, advance;
};

class TTFont : public gcn::Font, public gcn::SDLImageLoader
{
public:
	TTFont(const std::string &fname, int size);
	virtual ~TTFont();

	void drawString(gcn::Graphics *graphics, const std::string &text, int x, int y);
	gcn::OpenGLImage* genImage(const std::string &text);
	virtual int getHeight() const;
	virtual int getWidth(const std::string &text) const;
private:
	int height;
	SDL_Color fg;
	TTF_Font *font;
	int ascent;
	SDL_Surface *tscreen;
	Font_Glyph glyphs[256];
	gcn::OpenGLImage *zomg;
};

#endif
