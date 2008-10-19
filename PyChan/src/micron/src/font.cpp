#include "font.h"
#include <iostream>

TTFont::TTFont(const std::string &fname, int size)
{
	if(!TTF_WasInit() && TTF_Init()==-1) {
		printf("TTF_Init: %s\n", TTF_GetError());
		exit(1);
	}
	font = TTF_OpenFont(fname.c_str(), size);
      
	//TTF_SetTTFontStyle(font, TTF_STYLE_BOLD|TTF_STYLE_ITALIC|TTF_STYLE_UNDERLINE);
      
	height = size;
	ascent = TTF_FontAscent(font);
	fg.r = 255;
	fg.g = 255;
	fg.b = 255;
	fg.unused = 255;
      
	height = TTF_FontHeight(font);
      
	Uint16 count = 0;
	for(count=0;count < 256;count++) {
		tscreen = TTF_RenderGlyph_Blended(font, count, fg);
		tscreen = convertToStandardFormat(tscreen);
		glyphs[count].img = new gcn::OpenGLImage((unsigned int*)tscreen->pixels, tscreen->w, tscreen->h, false);
		glyphs[count].img->convertToDisplayFormat();
		SDL_FreeSurface(tscreen);
		TTF_GlyphMetrics(font, count, &glyphs[count].minx, &glyphs[count].maxx, &glyphs[count].miny, &glyphs[count].maxy, &glyphs[count].advance);
	}
}

TTFont::~TTFont()
{
	SDL_FreeSurface(tscreen);
	delete zomg;
	TTF_CloseFont(font);
}

void TTFont::drawString(gcn::Graphics *graphics, const std::string &text, int x, int y) {
	std::string txt = text;
	if(text.length() == 0 || text[0] == '\0' || text == "") return;
	int m_xpos = 0;
	std::string::iterator m_itr;
	for(m_itr = txt.begin(); m_itr != txt.end(); m_itr++) {
		Uint16 chr = *m_itr;
		graphics->drawImage(glyphs[chr].img, x + m_xpos + glyphs[chr].minx, y + ascent - glyphs[chr].maxy);
		m_xpos += glyphs[chr].advance;
	}
}

// gcn::OpenGLImage* TTFont::genImage(const std::string &text) { //Not sure why I added this anymore..
// 	gcn::OpenGLImage *pomg = NULL;
// 	if(text.length() == 0 || text[0] == '\0' || text == "") return pomg;
// 	if(!(tscreen = TTF_RenderText_Blended(font, text.c_str(), fg))) {
// 		std::cout << "Error! " << TTF_GetError() << std::endl;
// 		return pomg;
// 	}
// 	pomg = new gcn::OpenGLImage((unsigned int*)tscreen->pixels, tscreen->w, tscreen->h, false);
// 	pomg->convertToDisplayFormat();
// 	SDL_FreeSurface(tscreen);
// 	return pomg;
// }

int TTFont::getWidth(const std::string &text) const {
	int w = 0, h = 0;
	TTF_SizeText(font, text.c_str(), &w, &h);
	return w;
}

int TTFont::getHeight() const {
	return height;
}
