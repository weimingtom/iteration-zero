%module(directors="1") micron
%include "std_string.i"
/*
%include "std_vector.i"
%include "std_pair.i"
%include "std_list.i"
%include "std_map.i"
%include "std_set.i"
%include "typemaps.i"
%include "exception.i"
*/

%{
#include <SDL_ttf.h>
#include <guichan.hpp>
#include <guichan/font.hpp>
#include <guichan/image.hpp>
#include <guichan/opengl.hpp>
#include <guichan/opengl/openglsdlimageloader.hpp>
#include <guichan/sdl.hpp>
#include "micron.h"
#include "font.h"
%}

%nodefaultctor;
namespace gcn {
	class Font { };
}
%clearnodefaultctor;

class TTFont : public gcn::Font
{
public:
	TTFont(const std::string &fname, int size);
	virtual ~TTFont();

	void drawString(gcn::Graphics *graphics, const std::string &text, int x, int y);
/* 	gcn::OpenGLImage* genImage(const std::string &text); */
	virtual int getHeight() const;
	virtual int getWidth(const std::string &text) const;
};

class Micron {
public:
	Micron();
	~Micron();

	void init(int x, int y, bool fullscreen);
	void shutdown();
	void pumpEvents();

	gcn::Graphics* getGraphics();
	gcn::Input* getInput();

	void startFrame();
	void endFrame();

	bool isQuitRequested();
};
