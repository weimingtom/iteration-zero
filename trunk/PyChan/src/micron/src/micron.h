#include <guichan.hpp>
#include <guichan/opengl.hpp>
// The openglsdlimageloader.hpp header file needs to be included
// in order to get the image loader that uses OpenGL and SDL.
#include <guichan/opengl/openglsdlimageloader.hpp>
#include <guichan/sdl.hpp>

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

	bool isQuitRequested() { return m_quit_requested; }

private:
	SDL_Surface* m_screen;
	gcn::OpenGLGraphics* m_graphics;
	gcn::SDLInput* m_input;
	gcn::OpenGLSDLImageLoader* m_imageLoader;
	bool m_quit_requested;
	bool m_was_init;
};
