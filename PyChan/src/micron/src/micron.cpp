#include "micron.h"

Micron::Micron()
{
	m_imageLoader = 0;
	m_input = 0;
	m_graphics = 0;
	m_quit_requested = false;
	m_was_init = false;
}

Micron::~Micron()
{
	shutdown();
}

void Micron::init(int x, int y, bool fullscreen)
{
	if( m_was_init )
		return;
	m_was_init = true;
	// We simply initialise OpenGL and SDL as we would do with any OpenGL
	// and SDL application.
	SDL_Init(SDL_INIT_VIDEO);
	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

	if(!fullscreen) {
		m_screen = SDL_SetVideoMode(x, y, 32, SDL_HWSURFACE | SDL_OPENGL | SDL_HWACCEL);
	} else {
		m_screen = SDL_SetVideoMode(x, y, 32, SDL_HWSURFACE | SDL_OPENGL | SDL_HWACCEL | SDL_FULLSCREEN);
	}

	glViewport(0, 0, x, y);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

	// We want unicode for the SDLInput object to function properly.
	SDL_EnableUNICODE(1);
	// We also want to enable key repeat.
	SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

	// Now it's time to initialise the Guichan OpenGL back end
	// and the Guichan SDL back end.

	m_imageLoader = new gcn::OpenGLSDLImageLoader();
	// The ImageLoader Guichan should use needs to be passed to the Image object
	// using a static function.
	gcn::Image::setImageLoader(m_imageLoader);
	m_graphics = new gcn::OpenGLGraphics();
	// We need to tell the OpenGL Graphics object how big the screen is.
	m_graphics->setTargetPlane(x, y);
	m_input = new gcn::SDLInput();
}

void Micron::shutdown()
{
	if( !m_was_init )
		return;
	delete m_imageLoader;
	delete m_input;
	delete m_graphics;
	m_imageLoader = 0;
	m_input = 0;
	m_graphics = 0;
	m_was_init = false;
	SDL_Quit();
}

void Micron::pumpEvents()
{
	if( !m_was_init )
		return;
	// Check user input
	SDL_Event event;
	while(SDL_PollEvent(&event))
	{
		if (event.type == SDL_KEYDOWN)
		{
			if (event.key.keysym.sym == SDLK_ESCAPE)
			{
				m_quit_requested = true;
			}
			if (event.key.keysym.sym == SDLK_f)
			{
				if (event.key.keysym.mod & KMOD_CTRL)
				{
				    // Works with X11 only
				    SDL_WM_ToggleFullScreen(m_screen);
				}
			}
		}
		else if(event.type == SDL_QUIT)
		{
			m_quit_requested = true;
		}
		
		// After we have manually checked user input with SDL for
		// any attempt by the user to halt the application we feed
		// the input to Guichan by pushing the input to the Input
		// object.
		m_input->pushInput(event);
	}
}

void Micron::startFrame() {
	glClear(GL_COLOR_BUFFER_BIT);
}

void Micron::endFrame()
{
	if( !m_was_init )
		return;
	SDL_GL_SwapBuffers();
}

gcn::Graphics* Micron::getGraphics() { return m_graphics; }
gcn::Input* Micron::getInput() { return m_input; }
