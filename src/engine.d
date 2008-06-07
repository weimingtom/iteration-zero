module Engine;

import std.conv;
import std.string;
import std.c.string;
import std.stdio;
import std.math;
import derelict.sdl.sdl;
import derelict.sdl.keysym;
import derelict.sdl.image;
import derelict.opengl.gl;
import derelict.opengl.glext;
import derelict.opengl.glu;

import texture;
import pipeline;
import material;
import util;

import sofu = Sofu.Sofu;
import dlisp.dlisp;
import dlisp.predefs.all;
import dlisp.bind;

import guichan.all;
import guichan.bind;
import guichan.gui;
import guichan.opengl.graphics;
import guichan.opengl.font;
import guichan.sdl.input;

import guichan.widgets.container;
import guichan.widget;

import guichan.listener;
import guichan.event;
import guichan.key;
import gamestate;

class EventMapper : public KeyListener, public MouseListener
{
    void keyPressed(KeyEvent keyEvent)
    {
        if( keyEvent.getKey.getValue == Key.ESCAPE )
            Engine.instance.stop;

        Engine.instance.state.keyPressed(keyEvent);
    }

    void keyReleased(KeyEvent keyEvent)
    {
        Engine.instance.state.keyReleased(keyEvent);
    }

    void mouseEntered(MouseEvent mouseEvent) { }
    void mouseExited(MouseEvent mouseEvent) { }
    void mousePressed(MouseEvent mouseEvent) { }
    void mouseReleased(MouseEvent mouseEvent) { }
    void mouseClicked(MouseEvent mouseEvent)
    {
        if( !isConsumed(mouseEvent) )
            Engine.instance.state.mouseClicked(mouseEvent);
    }

    void mouseWheelMovedUp(MouseEvent mouseEvent) { }
    void mouseWheelMovedDown(MouseEvent mouseEvent) { }
    void mouseMoved(MouseEvent mouseEvent) { }
    void mouseDragged(MouseEvent mouseEvent) { }

    bool isConsumed(MouseEvent mouseEvent)
    {
        auto top = Engine.instance.gui.getTop;
        if( mouseEvent.isConsumed )
            return true;
        return top.getWidgetAt(mouseEvent.getX,mouseEvent.getY) !is null;
    }

}


class Engine
{
    private:
    static Engine _instance = null;
    public:
    mixin BindClass!("C/ENGINE");

    static Engine instance()
    {
        assert(_instance !is null);
        return _instance;
    }

    this(char[] configfile = "engine.cfg")
    {
        assert(_instance is null);
        _instance = this;

        loadConfig(configfile);
        init();
        _textureManager = new TextureManager;
        _pipeline = new RenderPipeline;
    }

    ~this()
    {
        cleanup();
    }

    void loadConfig(char[] configfile)
    {
        auto cfg = sofu.loadFile(configfile);
        auto resolution = cfg.list("resolution");
        _xResolution = resolution.value(0).toInt();
        _yResolution = resolution.value(1).toInt();
        _bitsPerPixel = cfg.value("bitsPerPixel").toInt();
        _fullScreen = cfg.value("fullScreen").toInt();
        writefln( "FullScreen:",_fullScreen);
    }

    void init()
    {
        // initialize SDL, GL and GLU Derelict modules
        DerelictSDL.load();
        DerelictSDLImage.load();
        DerelictGL.load();
        DerelictGLU.load();

        // initialize SDL's VIDEO module
        SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER);

        // enable double-buffering
        SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);

        // create our OpenGL window
        int flags = SDL_OPENGL;
        if( _fullScreen )
            flags |= SDL_FULLSCREEN;
        SDL_Surface* surface = SDL_SetVideoMode(_xResolution, _yResolution, _bitsPerPixel, flags);
        if( !surface )
        {
            throw new Exception("Couldn't set video-mode: " ~ to_string(SDL_GetError()));
        }

        SDL_WM_SetCaption(toStringz("iteration:zero"), null);
        SDL_EnableUNICODE(1);
        setupGL();

        _dlisp = new DLisp(addAllToEnvironment(new Environment));
        _dlisp.parseEvalPrint("(LOAD \"dlisp_oop/system.lisp\" T)", true);//"
        bindInstance(_dlisp.environment,"*engine*");

        bindGuichan(_dlisp.environment);
//         BasicContainer.bindClass(_dlisp.environment);

        _gui = new Gui;
        _gui.setGraphics( new OpenGLGraphics );
        _gui.setInput( new SDLInput );
        Container top = new Container;
        _gui.setTop( top );
        top.setFocusable(true);
        top.requestFocus();
        top.addKeyListener( new EventMapper );
        top.addMouseListener( new EventMapper );
        top.setSize( xResolution, yResolution );
        top.setOpaque( false );
        Widget.setGlobalFont( new OpenGLFont("data/fonts/7service/7the.ttf",16) );
    }

    // be nice and release all resources
    void cleanup() {
        // tell SDL to quit
        SDL_Quit();
        // release GL, GLU and SDL's shared libs
        DerelictGLU.unload();
        DerelictGL.unload();
        DerelictSDLImage.unload();
        DerelictSDL.unload();
    }

    /**
    Setup some basic OpenGL parameters
    */
    void setupGL() {
        EXTBlendColor.load ("GL_EXT_blend_color");
        if( EXTBlendColor.isEnabled )
            writefln ("GL_EXT_blend_color - ENABLED.");
        ARBTextureNonPowerOfTwo.load ("GL_ARB_texture_non_power_of_two");
        if( ARBTextureNonPowerOfTwo.isEnabled )
            writefln ("GL_ARB_texture_non_power_of_two - ENABLED.");

        glViewport(0, 0, xResolution, yResolution);
        glEnable (GL_DEPTH_TEST);
    }

    void mainLoop()
    {
        static ulong ticks = 0;
        _running = true;
        _frames = 0;
        while (_running) {
            // clear the screen. by default it clears to black
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
            // draw our stuff =)
            _pipeline.render();
            (cast(OpenGLGraphics)_gui.getGraphics).setTargetPlane(xResolution,yResolution);
            _gui.draw();
            _updateStates();
            // swap the buffers, making our backbuffer the visible one
            SDL_GL_SwapBuffers();

            if( _frames % 100 == 0 )
            {
                ulong new_ticks = SDL_GetTicks();
                writefln ("FPS: ",100_000.0/(new_ticks - ticks));
                ticks = new_ticks;
            }
            _frames += 1;

            _gui.logic();
            logic();
            handleEvents();
        }
    }

    void handleEvents()
    {
        SDL_Event event;
        // handle all SDL events that we might've received in this loop iteration
        while (_running && SDL_PollEvent(&event)) {
            (cast(SDLInput)_gui.getInput).pushInput(event);
            switch (event.type) {
                // user has clicked on the window's close button
                case SDL_QUIT:
                    _running = false;
                    break;
                // by default, we do nothing => break from the switch
                default:
                    break;
            }
        }
    }

    TextureManager textureManager()
    {
        return _textureManager;
    }

    RenderPipeline renderPipeline()
    {
        return _pipeline;
    }

    DLisp dlisp()
    {
        return _dlisp;
    }

    void stop()
    {
        _running = false;
        stopCallback();
    }

    void logic()
    {
        logicCallback();
    }

    GameState state()
    {
        return _currentState;
    }

    Gui gui()
    {
      return _gui;
    }

    void start(string name)
    {
        _nextState = _states[name];
    }

    GameState getState(string name)
    {
        return _states[name];
    }

    void addState(GameState state_)
    {
        assert( state_ !is null );
        assert( (state_.name in _states) is null );
        _states[ state_.name ] = state_;
    }

    int   xResolution () { return _xResolution; }
    int   yResolution () { return _yResolution; }


    mixin BindMethods!(xResolution,yResolution);
    mixin BindHandlers!(logic,stop);
    mixin BindMethods!(start,stop);
    mixin BindMethod!("get-gui",gui);
    mixin BindMethod!("get-current-state",state);
    mixin BindMethods!(addState,getState);

    private {
        // horizontal and vertical screen resolution
        int   _xResolution     = 800;
        int   _yResolution     = 600;
        // number of bits per pixel used for display. 24 => true color
        int   _bitsPerPixel    = 24;
        // fullscreen toggle
        int _fullScreen        = 0;
        // field of view => the angle our camera will see vertically
        float _fov             = 90.f;
        // distance of the near clipping plane
        float _nearPlane       = .1f;
        // distance of the far clipping plane
        float _farPlane        = 100.f;

        long _frames;
        bit _running;

        GameState[string] _states;
        GameState _currentState;
        GameState _nextState;
        
        TextureManager _textureManager;
        RenderPipeline _pipeline;

        DLisp _dlisp;
        Gui _gui;

        void _updateStates()
        {
            if( _nextState !is null )
            {
                    if( state !is null )
                        state.stop();
                _currentState = _nextState;
                _nextState = null;
                state.start();
            }
            state.logic();
        }
    }
}

