module izedit;

import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;
import util;
import derelict.opengl.gl;

import gui;

class TestState : IGameState
{
    public:
        Engine engine;
        LevelRenderer view;
        string filename;

        GuiRenderer gui;
        int tileproto = 0;
        string tileprotoname;


    this(char[] filename_)
    {
        filename = filename_;
        engine = Engine.instance;
        view = new LevelRenderer;
        gui = new GuiRenderer;
        engine.renderPipeline.add (view);
        engine.renderPipeline.add (gui);
    }

    void start()
    {
        engine.renderPipeline.setPipeline(["level","gui"]);

        view.level = new Level (filename);
        view.lookAt (0, 0);

        auto keys = view.level.dataset._prototypes.keys;
        tileprotoname = keys[tileproto];
        

        glEnable(GL_COLOR_MATERIAL);
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }

    void stop()
    {
    }

    void logic()
    {
        int x,y;
        int clicked = SDL_GetMouseState(&x,&y) & SDL_BUTTON(1);
        view.toLevelCoords(&x,&y);
        // FUGLY
        view.sceneMode();
        view.drawHighlight(x,y);
//         writefln ("Looking at (%f %f)", view.level.gobjects[0].real_x,view.level.gobjects[0].real_y);
        view.lookAt(view.level.gobjects[0].real_x,view.level.gobjects[0].real_y);
        gui.text = [ "tile:" ~ tileprotoname ];
    }

    string name() { return "test"; }

    void handleKeyEvent(SDL_Event event)
    {
        if( event.key.type == SDL_KEYUP )
            return;

        switch(event.key.keysym.sym)
        {
            case '+':
                tileproto += 1;
                break;
            case '-':
                tileproto -= 1;
                break;
            default: break;
        }
        
        auto keys = view.level.dataset._prototypes.keys;
        if( tileproto < 0 ) tileproto = keys.length - 1;
        if( tileproto == keys.length ) tileproto = 0;
        tileprotoname = keys[tileproto];
    }
    void handleMouseMotionEvent(SDL_Event event) {
        int x,y;
        SDL_GetMouseState(&x,&y);
        
    }
    void handleMouseEvent(SDL_Event event)
    {
        int x,y;
        SDL_GetMouseState(&x,&y);
        view.toLevelCoords(&x,&y);

        if( event.type == SDL_MOUSEBUTTONUP )
        {
            if( event.button.button == SDL_BUTTON_RIGHT )
            {
                if( !view.level.gobjects[0].isBusy )
                    view.level.gobjects[0].startMovingTo(x,y);
            }
            if( event.button.button == SDL_BUTTON_LEFT )
            {
                view.level.tiles[y*view.level.width + x] = view.level.dataset._prototypes[ tileprotoname ].create(x,y);
            }

            if( event.button.button == SDL_BUTTON_WHEELUP )
            {
               view.zoom += 0.1;
            }
            if( event.button.button == SDL_BUTTON_WHEELDOWN )
            {
                if( view.zoom > 0.21 )
                    view.zoom -= 0.2;
            }
        }
    }

}

void main(char[][] argv)
{
    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    engine.addState( new TestState (argv[1]) );
    engine.start ("test");

    engine.mainLoop();
}
