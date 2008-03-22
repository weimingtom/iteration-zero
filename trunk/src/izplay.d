module izplay;

import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;
import util;
import derelict.opengl.gl;

import dlisp.dlisp;
import gobject, party, character;

class LevelState : IGameState
{
    public:
        Engine engine;
        DLisp dlisp;
        LevelRenderer view;
        string filename;

    this(char[] filename_)
    {
        filename = filename_;
        engine = Engine.instance;
        dlisp = engine.dlisp;

        view = new LevelRenderer;
        engine.renderPipeline.add (view);
    }

    void start()
    {
        engine.renderPipeline.setPipeline(["level"]);

        GObject.bindClass(dlisp.environment);
        Character.bindClass(dlisp.environment);

        view.level = new Level (filename);
        view.lookAt (0, 0);

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
    }

    string name() { return "test"; }

    void handleKeyEvent(SDL_Event event)
    {
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
            if( event.button.button == SDL_BUTTON_LEFT )
            {
                if( !view.level.gobjects[0].isBusy )
                    view.level.gobjects[0].startMovingTo(x,y);
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
    if( argv.length < 2 )
    {
        writefln("usage: iz-play levelfile");
        return;
    }

    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    engine.addState( new LevelState (argv[1]) );
    engine.start ("test");

    engine.mainLoop();
}
