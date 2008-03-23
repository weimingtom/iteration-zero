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
import dataset;

import turn;

class LevelState : IGameState
{
    public:
        Engine engine;
        DLisp dlisp;
        LevelRenderer view;
        Party party;
        Character active;
        TurnManager turnManager;
        string filename;

    this(char[] filename_)
    {
        filename = filename_;
        engine = Engine.instance;
        dlisp = engine.dlisp;

        view = new LevelRenderer;
        engine.renderPipeline.add (view);

        party = new Party;
    }

    void start()
    {
        engine.renderPipeline.setPipeline(["level"]);

        party.bindInstance(dlisp.environment,"*party*");
        GObject.bindClass(dlisp.environment);
        Character.bindClass(dlisp.environment);

        view.level = new Level (filename);
        turnManager = new TurnManager(party,view.level);

//         dlisp.environment.pushScope();
//         dlisp.parseEvalPrint("(LOAD \"data/test/party.dl\" T)", true);//"
//         dlisp.environment.popScope();
        party.load("data/test/party.sf");
        writefln( "char01\n", party.get(1).toString() );

        assert( party.getSize() > 0 );
        active = party.getActive();

        party.placeInLevel( view.level, 5,5);

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
        turnManager.logic();
        active = party.getActive();

        int x,y;
        int clicked = SDL_GetMouseState(&x,&y) & SDL_BUTTON(1);
        view.toLevelCoords(&x,&y);
        // FUGLY
        view.sceneMode();
        view.drawHighlight(x,y);

        view.lookAt(active.real_x,active.real_y);
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
                if( !active.isBusy )
                    active.startMovingTo(x,y);
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
