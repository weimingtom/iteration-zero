module levelstate;

import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;
import std.utf;
import util;
import derelict.opengl.gl;

import dlisp.dlisp;
import gobject, party, character;
import dataset;

import turn;
import character;
import guichan.all;
import guichan.widgets.all;
import guichan.event;
import gamestate;

import dlisp.bind;

class LevelState : GameState
{
    public:
        Engine engine;
        DLisp dlisp;
        LevelRenderer view;
        Party party;
        Character active;
        TurnManager turnManager;
        string filename;

    mixin BindClass!("LEVEL-STATE");

    this()
    {
        super("level-state");
        engine = Engine.instance;
        dlisp = engine.dlisp;
        bindClass(dlisp.environment);

        view = new LevelRenderer;
        engine.renderPipeline.add (view);
    }

    void load(string filename_)
    {
        filename = filename_;
    }

    void start()
    {
        assert( filename.length > 0 );
        party = new Party;

        engine.renderPipeline.setPipeline(["level"]);

        party = Party.getInstance(dlisp.environment["*PARTY*"]);
        GObject.bindClass(dlisp.environment);
        Character.bindClass(dlisp.environment);

        view.level = new Level (filename);
        turnManager = new TurnManager(party,view.level);

        party.load("data/test/party.sf");
        active = party.getActive();
        party.placeInLevel( view.level, 20,20);

        glEnable(GL_COLOR_MATERIAL);
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        super.start();
    }

    void stop()
    {
        engine.renderPipeline.setPipeline([]);
        super.stop();
    }

    void logic()
    {
        turnManager.logic();
        active = party.getActive();
        view.trackObject (active);

        int x,y;
        int clicked = SDL_GetMouseState(&x,&y) & SDL_BUTTON(1);
        view.toLevelCoords(&x,&y);
        // FUGLY
        view.sceneMode();
        view.drawHighlight(x,y);

        super.logic();
    }

    void mouseClicked(MouseEvent mouseEvent)
    {
        int x,y;
        SDL_GetMouseState(&x,&y);
        view.toLevelCoords(&x,&y);
        uint button = mouseEvent.getButton;

        if( button == MouseEvent.LEFT )
        {
            if( !active.isBusy )
                active.startMovingTo(x,y);
        }
        super.mouseClicked(mouseEvent);
    }

    void mouseWheelMovedUp(MouseEvent mouseEvent)
    {
        view.zoom += 0.1;
        super.mouseWheelMovedUp(mouseEvent);
    }

    void mouseWheelMovedDown(MouseEvent mouseEvent)
    {
        if( view.zoom > 0.21 )
                 view.zoom -= 0.2;
        super.mouseWheelMovedDown(mouseEvent);
    }

    mixin BindMethods!(load);
}
