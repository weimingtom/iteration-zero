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

        auto topWidget = cast(Container)engine.gui.getTop;
        topWidget.setId( "top" );
        auto window = new Window("Party"d);
        window.setPosition (100, 100);
        window.setSize (300, 300);
        int y = 5;
        foreach(Character c; party)
        {
            auto label = new Label(toUTF32(c.getName()));
            window.add(label, 5, y);
            y += 20;
        }

        window.setId( "window" );

        topWidget.add( window );

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
        view.trackObject (turnManager.getCurrent());
        active = party.getActive();

        int x,y;
        int clicked = SDL_GetMouseState(&x,&y) & SDL_BUTTON(1);
        view.toLevelCoords(&x,&y);
        // FUGLY
        view.sceneMode();
        view.drawHighlight(x,y);

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
    }

    void mouseWheelMovedUp(MouseEvent mouseEvent)
    {
        view.zoom += 0.1;
    }

    void mouseWheelMovedDown(MouseEvent mouseEvent)
    {
        if( view.zoom > 0.21 )
                 view.zoom -= 0.2;
    }

    mixin BindMethods!(load);
}
