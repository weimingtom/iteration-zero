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
import guichan.all;
import guichan.widgets.all;
import guichan.event;


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

        auto topWidget = cast(Container)engine.gui.getTop;
        topWidget.setId( "top" );
        auto window = new Window("Hello World!"d);
        window.setPosition (100, 100);
        window.setSize (300, 300);
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

    string name() { return "test"; }

    void keyPressed(KeyEvent event) {}
    void keyReleased(KeyEvent event) {}

    void mouseEntered(MouseEvent mouseEvent) { }
    void mouseExited(MouseEvent mouseEvent) { }
    void mousePressed(MouseEvent mouseEvent) { }
    void mouseReleased(MouseEvent mouseEvent) { }
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

//         if( event.button.button == SDL_BUTTON_WHEELUP )
//         {
//             view.zoom += 0.1;
//         }
//         if( event.button.button == SDL_BUTTON_WHEELDOWN )
//         {
//             if( view.zoom > 0.21 )
//                 view.zoom -= 0.2;
//         }
//     }

    void mouseWheelMovedUp(MouseEvent mouseEvent) { }
    void mouseWheelMovedDown(MouseEvent mouseEvent) { }
    void mouseMoved(MouseEvent mouseEvent) { }
    void mouseDragged(MouseEvent mouseEvent) { }

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
