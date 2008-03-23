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
/+//         window.setVisible(true);
        auto label = new Label("Yo Label");
        label.setId( "label" );
        window.add( label, 20, 20);

        auto button = new Button("Yo Button");
        button.setId( "button" );
        window.add( button, 20, 40);

//         auto slider = new Slider(0.0,100.0);
//         slider.setId( "slider" );
//         slider.setSize(100,10);
//         window.add( slider, 20, 60);

        auto cbox = new CheckBox("Checkidecheck");
        cbox.setId( "checkbox" );
        window.add( cbox, 20, 80);

        auto rb1 = new RadioButton("Yo Radio 1 Ü"d,"1");
        rb1.setId( "radio1" );
        window.add( rb1, 20, 100);

        auto rb2 = new RadioButton("Yo Radio 1 Ä"d,"1");
        rb2.setId( "radio2" );
        window.add( rb2, 20, 120);

        auto rtf = new TextField("UÜÜÖßßß Ä"d);
        rtf.setId( "textfield" );
        rtf.setSize( 100,20 );
        window.add( rtf, 20, 140);

        auto lbox = new ListBox(new ListModel!(string[])(["ALOPHA","BETA","GAMMELN","DELTA","uiiuiu","Llklkjluiouoiuoiuo","u","u","uizuiz"]));
        lbox.setId( "lbox" );
        lbox.setSize(100,200);

        auto sarea = new ScrollArea(lbox);
        sarea.setSize(50,50);
+/
//         window.add( sarea, 140, 20);
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
