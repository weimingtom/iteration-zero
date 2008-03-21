module izedit;

import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;
import util;
import derelict.opengl.gl;

import guichan.widgets.all;

class EditState : IGameState
{
    public:
        Engine engine;
        LevelRenderer view;
        string filename;

        int tileproto = 0;
        string tileprotoname;


    this(char[] filename_)
    {
        filename = filename_;
        engine = Engine.instance;
        view = new LevelRenderer;
        engine.renderPipeline.add (view);
    }

    void start()
    {
        engine.renderPipeline.setPipeline(["level"]);

        view.level = new Level (filename);
        view.lookAt (0, 0);

        auto keys = view.level.dataset._prototypes.keys;
        tileprotoname = keys[tileproto];
        

        auto topWidget = cast(Container)engine.gui.getTop;
        topWidget.setId( "top" );
        auto window = new Window("Hello World!"d);
        window.setPosition (100, 100);
        window.setSize (300, 300);
        window.setId( "window" );
//         window.setVisible(true);
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

        window.add( sarea, 140, 20);

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
        int x,y;
        int clicked = SDL_GetMouseState(&x,&y) & SDL_BUTTON(1);
        view.toLevelCoords(&x,&y);
        // FUGLY
//         view.sceneMode();
//         view.drawHighlight(x,y);
//         writefln ("Looking at (%f %f)", view.level.gobjects[0].real_x,view.level.gobjects[0].real_y);
//         view.lookAt(view.level.gobjects[0].real_x,view.level.gobjects[0].real_y);
    }

    string name() { return "level"; }

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
        if( !view.level.isValid(x,y))
          return;
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
    if( argv.length < 2 )
    {
        writefln("usage: iz-play levelfile");
        return;
    }


    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    engine.addState( new EditState (argv[1]) );
    engine.start ("level");

    engine.mainLoop();
}
