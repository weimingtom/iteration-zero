import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;

class TestState : IGameState
{
    public:
        Engine engine;
        LevelRenderer view;

    this()
    {
        engine = Engine.instance;
        view = new LevelRenderer;
        engine.renderPipeline.add( new GridRenderer );
        engine.renderPipeline.add( view );
    }

    void start()
    {
        engine.renderPipeline["grid"].enabled = false;
        engine.renderPipeline["level"].enabled = true;

        view.level = new Level ("data/levels/test.lvl");
        view.lookAt (0, 0);

        with(luigui)
        {
            alias BorderArranger.Region Region;
            auto wgroup = engine.gui.add_arranged (new FlowPanel (Alignment.Top, Gaps (0)), Region.West);
            auto wtiles = wgroup.add_arranged (new Button ("Blarg"));

            auto group = engine.gui.add_arranged (new FlowPanel (Alignment.Left, Gaps (0)), Region.North);

            auto b_quit = group.add_arranged (new Button ("Quit"));
            auto b_save = group.add_arranged (new Button ("Save"));
            auto b_tiles = group.add_arranged (new Button ("Tiles"));

            b_quit.clicked ~= (Widget w){ 
                // engine.stop(); //SEGFAULT ???
                Engine.instance.stop();
            };
            b_tiles.clicked.connect( &wgroup.toggle_shown );
        }
    }

    void stop()
    {
    }

    void logic()
    {
        int x,y;
        int clicked = SDL_GetMouseState(&x,&y) & SDL_BUTTON(1);
        view.toLevelCoords(&x,&y);
        view.drawHighlight(x,y);
        view.lookAt(view.level.gobjects[0].real_x,view.level.gobjects[0].real_y);
    }

    string name() { return "test"; }

    void handleKeyEvent(SDL_Event event) {}
    void handleMouseMotionEvent(SDL_Event event) {}
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

void main(char[][] argv) {
    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    engine.addState( new TestState );
    engine.start( "test" );

    engine.mainLoop();
}
