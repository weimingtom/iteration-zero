module sdltest;

import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;

import font;
import 
	derelict.opengl.gl,
	derelict.freetype.ft;
// 	derelict.util.loader,
// 	derelict.util.exception,
// 	derelict.opengl.extension.ext.blend_color;

class TestState : IGameState
{
    public:
        Engine engine;
        LevelRenderer view;
        Font sfont;

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

        sfont = new Font ("data/verdana.ttf",80);

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
        view.drawHighlight(x,y);
        view.lookAt(view.level.gobjects[0].real_x,view.level.gobjects[0].real_y);

        char[][] hello = [ "Hello World!", "Fuck THIS CITY." ];

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrtho(0, engine.xResolution, engine.yResolution, 0, -100, 100);

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        glColor4f(1,1,1,1);
        glLineWidth(2);
        glBegin(GL_LINE_LOOP);
            glVertex2f(1,1);
            glVertex2f(engine.xResolution-2,1);
            glVertex2f(engine.xResolution-2,engine.yResolution-2);
            glVertex2f(1,engine.yResolution-2);
        glEnd();
        sfont.draw(hello,[10.0f,10.0f],[1.0f,1.0f,1.0f,0.7f]);
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

void main(char[][] argv)
{
    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    engine.addState( new TestState );
    engine.start( "test" );

    engine.mainLoop();
}
