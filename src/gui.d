module gui;

private import
    engine,
    pipeline,
    font,
    derelict.opengl.gl;

class GuiRenderer : Renderer
{
    Engine engine;
    Font sfont;
    string[] text;

    this()
    {
        super("gui");
        engine = Engine.instance;
        sfont = new Font ("data/verdana.ttf",16);
    }

    void render()
    {
        /+glMatrixMode(GL_PROJECTION);
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
/+
        sfont.draw(tex+/t,[10.0f,10.0f],[1.0f,1.0f,1.0f,1.f]);+/
    }
}