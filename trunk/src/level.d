module level;

private {
    import std.stdio;
    import std.utf;
    import material;
    import sofu = Sofu.Sofu;

    import engine;
    import derelict.sdl.sdl;
    import derelict.opengl.gl;
    import derelict.opengl.glu;

    import pipeline;
    import vector;
    import util;

    import baseClasses;
    import dataset;
    import gobject;
    import dlisp.dlisp;
    import dlisp.bind;
    import dlisp.predefs.all;
}

class Level : ILevel
{
    public:
    string name;
    int width;
    int height;

    Dataset dataset;

    Tile[] tiles;
    GObject[] gobjects;

    this(string filename)
    {
        dataset = new Dataset;

        Engine.instance.dlisp.environment.pushScope();
        bindInstance(Engine.instance.dlisp.environment,"*LEVEL*");

        Engine.instance.dlisp.parseEvalPrint("(LOAD \"" ~ filename ~ "\" T)", true);//"
        Engine.instance.dlisp.environment.popScope();
    }

    void init(int w_, int h_)
    {
        width  = w_;
        height = w_;

        tiles.length = width*height;
        writefln ("Loading Level: %s [size=(%d %d)]",name,width,height);
    }

    void finalize()
    {
        string defaultTile = "normal";//map.value("default_tile").toString();
        for(int i = 0; i != width*height; ++i)
            if( tiles[i] is null )
                tiles[i] = dataset._prototypes[defaultTile].create(i%width,i/width);

        writefln ("Loading Level: %s ... finished",name);
    }

    bool isValid(int x, int y)
    {
        return x >= 0 && x < width && y >= 0 && y < height;
    }

    void loadDataset(string filename)
    {
        dataset.load(filename);
    }

    Tile getTile(int x, int y)
    {
        return tiles[y*width +x];
    }

    GObject[] getObjects(int x, int y)
    {
        return tiles[y*width +x].getObjects;
    }

    GObject[] getAllObjects()
    {
        return gobjects;
    }

    void placeTile(int x, int y, string name)
    {
        if( isValid(x,y) )
            tiles[y*width + x] = dataset._prototypes[name].create(x,y);
    }

    void placeObject(int x, int y, string name)
    {
        if( isValid(x,y) )
            gobjects ~= dataset._objects[name].create (this, x, y);
    }

    bool isBlocked (int x, int y)
    {
        if( !isValid (x, y) )
            return true;
        return getTile (x, y). blocking;
    }

    float distance(int x0, int y0, int x1, int y1)
    {
        return cast(float)(abs(x0 - x1) + abs(y0-y1));
    }

    void setName(string name_ )
    {
        name = name_;
    }

    string getName()
    {
        return name;
    }

    mixin BindClass!("C/LEVEL");
    mixin BindConstructor!(Level function(string));
    mixin BindMethods!(init,finalize,isValid,placeTile,placeObject,loadDataset,getTile,getObjects,getAllObjects);
    mixin BindMethods!(setName);
}

class LevelRenderer : Renderer
{
    private:
        matrix4 projMatrix;
        matrix4 viewMatrix;

    public:
        Level level;

        float zoom = 5.0;
    
        float look_x,look_y;

        this()
        {
            super("level");
        }

        void sceneMode()
        {
            glMatrixMode(GL_PROJECTION);
            glLoadIdentity();
            glOrtho(-40.0f, 40.0f, -30.0f, 30.0f, -200.0f, 200.0f);

            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            glRotatef(-45.0f, 1.0f, 0.0f, 0.0f);
            glRotatef(45.0f, 0.0f, 0.0f, 1.0f);
            glScalef(zoom, zoom, zoom);

            glTranslatef(-look_x,-look_y,0);

            glGetFloatv(GL_PROJECTION_MATRIX,&projMatrix.a00);
            glGetFloatv(GL_MODELVIEW_MATRIX,&viewMatrix.a00);
            invert(&projMatrix);
            invert(&viewMatrix);
        }

        void lookAt(float x,float y)
        {
            look_x = x;
            look_y = y;
            level.centerx = cast(int)x;
            level.centery = cast(int)y;
        }

        void toLevelCoords(int* x, int* y)
        {
            vector4 screenCoords;
            long w_ = Engine.instance.xResolution;
            long h_ = Engine.instance.yResolution;
            float fx = 2*(cast(float)*x/w_ - 0.5);
            float fy = 4*(.5-cast(float)*y/h_);
            set(&screenCoords,fx,fy,0.0f,1.0);
            mul(&screenCoords,&projMatrix);
            mul(&screenCoords,&viewMatrix);
            *x = cast(int)screenCoords.x;
            *y = cast(int)screenCoords.y;
        }

        void drawHighlight(int x, int y)
        {
            float z_offset = 0.01;
            glColor4f(1,0,0,1);
            glBegin(GL_LINE_LOOP);
                glVertex3f( x+1, y+1, z_offset );       /* NE */
                glVertex3f( x, y+1, z_offset );       /* NW */
                glVertex3f( x, y, z_offset );       /* SW */
                glVertex3f( x+1, y, z_offset ); 
            glEnd();

            glBegin(GL_LINE_LOOP);
                glVertex3f( x+1, y+1, z_offset+1 );       /* NE */
                glVertex3f( x, y+1, z_offset+1 );       /* NW */
                glVertex3f( x, y, z_offset+1 );       /* SW */
                glVertex3f( x+1, y, z_offset+1 ); 
            glEnd();

            glColor4f(0,1,0,1);
            glBegin(GL_LINES);
                glVertex3f( x, y, z_offset );       /* NE */
                glVertex3f( x, y, z_offset+1 );       /* NW */
                glVertex3f( x+1, y, z_offset );       /* SW */
                glVertex3f( x+1, y, z_offset+1 ); 
                glVertex3f( x, y+1, z_offset );       /* NE */
                glVertex3f( x, y+1, z_offset+1 );       /* NW */
                glVertex3f( x+1, y+1, z_offset );       /* SW */
                glVertex3f( x+1, y+1, z_offset+1 ); 
            glEnd();
        }

        void render()
        {
            sceneMode();
            glEnable(GL_TEXTURE_2D);
            if( level is null )
                return;

            for(int x = 0; x < level.width; ++x)
                for(int y = 0; y < level.height; ++y)
                    level.getTile(x,y).draw();

            foreach(GObject gobject; level.getAllObjects())
            {
                gobject.draw();
            }
        }
}
