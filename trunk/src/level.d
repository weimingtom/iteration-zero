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
}

private class Chunk
{
    public:
        Level level;
        int x0,y0,w,h;
        int uidDisplayList = 0;

    this(Level level_, int x0_, int y0_, int w_, int h_)
    {
        level = level_;
        x0 = x0_;
        y0 = y0_;
        w = w_;
        h = h_;
    }

    void compile()
    {
        uidDisplayList = glGenLists(1);
        if (uidDisplayList == 0)
            throw new Exception("Failed to create display list");

        glNewList(uidDisplayList, GL_COMPILE);
        for(int y=y0+h-1; y >= y0; -- y)
        {
            for(int x=x0+w-1; x >= x0; -- x)
            {
                level.getTile(x,y).compile();
            }
        }
        glEndList();
    }

    void draw()
    {
        if(uidDisplayList == 0)
            compile();

        glCallList(uidDisplayList);
    }
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

    int chunk_width;
    int chunk_height;
    int xchunks;
    int ychunks;
    Chunk[] chunks;

    int centerx;
    int centery;

    this(string filename)
    {
        sofu.Map map = sofu.loadFile(filename);
        name = map.value("name").toString();
        width  = map.list("size").value(0).toInt();
        height = map.list("size").value(1).toInt();

        chunk_width  = 4;
        chunk_height = 4;
        
        width  = width + (chunk_width - width % chunk_width);
        height = height + (chunk_height - height % chunk_height);

        xchunks = width/chunk_width;
        ychunks = height/chunk_height;

        tiles.length = width*height;
        chunks.length = xchunks * ychunks;

        dataset = new Dataset(map.list("datasets"));
        writefln ("Loading Level: %s [size=(%d %d)]",name,width,height);

        sofu.List tilesInfos = map.list("tiles");
        foreach(sofu.SofuObject element; tilesInfos)
        {
            sofu.List tileInfo = element.asList();
            int x = tileInfo.list(0).value(0).toInt();
            int y = tileInfo.list(0).value(1).toInt();
            string proto = tileInfo.value(1).toString();
            if( (proto in dataset._prototypes) is null)
            {
                throw new Exception("Unknown prototype: " ~ proto);
            }
            tiles[y*width + x] = dataset._prototypes[proto].create(x,y);
        }
        writefln ("%d Tiles created",width*height);

        sofu.List objectInfos = map.list("objects");
        foreach(sofu.SofuObject element; objectInfos)
        {
            sofu.List objectInfo = element.asList();
            int x = objectInfo.list(0).value(0).toInt();
            int y = objectInfo.list(0).value(1).toInt();
            string proto = objectInfo.value(1).toString();
            if( (proto in dataset._objects) is null)
            {
                throw new Exception ("Unknown object: " ~ proto);
            }
            gobjects ~= dataset._objects[proto].create (this, x, y);
        }
        writefln ("%d GObjects created", gobjects.length);

        string defaultTile = map.value("default_tile").toString();
        for(int i = 0; i != width*height; ++i)
            if( tiles[i] is null )
                tiles[i] = dataset._prototypes[defaultTile].create(i%width,i/width);

        for(int x = 0; x != xchunks; ++x)
        {
            for(int y = 0; y != ychunks; ++y)
            {
                chunks[y*xchunks + x] = new Chunk (this, x*chunk_width, y*chunk_height, chunk_width, chunk_height);
            }
        }
        writefln ("Loading Level: %s ... finished",name);

        centerx = width/2;
        centery = width/2;
    }

    bool isValid(int x, int y)
    {
        return x >= 0 && x < width && y >= 0 && y < height;
    }

    Tile getTile(int x, int y)
    {
        return tiles[y*width +x];
    }

    bool isBlocked (int x, int y)
    {
        if( !isValid (x, y) )
            return true;
        return getTile (x, y). blocking;
    }

    void draw()
    {
        int xc = centerx/chunk_width;
        int yc = centery/chunk_height;

        int dx = 5;
        int dy = 5;

        for(int x = xc - dx; x != xc +dx; ++x)
        {
            if( x < 0 || x >= xchunks )
                continue;
            for(int y = yc - dy; y != yc +dy; ++y)
            {
                if( y < 0 || y >= ychunks )
                    continue;
                chunks[y*xchunks+x].draw();
            }
        }

        foreach(GObject gobject; gobjects)
        {
            gobject.draw();
        }
    }

    float distance(int x0, int y0, int x1, int y1)
    {
        return cast(float)(abs(x0 - x1) + abs(y0-y1));
    }
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

            /+
            glColor4f(1,0,1,1);
            glBegin(GL_LINES);
                glVertex3f( look_x, look_y, 0 );       /* NE */
                glVertex3f( x, y, 0 );       /* NW */
            glEnd();
            +/
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
            level.draw();
        }
}
