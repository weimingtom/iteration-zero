module level;

private {
    import std.stdio;
    import std.utf;
    import std.math : sqrt;
    import material;
    import sofu = Sofu.Sofu;

    import engine;
    import derelict.sdl.sdl;
    import derelict.opengl.gl;
    import derelict.opengl.glu;

    import pipeline;
    import vector;
    import util;

    import interfaces;
    import dataset;
    import gobject;
    import party;
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

    private:
        Party _party;

    public:

    this(string filename)
    {
        dataset = Dataset.instance;
        _party = new Party;

        //Engine.instance.dlisp.environment.pushScope();
        bindInstance(Engine.instance.dlisp.environment,"*LEVEL*");

        Engine.instance.dlisp.parseEvalPrint("(LOAD \"" ~ filename ~ "\" T)", true);//"
        //Engine.instance.dlisp.environment.popScope();
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
        return tiles[y*width +x].getObjects();
    }

    GObject[] getAllObjects()
    {
        return gobjects;
    }

    void placeTileByName(int x, int y, string name)
    {
        if( isValid(x,y) )
            tiles[y*width + x] = dataset._prototypes[name].create(x,y);
    }

    void placeObjectByName(int x, int y, string name)
    {
        if( !isValid(x,y) )
            throw new Exception("placeObjectByName: Coordinates not valid.");
        if( !(name in dataset._objects) )
            throw new Exception("placeObjectByName: Unknown object:" ~ name);
         gobjects ~= dataset._objects[name].create (this, x, y);
    }

    void placeTile(int x, int y, Tile tile)
    {
        if( isValid(x,y) )
            tiles[y*width + x] = tile;
    }

    void placeIObject(int x, int y, IGObject o) { placeObject(x,y,cast(GObject)o);}

    void placeObject(int x, int y, GObject gobject)
    {
        if( isValid(x,y) )
        {
            gobject.setLevel(this);
            gobject.setPosition (x,y);
            gobjects ~= gobject;
        }
    }

    bool isBlocked (int x, int y)
    {
        if( !isValid (x, y) )
            return true;
        return getTile (x, y).isBlocking();
    }

    float distance(int x0, int y0, int x1, int y1)
    {
        return cast(float)(abs(x0 - x1) + abs(y0-y1));
    }

    void setName(string name_ ) { name = name_; }
    string getName() {  return name; }

    Party getParty() { return _party; }
    void setParty(Party p) { _party = p; }

    mixin BindClass!("C/LEVEL");
    mixin BindConstructor!(Level function(string));
    mixin BindMethods!(init,finalize,isValid,loadDataset,getTile,getObjects,getAllObjects);
    mixin BindMethods!(setName,getName,getParty);
    mixin BindMethods!(placeTileByName,placeObjectByName,placeTile,placeObject);
}

