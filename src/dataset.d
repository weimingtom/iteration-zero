module dataset;

private {
    import std.stdio;
    import std.utf;
    import material;
    import sofu = Sofu.Sofu;

    import engine;
    import derelict.sdl.sdl;
    import derelict.opengl.gl;
    import derelict.opengl.glu;

    import vector;

    import modelmesh;

    import dlisp.bind;
}

public import
    gobject,
    tile;

class TilePrototype
{
    public:

    string name;
    bool blocking;

    StaticMesh _mesh;

    this(string name_, sofu.Map info)
    {
        name = name_;
        _mesh = new StaticMesh(name_);

        writefln ("Loading Tile Prototype: '%s'",name);
        blocking = info.value("blocking").toInt() != 0;

        foreach(sofu.SofuObject element; info.list("faces"))
        {
            writefln ("Face added: %s", element.asList().value(0).toString());
            Face face = Dataset.instance.getFace( element.asList().value(0).toString() );
            string material = element.asList().value(1).toString();
            _mesh.addFace( material, face);
        }
    }

    Tile create(int x, int y)
    {
        Tile t = new Tile(_mesh,x,y);
        return t;
    }

    mixin BindClass!("C/TILE-PROTOTYPE");
    mixin BindMethods!(create);
}


class Dataset
{
    static Dataset _instance;
    this()
    {
        _instance = this;
    }

    public:
        Face[string] _faces;

        TilePrototype[string]   _prototypes;
        GObjectPrototype[string] _objects;

    static Dataset instance()
    {
        if(_instance is null)
            _instance = new Dataset;
        return _instance;
    }

    void load(string filename)
    {
        writefln ("Loading Dataset: %s", filename);
        sofu.Map map = sofu.loadFile(filename);

        sofu.List materials = map.list("materials");
        foreach(sofu.SofuObject element; materials)
        {
            material.loadMaterials(element.toString());
        }

        sofu.Map faces = map.map("faces");
        foreach(wchar[] name, sofu.SofuObject element; faces)
        {
            writefln( "-- Loading Face Type: %s", name);
            _faces[toUTF8(name)] = Face(element.asList());
        }

        sofu.Map protoMap = map.map("prototypes");
        foreach(wchar[] name, sofu.SofuObject element; protoMap)
        {
            writefln( "-- Loading Tile Type: %s", name);
            _prototypes[toUTF8(name)] = new TilePrototype(toUTF8(name), element.asMap());
        }

        sofu.Map entityMap = map.map("objects");
        foreach(wchar[] name, sofu.SofuObject element; entityMap)
        {
            writefln( "-- Loading Object Type: %s", name);
            _objects[toUTF8(name)] = new GObjectPrototype(toUTF8(name), element.asMap());
        }
    }

    TilePrototype getTilePrototype(string name)
    {
      return _prototypes[name];
    }

    GObjectPrototype getGObjectPrototype(string name)
    {
      return _objects[name];
    }

    Face getFace(string name) { return _faces[name]; }

    mixin BindClass!("C/DATASET");
    mixin BindMethods!(load,getTilePrototype,getGObjectPrototype);
}
