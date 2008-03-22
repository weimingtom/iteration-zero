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
    import model;

    import dlisp.bind;
}

public import
    interfaces,
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
        t.setBlocking( blocking );
        return t;
    }

    mixin BindClass!("C/TILE-PROTOTYPE");
    mixin BindMethods!(create);
}

class GObjectPrototype
{
    private:
        string _name;
        ObjectType _type;
    public:
        Model model;

        bool selectable = false;
        bool active     = false;
        bool blocking   = true;

        float speed = 1.0;

        this(char[] name_, sofu.Map map)
        {
            _name = name_;
            model = Dataset.instance.getModel(map.value("model").toString());

            if( map.hasAttribute("speed") )
                speed = map.value("speed").toFloat();

            if( map.hasAttribute("blocking") )
                blocking = map.value("blocking").toInt() == 1;
        }

        GObject create(ILevel level, int x, int y)
        {
            auto gobject = new GObject;
            gobject.setModel (model);
            gobject.setLevel (level);
            gobject.setPosition (x,y);
            return gobject;
        }

        string getName() { return _name; }
        void setName(string name) { _name = name; }

        mixin BindClass!("C/OBJECT-PROTOTYPE");
        mixin BindMethods!(getName,setName);
}

class Dataset
{
    static Dataset _instance;
    this()
    {
        _instance = this;
        bindInstance(Engine.instance.dlisp.environment,"*dataset*");
    }

    public:
        Face[string] _faces;

        TilePrototype[string]   _prototypes;
        GObjectPrototype[string] _objects;
        Model[string] _models;

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

        sofu.Map modelMap = map.map("models");
        foreach(wchar[] name, sofu.SofuObject element; modelMap)
        {
            writefln( "-- Loading Model Type: %s", name);
            _models[toUTF8(name)] = loadFromSofu(element.asMap());
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

    Model getModel(string name)
    {
      return _models[name].clone();
    }

    Face getFace(string name) { return _faces[name]; }

    mixin BindClass!("C/DATASET");
    mixin BindMethods!(load,getTilePrototype,getGObjectPrototype,getModel);
}
