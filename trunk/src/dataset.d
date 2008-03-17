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

    import gobject;

    import dlisp.bind;
}

class FaceInfo
{
    public:
        float[][] data;

    this(sofu.List list)
    {
        foreach(sofu.SofuObject f; list)
        {
            float[] face;
            for(int i=0; i!=8;++i)
            {
                face ~= f.asList().value(i).toFloat();
            }
            data ~= face;
        }
    }

    void draw(float dx, float dy, float dz)
    {
        for(int i=0; i!= data.length; ++i)
        {
            glTexCoord2f(data[i][3],data[i][4]);
            glNormal3f(data[i][5],data[i][6],data[i][7]);
            glVertex3f(data[i][0]+dx,data[i][1]+dy,data[i][2]+dz);
        }
    }
}

class Face
{
    public:

    FaceInfo _faceInfo;
    Material _material;

    this(FaceInfo fi)
    {
        _faceInfo = fi;
    }
}

class Tile
{
    private:
        TilePrototype _prototype;
        Face[] _faces;
        int _x, _y;

        GObject[] _gobjects;
    public:
        this(TilePrototype prototype, int x_, int y_)
        {
            _prototype = prototype;
            _faces.length = prototype.faces.length;
            for(int i=0; i != prototype.faces.length; ++i)
            {
                _faces[i] = new Face(prototype.faces[i].faceinfo);
            }
            _x = x_;
            _y = y_;
        }

        Material getMaterial(int faceIndex)
        {
            if( _faces[faceIndex]._material is null )
            {
                _faces[faceIndex]._material = material.getMaterial(_prototype.faces[faceIndex].materialName);
                _faces[faceIndex]._material.register();
            }
            return _faces[faceIndex]._material;
        }

        void draw()
        {
            glEnable(GL_TEXTURE_2D);
            for(int i=0; i!=_faces.length; ++i)
            {
                getMaterial(i).draw();
                glBegin(GL_QUADS);
                _faces[i]._faceInfo.draw(x,y,0);
                glEnd();
            }
        }

        void compile()
        {
            for(int i=0; i!=_faces.length; ++i)
            {
                getMaterial(i).compile();
                glBegin(GL_QUADS);
                _faces[i]._faceInfo.draw(x,y,0);
                glEnd();
            }
        }

        int x() { return _x; }
        int y() { return _y; }

        bool blocking()
        {
            if( _prototype.blocking )
                return true;

            foreach(GObject gobject; _gobjects)
            {
                if( gobject.blocking )
                    return true;
            }
            return false;
        }

        GObject[] gobjects() { return _gobjects; }

        mixin BindClass!("C/TILE");
}

struct TileFace
{
    FaceInfo faceinfo;
    string materialName;
}

class TilePrototype
{
    public:

    string name;
    bool blocking;

    TileFace[] faces;

    this(Dataset data, string name_, sofu.Map info)
    {
        name = name_;
        writefln ("Loading Tile Prototype: '%s'",name);
        blocking = info.value("blocking").toInt() != 0;

        foreach(sofu.SofuObject element; info.list("faces"))
        {
            writefln ("Face added: %s", element.asList().value(0).toString());
            TileFace f;
            f.faceinfo = data._faceInfos[ element.asList().value(0).toString() ];
            f.materialName = element.asList().value(1).toString();
            faces ~= f;
        }
    }

    Tile create(int x, int y)
    {
        Tile t = new Tile(this,x,y);
        return t;
    }

    mixin BindClass!("C/TILE-PROTOTYPE");
    mixin BindMethods!(create);
}

class Dataset
{
    public:
        FaceInfo[string] _faceInfos;

        TilePrototype[string]   _prototypes;
        GObjectPrototype[string] _objects;

    this()
    {
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
            _faceInfos[toUTF8(name)] = new FaceInfo(element.asList());
        }

        sofu.Map protoMap = map.map("prototypes");
        foreach(wchar[] name, sofu.SofuObject element; protoMap)
        {
            writefln( "-- Loading Tile Type: %s", name);
            _prototypes[toUTF8(name)] = new TilePrototype(this, toUTF8(name), element.asMap());
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

    mixin BindClass!("C/DATASET");
    mixin BindMethods!(load,getTilePrototype,getGObjectPrototype);
}
