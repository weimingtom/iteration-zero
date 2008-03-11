//------------------------------------------------------------------------------------------
// OpenGL Material Class
//    Copyright (c) kei mesuda 2007
//    (http://www.yomogi.sakura.ne.jp/~si)
//    Modified klaus blindert 2008
//------------------------------------------------------------------------------------------

module material;

private {
    import std.stdio;
    import std.conv;
    import std.utf;
    import derelict.opengl.gl;
    import derelict.opengl.glu;
    import texture;
    import vector;
    import util;
    import sofu = Sofu.Sofu;

    TextureManager _textures;
    Material[char[]] _materials;
}

static this()
{
    _textures = new TextureManager;
}

Material getMaterial(char[] name)
{
    return _materials[name];
}

void loadMaterials(char[] filename)
{
    sofu.Map map = sofu.loadFile(filename);
    foreach(wchar[] materialName, sofu.SofuObject attrib; map) {
        writefln("Loading Material: '%s'", materialName);
        sofu.Map d = attrib.asMap();
        Material mat = new Material;

        if( d.hasList("color") )
        {
            sofu.List color = d.list("color");
            mat.setColor( color.value(0).toFloat(), color.value(1).toFloat(), color.value(2).toFloat(), color.value(3).toFloat() );
        }
        if( d.hasList("light") )
        {
            sofu.List color = d.list("light");
            mat.setLight( color.value(0).toFloat(), color.value(1).toFloat(), color.value(2).toFloat(), color.value(3).toFloat(), color.value(4).toFloat() );
        }
        if( d.hasValue("texture") )
        {
            mat.texture = _textures.load(d.value("texture").toString());
        }
        _materials[toUTF8(materialName)] = mat;
    }
    _materials.rehash;
}

class Material
{
public:
    vector4     color;        // material color
    vector4     colAmb;       // ambient light color
    vector4     colDiff;      // diffuse
    vector4     colSpec;      // specular
    vector4     colEmi;       // emission
    float       fPhong=0;     // phong
    Texture    texture=null; // texture


public:
    // default constructor
    this() { }

    // copy constructor
    this(Material src) { copy(src); }

    // copy
    Material copy(Material src)
    {
        color = src.color;
        colAmb = src.colAmb;
        colDiff = src.colDiff;
        colSpec = src.colSpec;
        colEmi = src.colEmi;
        fPhong = src.fPhong;
        texture = src.texture;
        return this;
    }


    // set basic color
    Material setColor(float red, float green, float blue, float alph)
    {
        set(&color, red, green, blue, alph);
        return this;
    }


    // set lighting parameter
    Material setLight(float amb, float diff, float spec, float phong, float emi)
    {
        scale(&colAmb,  &color, amb);
        scale(&colDiff, &color, diff);
        scale(&colSpec, &color, spec);
        fPhong = phong * 128;
        scale(&colEmi,  &color, emi);
        return this;
    }


    // set parameter by ubyte[9] = {a,g,b,a,amb,dif,spec,phong,emi}
    Material setByByte(ubyte[] data)
    {
        setColor(ubyte2float(data[0]), ubyte2float(data[1]), ubyte2float(data[2]), ubyte2float(data[3]));
        setLight(ubyte2float(data[4]), ubyte2float(data[5]), ubyte2float(data[6]), ubyte2float(data[7]), ubyte2float(data[8]));
        return this;
    }


    // set texture
    Material setTexture(Texture texture_)
    {
        texture = texture_;
        return this;
    }


    // get parameter by ubyte[9] = {a,g,b,a,amb,dif,spec,phong,emi}
    Material getByByte(ubyte[] data)
    {
        float amb, diff, spec, phong, emi;
        getLight(amb, diff, spec, phong, emi);
        data[0] = float2ubyte(color.r);
        data[1] = float2ubyte(color.g);
        data[2] = float2ubyte(color.b);
        data[3] = float2ubyte(color.a);
        data[4] = float2ubyte(amb);
        data[5] = float2ubyte(diff);
        data[6] = float2ubyte(spec);
        data[7] = float2ubyte(phong);
        data[8] = float2ubyte(emi);
        return this;
    }


    // get basic color
    Material getColor(out float red, out float green, out float blue, out float alph)
    {
        red   = color.r;
        green = color.g;
        blue  = color.b;
        alph  = color.a;
        return this;
    }


    // get lighting parameter
    Material getLight(out float amb, out float diff, out float spec, out float phong, out float emi)
    {
        float collen = length(&color.v3);
        if (collen == 0) {
            amb   = 0;
            diff  = 0;
            spec  = 0;
            phong = 0;
            emi   = 0;
        } else {
            amb   = length(&colAmb.v3)  / collen;
            diff  = length(&colDiff.v3) / collen;
            spec  = length(&colSpec.v3) / collen;
            phong = fPhong / 128;
            emi   = length(&colEmi.v3)  / collen;
        }
        return this;
    }


    // glMaterial*()
    void draw(GLenum front_back = GL_FRONT)
    {
        glColor4fv(&color.x);
        glMaterialfv(front_back, GL_AMBIENT,   &colAmb.x);
        glMaterialfv(front_back, GL_DIFFUSE,   &colDiff.x);
        glMaterialfv(front_back, GL_SPECULAR,  &colSpec.x);
        glMaterialf (front_back, GL_SHININESS, fPhong);
        glMaterialfv(front_back, GL_EMISSION,  &colEmi.x);
        if (texture !is null)
        {
            texture.bind();
        } else 
        {
            glBindTexture(GL_TEXTURE_2D, 0);
        }
    }


    // glMaterial*() without doubling
    Material compile(GLenum front_back = GL_FRONT)
    {
        vector4 vc;
        if (texture !is null) texture.bind();

        glColor4fv(&color.x);
        glGetMaterialfv(front_back, GL_AMBIENT,   &vc.x);
        if (!pisEqual(&vc, &colAmb))  glMaterialfv(front_back, GL_AMBIENT,   &colAmb.x);
        glGetMaterialfv(front_back, GL_DIFFUSE,   &vc.x);
        if (!pisEqual(&vc, &colDiff)) glMaterialfv(front_back, GL_DIFFUSE,   &colDiff.x);
        glGetMaterialfv(front_back, GL_SPECULAR,  &vc.x);
        if (!pisEqual(&vc, &colSpec)) glMaterialfv(front_back, GL_SPECULAR,  &colSpec.x);
        glGetMaterialfv(front_back, GL_SHININESS, &vc.x);
        if (vc.x != fPhong)           glMaterialf(front_back,  GL_SHININESS, fPhong);
        glGetMaterialfv(front_back, GL_EMISSION,  &vc.x);
        if (!pisEqual(&vc, &colEmi))  glMaterialfv(front_back, GL_EMISSION,  &colEmi.x);

        return this;
    }


    // register texture
    Material register()
    {
        if (texture !is null) texture.register();
        return this;
    }


    // unregister texture
    Material unregister()
    {
        if (texture !is null) texture.unregister();
        return this;
    }
}