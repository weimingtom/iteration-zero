module model;

import std.stdio;
import derelict.opengl.gl;
import sofu = Sofu.Sofu;

import engine;
import texture;
import modelmesh;
import md2;
import dlisp.bind;

class Model
{
protected:
    Mesh _mesh;
    string _name = "__unnamed__";
    float  _scale = .05;       // scale value

    float[3]   _pre_translation  = [0, 0, 0];
    float[3]   _post_translation = [0, 0, 0];
    float[4][] _rotations;

    float _facing = 0.0;

    public:

    this()
    {
    }

    string getName() { return _name; }
    void setName(string name) { _name = name; }

    float getScale() { return _scale; }
    void setScale(float scale) { _scale = scale; }

    void setPreTranslation(in float[3] t) { _pre_translation[] = t[]; }
    void setPostTranslation(in float[3] t) { _post_translation[] = t[]; }
    void addRotation(in float[4] r) { _rotations ~= r; }
    void setFacing(float f) { _facing = f; }
    void beginDraw(float x, float y, float z)
    {
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glTranslatef(x+_pre_translation[0],y+_pre_translation[1],z+_pre_translation[2]);
        foreach(float[4] rots; _rotations)
        {
            glRotatef(rots[0],rots[1],rots[2],rots[3]);
        }
        glRotatef(_facing,0,0,1);
        glTranslatef(_post_translation[0],_post_translation[1],_post_translation[2]);
        glScalef(1, 1, 1);
    }

    void endDraw()
    {
        glPopMatrix();
    }

    void setMesh(Mesh mesh)
    {
        _mesh = mesh;
    }

    void drawAt(float x, float y)
    {
        beginDraw(x,y,0.0f);
        scope(exit)
            endDraw();
        _mesh.renderFrame();
    }

    void update(float deltatime)
    {
    }

    Model clone()
    {
        return this;
    }

    bool isAnimated() { return false; }
    final bool isStatic() { return !isAnimated(); }

    AnimatedModel asAnimated() {
        assert( isAnimated() );
        return cast(AnimatedModel)this;
    }

    mixin BindClass!("C/MODEL");
    mixin BindMethods!(getName,isAnimated,setScale,getScale);
}


static Model loadFromSofu(string name,sofu.Map map)
{
    float[3]   pre_translation  = [0, 0, 0];
    float[3]   post_translation = [0, 0, 0];

    AnimatedModel m = new AnimatedModel;
    m.setName(name);
    m.setMesh( loadMesh(map.value("mesh").toString()) );
    m.setAnimation(Md2Action.MD2_ATTACK);

    if( map.hasAttribute("pre_trans") )
        for(int i=0; i != 3; ++i)
            pre_translation[i] = map.list("pre_trans").value(i).toFloat();

    if( map.hasAttribute("post_trans") )
        for(int i=0; i != 3; ++i)
            post_translation[i] = map.list("post_trans").value(i).toFloat();

    foreach(sofu.SofuObject rlist; map.list("rotations"))
    {
        float[4] rots;
        for(int i=0; i != 4; ++i)
            rots[i] = rlist.asList().value(i).toFloat();
        m.addRotation( rots );
    }
    m.setScale( map.value("scale").toFloat() );
    m.setPreTranslation( pre_translation );
    m.setPostTranslation( post_translation );

    m.loadSkin( map.value("skin").toString() );
    return m;
}

class AnimatedModel : Model
{
    public:
        AnimatedMesh animated_mesh;
        AnimationState animationState;
        Texture texture;

    Model clone()
    {
        AnimatedModel m = new AnimatedModel();
        m.setName (getName());
        m.setMesh (animated_mesh);
        m.texture = texture;
        m._scale = _scale;
        m._pre_translation[]  = _pre_translation[];
        m._post_translation[] = _post_translation[];
        m._rotations[] = _rotations[];
        m._facing = _facing;
        return m;
    }

    void setMesh(Mesh mesh)
    {
        _mesh = mesh;
        animated_mesh = cast(AnimatedMesh)mesh;
    }

    void setAnimation( int type )
    {
        if( (type < 0) || (type > Md2Action.MD2_CREATURE_ACTION_COUNT ) )
            type = 0;
        animationState.startframe   = md2.animations[ type ].first_frame;
        animationState.endframe     = md2.animations[ type ].last_frame;
        animationState.next_frame   = md2.animations[ type ].first_frame + 1;
        animationState.fps          = md2.animations[ type ].fps;
        animationState.type         = type;
    }

    void loadSkin( char[] filename )
    {
        texture = Engine.instance.textureManager.load(filename);
    }

    void drawAt(float x, float y)
    {
        animated_mesh.texture = texture;

        animated_mesh.setCurrentFrame( animationState.curr_frame );
        animated_mesh.setNextFrame( animationState.next_frame );
        animated_mesh.setInterpol( animationState.interpol );

        animated_mesh.setScale( _scale );
//         animated_mesh.setFacing( facing );

        beginDraw(x,y,0.0f);
        scope(exit)
            endDraw();
        animated_mesh.renderFrame();
    }

    void update ( float deltatime )
    {
        animationState.curr_time += deltatime;
        // calculate current and next frames
        if( animationState.curr_time - animationState.old_time > (1.0 / animationState.fps) )
        {
            animationState.curr_frame = animationState.next_frame;
            animationState.next_frame++;
            if( animationState.next_frame > animationState.endframe )
                animationState.next_frame = animationState.startframe;
            animationState.old_time = animationState.curr_time;
        }
        // prevent having a current/next frame greater
        // than the total number of frames...
        if( animationState.curr_frame > (animated_mesh.numFrames - 1) )
            animationState.curr_frame = 0;

        if( animationState.next_frame > (animated_mesh.numFrames - 1) )
            animationState.next_frame = 0;

        animationState.interpol = animationState.fps * (animationState.curr_time - animationState.old_time);
    }

    bool isAnimated() { return true; }
}


private AnimatedMesh[char[]] _meshes;

AnimatedMesh loadMesh(char[] filename)
{
    if( filename in _meshes)
        return _meshes[filename];
    _meshes[filename] = new Md2AnimatedMesh(filename);
    return _meshes[filename];
}