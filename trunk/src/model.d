module model;

import std.stdio;
import derelict.opengl.gl;
import sofu = Sofu.Sofu;

import engine;
import texture;
import modelmesh;
import md2;


class Model
{
    public:
        AnimatedMesh mesh;
        AnimationState animationState;
        Texture texture;

        float[3]   pre_translation  = [0, 0, 0];
        float[3]   post_translation = [0, 0, 0];
        float[4][] rotations;
        float scale = 1.0;
        float facing = 0.0;

    this(sofu.Map map)
    {
        mesh = loadModel(map.value("mesh").toString());
        setAnimation(Md2Action.MD2_ATTACK);

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
            rotations ~= rots;
            mesh.addRotation( rots );
        }
        scale = map.value("scale").toFloat();
        mesh.setPreTranslation( pre_translation );
        mesh.setPostTranslation( post_translation );

        loadSkin( map.value("skin").toString() );
    }

    this(Model m)
    {
        mesh = m.mesh;
        animationState = m.animationState;
        texture = m.texture;
        rotations.length = m.rotations.length;

        pre_translation[]  = m.pre_translation;
        post_translation[] = m.post_translation;
        rotations[] = m.rotations;
        scale = m.scale;
    }

    ~this()
    {
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
        mesh.texture = texture;

        mesh.setCurrentFrame( animationState.curr_frame );
        mesh.setNextFrame( animationState.next_frame );
        mesh.setInterpol( animationState.interpol );

        mesh.setScale( scale );
        mesh.setFacing( facing );

        mesh.beginDraw(x,y,0.0f);
        scope(exit)
            mesh.endDraw();

        mesh.renderFrame();
    }

    void animate( float deltatime )
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
        if( animationState.curr_frame > (mesh.numFrames - 1) )
            animationState.curr_frame = 0;

        if( animationState.next_frame > (mesh.numFrames - 1) )
            animationState.next_frame = 0;

        animationState.interpol = animationState.fps * (animationState.curr_time - animationState.old_time);
    }
}


private AnimatedMesh[char[]] _meshes;

AnimatedMesh loadModel(char[] filename)
{
    if( filename in _meshes)
        return _meshes[filename];
    _meshes[filename] = new Md2AnimatedMesh(filename);
    return _meshes[filename];
}