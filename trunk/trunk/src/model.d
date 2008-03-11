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
        ModelMesh mesh;
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
        }
        scale = map.value("scale").toFloat();
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
        mesh.animationState = animationState;
        mesh.texture = texture;
        mesh.scale = scale;

//           writefln ("Rendering... %f", mesh.animationState.interpol);
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
            // rotate the model
            glTranslatef(x+pre_translation[0],y+pre_translation[1],pre_translation[2]);
            foreach(float[4] rots; rotations)
            {
                glRotatef(rots[0],rots[1],rots[2],rots[3]);
            }
            glRotatef(facing,0,0,1);
            glTranslatef(post_translation[0],post_translation[1],post_translation[2]);
            glScalef(1, 1, 1);

            // render it on the screen
            mesh.renderFrame();
        glPopMatrix();
    }

    void animate( float time )
    {
        animationState.curr_time = time;
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


private ModelMesh[char[]] _meshes;

ModelMesh loadModel(char[] filename)
{
    if( filename in _meshes)
        return _meshes[filename];
    _meshes[filename] = new Md2ModelMesh(filename);
    return _meshes[filename];
}