module modelmesh;

import texture;
import material;
import derelict.opengl.gl;
import std.stdio;

import sofu = Sofu.Sofu;

// animation state
struct AnimationState
{
    int     startframe;              // first frame
    int     endframe;                // last frame
    int     fps;                     // frame per second for this animation

    float   curr_time = 0.0;         // current time
    float   old_time = 0.0;          // old time
    float   interpol = 0.0;          // percent of interpolation

    int     type;                    // animation type

    int     curr_frame;              // current frame
    int     next_frame;              // next frame

}


class Mesh
{
    private:
        char[] _filename;
        float  _scale = .05;

    public:

    this(char[] filename_)
    {
        _filename = filename_;
    }

    abstract void renderFrame();

    float getScale() { return _scale; }
    void setScale(float scale) { _scale = scale; }

}

struct Face
{
    public:
        float[3][] _vertices;
        float[2][] _texcoords;
        float[3][] _normals;

    static Face opCall()
    {
        Face face;
        return face;
    }

    static Face opCall(sofu.List list)
    {
        Face face;
        foreach(sofu.SofuObject f; list)
        {
            face._vertices  ~= [ f.asList().value(0).toFloat(),f.asList().value(1).toFloat(),f.asList().value(2).toFloat() ];
            face._texcoords ~= [ f.asList().value(3).toFloat(),f.asList().value(4).toFloat() ];
            face._normals   ~= [ f.asList().value(5).toFloat(),f.asList().value(6).toFloat(),f.asList().value(7).toFloat() ];
        }
        return face;
    }

    void draw()
    {
        for(int i=0; i!= _vertices.length; ++i)
        {
            glTexCoord2f(_texcoords[i][0],_texcoords[i][1]);
            glNormal3f(_normals[i][0],_normals[i][1],_normals[i][2]);
            glVertex3f(_vertices[i][0],_vertices[i][1],_vertices[i][2]);
        }
    }
}

class StaticMesh : Mesh
{
    private:
        string[] _material_names;
        Face[] _faces;

    public:

    this(string filename)
    {
        super(filename);
    }

    void addFace(string material, Face face)
    {
        _material_names ~= material;
        _faces ~= face;
    }

    void renderFrame()
    {
        glEnable(GL_TEXTURE_2D);
        for(int i=0; i!=_faces.length; ++i)
        {
            getMaterial(_material_names[i]).register();
            getMaterial(_material_names[i]).draw();
//             glLineWidth(2);
//             glColor3f(0,1,0);
//             glBegin(GL_LINE_LOOP);
            glBegin(GL_QUADS);
            _faces[i].draw();
            glEnd();
        }
    }
}


class AnimatedMesh : Mesh
{
    private:
        int _currentFrame;
        int _nextFrame;
        float _interpol;

    public:
        int    numFrames;         // number of frames
        Texture texture;

    this(string filename_)
    {
        super(filename_);
    }

    void setCurrentFrame(int f) { _currentFrame = f; }
    void setNextFrame(int f) { _nextFrame = f; }
    void setInterpol(float f) { _interpol = f; }

    int getCurrentFrame() { return _currentFrame; }
    int getNextFrame() { return _nextFrame; }
    float getInterpol() { return _interpol; }

}
