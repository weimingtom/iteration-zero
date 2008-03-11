/**
 * Credit for this code is mainly due to:
 * DigiBen     digiben@gametutorials.com
 * Look up his other great tutorials at:
 * http://www.gametutorials.com
 *
 * glCommands (and thus simplification of this file) is implemented
 * thanks to David Henry tutorial :
 *   http://tfc.duke.free.fr/us/tutorials/models/md2.htm
 */

/**
  *@author Gabor Torok
  *@author KB (2008)
  */

import std.stream;
import std.stdio;
import std.utf;

import derelict.opengl.gl;
import derelict.sdl.sdl;

import vector;
import util;

import texture;
import engine;

import modelmesh;
import md2normals;

/////////////////////////////////////////////////////////////////////////////////
//
// Ben Humphrey (DigiBen)
// Game Programmer
// DigiBen@GameTutorials.com
// Co-Web Host of www.GameTutorials.com
//
// The Quake2 .Md2 file format is owned by ID Software.  This tutorial is being used
// as a teaching tool to help understand model loading and animation.  This should
// not be sold or used under any way for commercial use with out written conset
// from ID Software.
//
// Quake and Quake2 are trademarks of id Software.
// All trademarks used are properties of their respective owners.
//
//


// These are the needed defines for the max values when loading .MD2 files

const int MD2_MAX_TRIANGLES      = 4096;
const int MD2_MAX_VERTICES       = 2048;
const int MD2_MAX_TEXCOORDS      = 2048;
const int MD2_MAX_FRAMES         = 512;
const int MD2_MAX_SKINS          = 32;
const int MD2_MAX_FRAMESIZE      = (MD2_MAX_VERTICES * 4 + 128);


/**
 * Credit for this code is mainly due to:
 * DigiBen     digiben@gametutorials.com
 * Look up his other great tutorials at:
 * http://www.gametutorials.com
 *
 * glCommands (and thus simplification of this file) is implemented
 * thanks to David Henry tutorial :
 *   http://tfc.duke.free.fr/us/tutorials/models/md2.htm
 */

// Md2 header information
struct Md2Header
{
   int magic;                   // This is used to identify the file
   int version_;                // The version number of the file (Must be 8)
   int skinWidth;               // The skin width in pixels
   int skinHeight;              // The skin height in pixels
   int frameSize;               // The size in bytes of a frame (constant for each)
   int numSkins;                // The number of skins associated with the model
   int numVertices;             // The number of vertices (constant for each frame)
   int numTexCoords;            // The number of texture coordinates
   int numTriangles;            // The number of faces (polygons)
   int numGlCommands;           // The number of gl commands
   int numFrames;               // The number of animation frames
   int offsetSkins;             // The offset in the file for the skin data
   int offsetTexCoords;         // The offset in the file for the texture data
   int offsetTriangles;         // The offset in the file for the face data
   int offsetFrames;            // The offset in the file for the frames data
   int offsetGlCommands;        // The offset in the file for the gl commands data
   int offsetEnd;               // The end of the file offset
}


// This is used to store the vertices that are read in for the current frame
struct Md2AliasTriangle
{
   ubyte vertex[3];
   ubyte lightNormalIndex;
}

// This stores the animation scale, translation and name information for a frame, plus verts
struct Md2AliasFrame
{
   float[3] scale;
   float[3] translate;
   char[16] name;
   Md2AliasTriangle[] aliasVertices;
}


// This stores a skin or a frame name
alias char[64] Md2String;

// different actions possible in a md2 file
enum Md2Action
{
    MD2_STAND,
    MD2_RUN,
    MD2_ATTACK,
    MD2_PAIN1,
    MD2_PAIN2,
    MD2_PAIN3,
    MD2_JUMP,
    MD2_FLIP,
    MD2_SALUTE,
    MD2_TAUNT,
    MD2_WAVE,
    MD2_POINT,
    MD2_CRSTAND,
    MD2_CRWALK,
    MD2_CRATTACK,
    MD2_CRPAIN,
    MD2_CRDEATH,
    MD2_DEATH1,
    MD2_DEATH2,
    MD2_DEATH3,

    // Must be last
    MD2_CREATURE_ACTION_COUNT
}

struct Animation
{
    int     first_frame;            // first frame of the animation
    int     last_frame;             // number of frames
    int     fps;                    // number of frames per second
}

class Md2ModelMesh : ModelMesh
{
public:
    // constructor/destructor
    this(char[] filename)
    {
        super(filename);
        loadMd2Model(filename);
    }

    ~this()
    {
    }

private:
    int             numVertices;            // number of vertices
    int             numGlCommands;         // number of opengl commands

    vector3[]       vertices;        // vertex array
    int[]           glCommands;          // opengl command array
    int[]           lightNormals;    // normal index array

    vector3 min_vertex;
    vector3 max_vertex;

public:

    // functions
    void loadMd2Model(char[] filename)
    {
        Md2Header header;
        std.stream.File f = new std.stream.File(filename);
        f.read(header.magic);
        f.read(header.version_);
        assert(header.magic == 844121161 && header.version_ == 8);
        writefln ("Loading Md2 Model: '%s'", filename);
    
        f.read(header.skinWidth);               // The skin width in pixels
        f.read(header.skinHeight);              // The skin height in pixels
        f.read(header.frameSize);               // The size in bytes of a frame (header.constant for each)
        f.read(header.numSkins);                // The number of skins associated with the model
        f.read(header.numVertices);             // The number of vertices (header.constant for each frame)
        f.read(header.numTexCoords);            // The number of texture coordinates
        f.read(header.numTriangles);            // The number of faces (header.polygons)
        f.read(header.numGlCommands);           // The number of gl commands
        f.read(header.numFrames);               // The number of animation frames
        f.read(header.offsetSkins);             // The offset in the file for the skin data
        f.read(header.offsetTexCoords);         // The offset in the file for the texture data
        f.read(header.offsetTriangles);         // The offset in the file for the face data
        f.read(header.offsetFrames);            // The offset in the file for the frames data
        f.read(header.offsetGlCommands);        // The offset in the file for the gl commands data
        f.read(header.offsetEnd);

        numVertices   = header.numVertices;
        numFrames     = header.numFrames;
        numGlCommands = header.numGlCommands;

        vertices.length = numVertices * numFrames;
        lightNormals.length = numVertices * numFrames;
        glCommands.length = numGlCommands;

        // Read Skin Names
        f.seekSet( header.offsetSkins );
        for(int i = 0; i != header.numSkins; ++i)
        {
            Md2String skinName;
            skinName[] = 0;
            f.readBlock(cast(void*)&skinName,64);
//             writefln ("-- Skin #%d %s",i,skinName ~ "\0");
        }

        // Read GL Commands
        f.seekSet( header.offsetGlCommands );
        for(int i = 0; i != header.numGlCommands; ++i)
        {
            f.read(glCommands[i]);
        }

        // Read Frame + Vertex Data
        f.seekSet( header.offsetFrames );
        Md2AliasFrame[] frames;
        frames.length = numFrames;
        for(int i = 0; i != header.numFrames; ++i)
        {
            for(int j=0; j!=3; ++j)
                f.read(frames[i].scale[j]);
            for(int j=0; j!=3; ++j)
                f.read(frames[i].translate[j]);
            f.readBlock(cast(void*)&frames[i].name,16);
//             try { writefln ("-- Frame #%d %s",i,toUTF8(frames[i].name)); } catch( Exception ){}
            frames[i].aliasVertices.length = numVertices;
            for(int j=0; j!=header.numVertices; ++j)
            {
                for(int k=0; k!=3; ++k)
                    f.read(frames[i].aliasVertices[j].vertex[k]);
                f.read(frames[i].aliasVertices[j].lightNormalIndex);
            }
       }

        // Unpack Frame Data
        for( int j = 0; j < numFrames; j++ )
        {
            for( int i = 0; i < numVertices; i++ )
            {
                int idx = i + numVertices * j;
                vertices[idx].e[0] = (frames[j].aliasVertices[i].vertex[0] * frames[j].scale[0]) + frames[j].translate[0];
                vertices[idx].e[1] = (frames[j].aliasVertices[i].vertex[1] * frames[j].scale[1]) + frames[j].translate[1];
                vertices[idx].e[2] = (frames[j].aliasVertices[i].vertex[2] * frames[j].scale[2]) + frames[j].translate[2];
                lightNormals[idx]  = frames[j].aliasVertices[i].lightNormalIndex;
    
                min_vertex.x = min( min_vertex.x, vertices[idx].x);
                min_vertex.y = min( min_vertex.y, vertices[idx].y);
                min_vertex.z = min( min_vertex.z, vertices[idx].z);
    
                max_vertex.x = max( max_vertex.x, vertices[idx].x);
                max_vertex.y = max( max_vertex.y, vertices[idx].y);
                max_vertex.z = max( max_vertex.z, vertices[idx].z);
            }
        }
        for( int j = 0; j < numFrames; j++ )
        {
            for( int i = 0; i < numVertices; i++ )
            {
                int idx = i + numVertices * j;
//                 vertices[idx].x += -min_vertex.x;
//                 vertices[idx].y += -min_vertex.y;
//                 vertices[idx].z += -min_vertex.z;
            }
        }
        writefln ("Bounding Box: (%f %f) (%f %f) (%f %f)",min_vertex.x,max_vertex.x,min_vertex.y,max_vertex.y,min_vertex.z,max_vertex.z);
    }

    void renderFrame()
    {
        static vector3    vertexList[ MD2_MAX_VERTICES ];  // interpolated vertices
        int* ptricmds = &glCommands[0];
        // reverse the orientation of front-facing
        // polygons because gl command list's triangles
        // have clockwise winding
        glPushAttrib( GL_POLYGON_BIT );
        glFrontFace( GL_CW );
    
        // enable backface culling
        glEnable( GL_CULL_FACE );
        glCullFace( GL_BACK );
        glColor4f( 1,1,1,.5 );

    
        // interpolate
        interpolate( &vertexList[0] );
    
        // bind model's texture
        if( texture !is null)
        {
            texture.register();
            texture.bind();
        }

        // draw each triangle!
        int i = 0;
        int cmd = 0;
        while(true)
        {
            cmd = glCommands[i++];
            if( cmd == 0 )
                break;
            // Get GL Triangle Mode
            if( cmd < 0 )
            {
                glBegin( GL_TRIANGLE_FAN );
                cmd = -cmd;
            } else
            {
                glBegin( GL_TRIANGLE_STRIP );
            }

            for( /* nothing */; cmd > 0; cmd--, i += 3 )
            {
                // ptricmds[0] : texture coordinate s
                // ptricmds[1] : texture coordinate t
                // ptricmds[2] : vertex index to render
                // parse texture coordinates
                glTexCoord2f( *cast(float*)&glCommands[i], *cast(float*)&glCommands[i+1] );

                // parse triangle's normal (for the lighting)
                int idx = glCommands[i+2];
                int normal_idx = lightNormals[ idx ];
//                 writefln ("%d %f %f %f",idx,vertexList[ idx ].x,vertexList[ idx ].y,vertexList[ idx ].z);

                glNormal3fv( &anorms[normal_idx ].x );
                glVertex3fv( &vertexList[ idx ].x );
            }
            glEnd();
        }

        // Restore
        glDisable( GL_CULL_FACE );
        glPopAttrib();
    }
private:

    void interpolate( vector3 *vertlist )
    {
        vector3* curr_v = &vertices[ numVertices * animationState.curr_frame ];
        vector3* next_v = &vertices[ numVertices * animationState.next_frame ];

        float interpol = animationState.interpol;
//         writefln ("%d %f %f",animationState.curr_frame,interpol,scale);

        for( int i = 0; i < numVertices ; i++ )
        {
//             writefln ("%d %f %f",animationState.curr_frame,interpol,scale);
            vertlist[i].x = (curr_v[i].x + interpol*(next_v[i].x - curr_v[i].x)) * scale;
            vertlist[i].y = (curr_v[i].y + interpol*(next_v[i].y - curr_v[i].y)) * scale;
            vertlist[i].z = (curr_v[i].z + interpol*(next_v[i].z - curr_v[i].z)) * scale;
        }
    }

}


// member variables
Animation   animations[21] = [
    {   0,  39,  9 },   // STAND
    {  40,  45, 10 },   // RUN
    {  46,  53, 10 },   // ATTACK
    {  54,  57,  7 },   // PAIN_A
    {  58,  61,  7 },   // PAIN_B
    {  62,  65,  7 },   // PAIN_C
    {  66,  71,  7 },   // JUMP
    {  72,  83,  7 },   // FLIP
    {  84,  94,  7 },   // SALUTE
    {  95, 111, 10 },   // FALLBACK
    { 112, 122,  7 },   // WAVE
    { 123, 134,  6 },   // POINT
    { 135, 153, 10 },   // CROUCH_STAND
    { 154, 159,  7 },   // CROUCH_WALK
    { 160, 168, 10 },   // CROUCH_ATTACK
    { 196, 172,  7 },   // CROUCH_PAIN
    { 173, 177,  5 },   // CROUCH_DEATH
    { 178, 183,  7 },   // DEATH_FALLBACK
    { 184, 189,  7 },   // DEATH_FALLFORWARD
    { 190, 197,  7 },   // DEATH_FALLBACKSLOW
    { 198, 198,  5 },   // BOOM
];

//static float    anorms_dots[ SHADEDOT_QUANT ][256];




