module modelmesh;

import texture;

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

class ModelMesh
{
    public:
        char[] filename;
        int    numFrames;         // number of frames

        AnimationState animationState;
        Texture texture;
        float   scale = .05;       // scale value


        this(char[] filename_)
        {
            filename = filename_;
        }

        void renderFrame() { assert(0); }
}
