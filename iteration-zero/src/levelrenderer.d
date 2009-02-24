module levelrenderer;

private {
    import std.stdio;
    import std.utf;
    import std.math : sqrt;
    import material;
    import sofu = Sofu.Sofu;

    import engine;
    import derelict.sdl.sdl;
    import derelict.opengl.gl;
    import derelict.opengl.glu;

    import pipeline;
    import vector;
    import util;

    import interfaces;
    import dataset;
    import gobject;
    import party;
	 import level;
    import dlisp.dlisp;
    import dlisp.bind;
    import dlisp.predefs.all;
}


class LevelRenderer : Renderer
{
    private:
        matrix4 projMatrix;
        matrix4 viewMatrix;

        GObject _trackedObject;

    public:
        Level level;

        float zoom = 5.0;

        float look_x = 0.0;
        float look_y = 0.0;

        this()
        {
            super("level");
        }

        void sceneMode()
        {
            glMatrixMode(GL_PROJECTION);
            glLoadIdentity();
            glOrtho(-40.0f, 40.0f, -30.0f, 30.0f, -200.0f, 200.0f);

            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            glRotatef(-45.0f, 1.0f, 0.0f, 0.0f);
            glRotatef(45.0f, 0.0f, 0.0f, 1.0f);
            glScalef(zoom, zoom, zoom);
            glEnable(GL_TEXTURE_2D);

            glTranslatef(-look_x,-look_y,0);

            glGetFloatv(GL_PROJECTION_MATRIX,&projMatrix.a00);
            glGetFloatv(GL_MODELVIEW_MATRIX,&viewMatrix.a00);
            invert(&projMatrix);
            invert(&viewMatrix);
        }

        void trackObject(GObject gobject)
        {
            _trackedObject = gobject;
        }

        void lookAt(float x,float y)
        {
            look_x = x;
            look_y = y;
        }

        void toLevelCoords(int* x, int* y)
        {
            vector4 screenCoords;
            long w_ = Engine.instance.xResolution;
            long h_ = Engine.instance.yResolution;
            float fx = 2*(cast(float)*x/w_ - 0.5);
            float fy = 4*(.5-cast(float)*y/h_);
            set(&screenCoords,fx,fy,0.0f,1.0);
            mul(&screenCoords,&projMatrix);
            mul(&screenCoords,&viewMatrix);
            *x = cast(int)screenCoords.x;
            *y = cast(int)screenCoords.y;
        }

        void drawHighlight(int x, int y)
        {
            float z_offset = 0.01;
            glColor4f(1,0,0,1);
            glBegin(GL_LINE_LOOP);
                glVertex3f( x+1, y+1, z_offset );       /* NE */
                glVertex3f( x, y+1, z_offset );       /* NW */
                glVertex3f( x, y, z_offset );       /* SW */
                glVertex3f( x+1, y, z_offset );
            glEnd();

            glBegin(GL_LINE_LOOP);
                glVertex3f( x+1, y+1, z_offset+1 );       /* NE */
                glVertex3f( x, y+1, z_offset+1 );       /* NW */
                glVertex3f( x, y, z_offset+1 );       /* SW */
                glVertex3f( x+1, y, z_offset+1 );
            glEnd();

            glColor4f(0,1,0,1);
            glBegin(GL_LINES);
                glVertex3f( x, y, z_offset );       /* NE */
                glVertex3f( x, y, z_offset+1 );       /* NW */
                glVertex3f( x+1, y, z_offset );       /* SW */
                glVertex3f( x+1, y, z_offset+1 );
                glVertex3f( x, y+1, z_offset );       /* NE */
                glVertex3f( x, y+1, z_offset+1 );       /* NW */
                glVertex3f( x+1, y+1, z_offset );       /* SW */
                glVertex3f( x+1, y+1, z_offset+1 );
            glEnd();
        }

        void render()
        {
            if( _trackedObject !is null )
            {
                float dx = look_x - _trackedObject.real_x;
                float dy = look_y - _trackedObject.real_y;
                float len = sqrt(dx*dx+dy*dy);
                if( len > 1.0 )
                {
                    lookAt( look_x - dx/len, look_y - dy/len );
                }
                if( len <= 1.0 )
                {
                    lookAt( look_x - dx, look_y - dy );
                }
            }
            sceneMode();
            if( level is null )
                return;

            for(int x = 0; x < level.width; ++x)
                for(int y = 0; y < level.height; ++y)
                    level.getTile(x,y).draw();

            foreach(GObject gobject; level.getAllObjects())
            {
                gobject.draw();
            }
        }
}
