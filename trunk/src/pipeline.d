module pipeline;

private {
    import derelict.opengl.gl;
    import derelict.opengl.glu;
}


class Renderer
{
    public:
        this(char[] _name)
        {
            name = _name;
        }

        void render()
        {
        }

        char[] name;
        bool enabled = true;
}

class GridRenderer : public Renderer
{
    public:
        this()
        {
            super("grid");
        }

        void sceneMode()
        {
            static float offset = 0.001;
            glMatrixMode(GL_PROJECTION);
            glLoadIdentity();
            glOrtho(-40.0f, 40.0f, -30.0f, 30.0f, -200.0f, 200.0f);

            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            glTranslatef(offset,0,0);
            offset+=0.1;
//             glRotatef(35.264f, 0.0f, 0.0f, 1.0f);
            glRotatef(45.0f, 0.0f, 0.0f, 1.0f);
            glRotatef(-45.0f, 0.0f, 1.0f, 0.0f);
            glScalef(1.0f, 1.0f, -1.0f);


        }

        void render()
        {
            float dx = 0.1;
            float dy = 0.1;
            float z_offset = 0;

            glPushMatrix();
            glDisable( GL_CULL_FACE );
            sceneMode();

            for(int x = -102; x != 102; ++x)
            {
                for(int y = -102; y != 102; ++y)
                {
                    glBegin( GL_POLYGON );
                    glColor3f( 0.2, 0.1, 0.2 );               /* purple */
                        glVertex3f( x+1-dx, y+1-dy, z_offset );       /* NE */
                        glVertex3f( x+dx, y+1-dy, z_offset );       /* NW */
                        glVertex3f( x+dx, y+dy, z_offset );       /* SW */
                        glVertex3f( x+1-dx, y+dy, z_offset );       /* SE */
                    glEnd();
                }
            }

            for(int x = -12; x < 12; x+=2)
            {
                for(int y = -12; y < 12; y+=2)
                {
                    glBegin( GL_POLYGON );
                    glColor3f( 0.1, 0.1, 0.3 );               /* purple */
                        glVertex3f( x+1-dx, y+1-dy, z_offset+.5 );       /* NE */
                        glVertex3f( x+dx, y+1-dy, z_offset+.5 );       /* NW */
                        glVertex3f( x+dx, y+dy, z_offset );       /* SW */
                        glVertex3f( x+1-dx, y+dy, z_offset );       /* SE */
                    glEnd();
                }
            }

            int x = -12;
            for(int y = -12; y != 12; ++y)
            {
                glBegin( GL_POLYGON );
                glColor3f( 0.4, 0.3, 0.4 );               /* purple */
                    glVertex3f( x+dx, y+dy, z_offset );       /* NE */
                    glVertex3f( x-dx, y+dy, z_offset );       /* NW */
                    glVertex3f( x-dx, y-dy, z_offset );       /* SW */
                    glVertex3f( x+dx, y-dy, z_offset );       /* SE */
                glEnd();
            }

            int y = -12;
            for(x = -12; x != 12; ++x)
            {
                glBegin( GL_POLYGON );
                glColor3f( 0.4, 0.3, 0.4 );               /* purple */
                    glVertex3f( x+dx, y+dy, z_offset );       /* NE */
                    glVertex3f( x-dx, y+dy, z_offset );       /* NW */
                    glVertex3f( x-dx, y-dy, z_offset );       /* SW */
                    glVertex3f( x+dx, y-dy, z_offset );       /* SE */
                glEnd();
            }
            glPopMatrix();
        }
}

class RenderPipeline
{
    public:
        this()
        {
        }

        void render()
        {
            foreach(Renderer r; _pipeline)
            {
                if( r.enabled )
                    r.render();
            }
        }

        void add(Renderer r)
        {
            _pipeline ~= r;
            _byName[r.name] = r;
        }

        Renderer opIndex(char[] name)
        {
            return _byName[name];
        }

    private:
        Renderer[] _pipeline;
        Renderer[char[]] _byName;
}