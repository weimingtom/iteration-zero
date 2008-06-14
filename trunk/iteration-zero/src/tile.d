module tile;

import modelmesh;
import gobject;
import dlisp.bind;
import derelict.opengl.gl;

class Tile
{
    private:
        StaticMesh _mesh;
        int _x, _y;

        GObject[] _gobjects;
        bool _blocking;

    public:
        this(StaticMesh mesh, int x_, int y_)
        {
            _x = x_;
            _y = y_;
            _mesh = mesh;
        }

        void draw()
        {
            if( _mesh is null )
                return;
            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();
            glTranslatef(x,y,0);
            _mesh.renderFrame();
            glPopMatrix();
        }

        int x() { return _x; }
        int y() { return _y; }

        bool isBlocking()
        {
            if( _blocking )
                return true;
            foreach(GObject gobject; _gobjects)
            {
                if( gobject.blocking )
                    return true;
            }
            return false;
        }

        void setBlocking(bool b) { _blocking = b; }

        GObject[] getObjects() { return _gobjects; }

        mixin BindClass!("C/TILE");
        mixin BindMethods!(isBlocking,setBlocking);
}

