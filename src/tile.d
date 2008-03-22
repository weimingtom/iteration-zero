module tile;

import modelmesh;
import gobject;
import dlisp.bind;

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

            _mesh.beginDraw(_x,_y,0.0);
            _mesh.renderFrame();
            _mesh.endDraw();
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

