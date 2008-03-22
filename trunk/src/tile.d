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
    public:
        this(StaticMesh mesh, int x_, int y_)
        {
            _x = x_;
            _y = y_;
            _mesh = mesh;
        }

        void draw()
        {
            _mesh.beginDraw(_x,_y,0.0);
            _mesh.renderFrame();
            _mesh.endDraw();
        }

        int x() { return _x; }
        int y() { return _y; }

        bool blocking()
        {
//             if( _prototype.blocking )
//                 return true;

            foreach(GObject gobject; _gobjects)
            {
                if( gobject.blocking )
                    return true;
            }
            return false;
        }

        GObject[] getObjects() { return _gobjects; }

        mixin BindClass!("C/TILE");
}

