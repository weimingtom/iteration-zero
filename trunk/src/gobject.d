module gobject;

private {
    import derelict.sdl.sdl;
    import std.math;
    import std.stdio;

    import baseClasses;
    import path;
    import model;
    import md2;
    import sofu = Sofu.Sofu;

    import dlisp.bind;
}

class MotionState
{
    public:
        GObject gobject;

        int dx,dy;
        bool isMoving = false;
        float interpol = 0.0;
        float started, eta;

        this(GObject gobject_)
        {
            gobject = gobject_;
        }

        void _startMovingTo(int tx, int ty, float _time, float time)
        {
            assert( isMoving == false );

            isMoving = true;
            dx = tx - gobject.x;
            dy = ty - gobject.y;
            eta = _time + time;
            started = _time;
        }

        void _updateMovement(float _time)
        {
            if( isMoving )
            {
                if( _time > eta )
                {
                    interpol = 0.0;
                    gobject.x += dx;
                    gobject.y += dy;
                    isMoving = false;

                    gobject.arrivedAt.emit (gobject, gobject.x, gobject.y);
                } else
                {
                    interpol = (_time - started)/(eta - started);
                }
            }
            gobject.real_x = gobject.x + 0.5 + interpol*dx;
            gobject.real_y = gobject.y + 0.5 + interpol*dy;
            if( dx != 0 || dy != 0 )
              gobject.model.facing = atan2(cast(float)dy,cast(float)dx)*180.0/3.1415;
        }

}

class GObject : GObjectBase, IGObject
{
    private:
        GObjectPrototype _prototype;
        Model _model;
        MotionState _motionState;
        ILevel _level;
        Path _path;

        float _time;

        GObject[] _inventory;

    public:
        float real_x, real_y;
        int x,y;

        float speed;
        bool blocking;


        this(ILevel level, GObjectPrototype prototype, int x_, int y_)
        {
            _level = level;
            _prototype = prototype;
            _model = new Model (_prototype.model);
            _motionState = new MotionState (this);

            blocking = _prototype.blocking;
            speed = _prototype.speed;

            x = x_;
            y = y_;

            _path = new Path (_level, x, y, x, y);
            _time = SDL_GetTicks()*0.001;
            _model.setAnimation(Md2Action.MD2_STAND);
        }

        void update()
        {
            _time = SDL_GetTicks()*0.001;
            _motionState._updateMovement(_time);
            if( !_motionState.isMoving )
            {
                if( _path.step )
                {
                    _motionState._startMovingTo(x + _path.dx, y + _path.dy, _time,1.0/speed);
                } else
                {
                    _model.setAnimation(Md2Action.MD2_STAND);
                }
            }
            _model.animate(_time);
        }

        void draw()
        {
            update();
            _model.drawAt (real_x, real_y);
        }

        bool isBusy()
        {
            return _motionState.isMoving;
        }

        Model model() { return _model; }

        void startMovingTo(int tx, int ty)
        {
            _path = new Path (_level, x, y, tx, ty);
            if( _path.step )
            {
                _motionState._startMovingTo(x + _path.dx, y + _path.dy, _time,1.0/speed);
                _model.setAnimation(Md2Action.MD2_RUN);
            }
        }

        void setFacing(float f)
        {
          _model.facing = f;
        }

        int getX() { return x; }
        int getY() { return y; }

        mixin BindClass!("C/GOBJECT");
        mixin BindMethods!(isBusy,startMovingTo,setFacing,getX,getY);
}

class GObjectPrototype
{
    public:
        mixin BindClass!("C/GOBJECT-PROTOTYPE");
  
        char[] name;
        Model model;

        bool selectable = false;
        bool active     = false;
        bool blocking   = true;

        float speed = 1.0;

        this(char[] name_, sofu.Map map)
        {
            name = name_;
            model = new Model(map.map("model"));

            if( map.hasAttribute("speed") )
                speed = map.value("speed").toFloat();

            if( map.hasAttribute("blocking") )
                blocking = map.value("blocking").toInt() == 1;
        }

        GObject create(ILevel level, int x, int y)
        {
            return new GObject (level, this, x, y);
        }
}