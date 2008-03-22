module gobject;

private {
    import derelict.sdl.sdl;
    import std.math;
    import std.stdio;

    import interfaces;
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
            if( dx != 0 || dy != 0 && gobject.model !is null)
              gobject.model.setFacing( atan2(cast(float)dy,cast(float)dx)*180.0/3.1415 );
        }

}

class GObject : GObjectBase, IGObject
{
    private:
        Model _model;
        MotionState _motionState;
        ILevel _level;
        Path _path;

        float _time;

        GObject[] _inventory;

    public:
        float real_x, real_y;
        int x = 0;
        int y = 0;

        float speed = 1;
        bool blocking = false;

        this()
        {
            _motionState = new MotionState (this);
            _time = SDL_GetTicks()*0.001;
        }

        void setLevel(ILevel level)
        {
            _level = level;
            _path = new Path (_level, x, y, x, y);
            if( _model !is null )
                _model.asAnimated().setAnimation(Md2Action.MD2_STAND);
        }

        void setModel(Model model)
        {
            _model = model;
            if( _model !is null )
                _model.asAnimated().setAnimation(Md2Action.MD2_STAND);
        }

        void setPosition(int x_,int y_)
        {
            x = x_;
            y = y_;
            real_x = x_;
            real_y = y_;
            if( _level )
                _path = new Path (_level, x, y, x, y);
        }

        void update()
        {
            float oldtime = _time;
            _time = SDL_GetTicks()*0.001;
            _motionState._updateMovement(_time);
            if( !_motionState.isMoving && _path !is null)
            {
                if( _path.step )
                {
                    _motionState._startMovingTo(x + _path.dx, y + _path.dy, _time,1.0/speed);
                } else
                {
                    if( _model !is null)
                        _model.asAnimated().setAnimation(Md2Action.MD2_STAND);
                }
            }
            if( _model !is null)
                _model.update (_time - oldtime);
        }

        void draw()
        {
            update();
            if( _model !is null)
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
                _model.asAnimated().setAnimation(Md2Action.MD2_RUN);
            }
        }

        void setFacing(float f)
        {
            if( _model !is null)
                _model.setFacing(f);
        }

        int getX() { return x; }
        int getY() { return y; }
        bool isItem() {  return false; }
        GObject[] getInventory() { return _inventory; }
        void addToInventory(GObject gobject)  {  _inventory ~= gobject;   }

        mixin BindClass!("C/OBJECT");
        mixin BindMethods!(isBusy,startMovingTo,setFacing,getX,getY);
        mixin BindMethods!(getInventory,addToInventory,setModel);
}

