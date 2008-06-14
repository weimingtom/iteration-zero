module path;

import interfaces;

private import std.stdio;

class Path
{
    public:
        ILevel level;
        int start_x;
        int start_y;
        int target_x;
        int target_y;
        int x;
        int y;

        int last_dx = 0;
        int last_dy = 0;

        int dx = 0;
        int dy = 0;

        this(ILevel level_, int sx_, int sy_, int tx_, int ty_)
        {
            level = level_;
            start_x = sx_;
            start_y = sy_;
            target_x = tx_;
            target_y = ty_;
            x = sx_;
            y = sy_;
        }

        bool finished()
        {
            return x == target_x && y == target_y;
        }

        bool step()
        {
//             writefln ("Path: (%d %d) -> (%d %d)", x, y, target_x, target_y);

            x += dx;
            y += dy;
            dx = dy = 0;

            if( finished )
                return false;

            if( target_x > x && !level.isBlocked (x+1, y) )
            {
                dx = 1;
                dy = 0;
            }
            if( target_x < x  && !level.isBlocked (x-1, y) )
            {
                dx = -1;
                dy = 0;
            }
            if( target_y > y  && !level.isBlocked (x, y+1) )
            {
                dx = 0;
                dy = 1;
            }
            if( target_y < y  && !level.isBlocked (x, y-1) )
            {
                dx = 0;
                dy = -1;
            }
            return dx != 0 || dy != 0;
        }
}