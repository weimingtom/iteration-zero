module turn;

import std.stdio;

import party;
import level;
import character;
import gobject;
import actor;



class TurnManager
{
private:
    Party _party;
    Level _level;

    Actor[] _actors;

    Actor _current;
public:

    this(Party party, Level level)
    {
        _party = party;
        _level = level;
    }

    Actor getCurrent()
    {
        return _current;
    }

    void logic()
    {
        if( _current is null )
        {
            if( findNextActor() is null )
                return;
            _current.doTurn( &waitCurrentActor, &finishedCurrentActor );
        } else
            _current.logic();
    }

    void waitCurrentActor()
    {
        if( _current !is null )
            _actors ~= _current;
        _current = null;
    }

    void finishedCurrentActor()
    {
        writefln("Finished turn: ",_current);
        _current = null;
    }

    Actor findNextActor()
    in { assert( _level !is null && _party !is null ); }
    body
    {
        if( _current !is null )
            return _current;

        if( _actors.length == 0 )
        {
            foreach(Character c; _party)
                _actors ~= c;
            foreach(GObject o; _level.getAllObjects())
            {
                if( o.isActor() )
                    _actors ~= cast(Actor)o;
            }
            foreach(Actor actor; _actors)
                actor.newTurn();
        }
        _current = null;
        if( _actors.length == 0 )
            return null;
        _current = _actors[0];
        _actors = _actors[1 .. $];
        return _current;
    }

}