module actor;

import gobject;
import dlisp.bind;

class Actor : GObject
{
private:
    int _ap;
    int _max_ap;
    bool _does_turn;
    void delegate() _wait;
    void delegate() _skip;

public:
    this()
    {
        _max_ap = 10;
    }

    void newTurn ()
    {
        _ap = _max_ap;
    }

    void doTurn(void delegate() _wait_dg, void delegate() _skip_dg)
    {
        _does_turn = true;
        _wait = _wait_dg;
        _skip = _skip_dg;
    }

    void logic()
    {
    }

    bool isActor() {  return true; }

    bool isActive() { return _does_turn; }

    void waitTurn()
    {
        assert( isActive() );
        _does_turn = false;
        _wait();
    }

    void finishTurn()
    {
        assert( isActive() );
        _does_turn = false;
        _skip();
    }

    mixin BindClass!("C/ACTOR");
}
