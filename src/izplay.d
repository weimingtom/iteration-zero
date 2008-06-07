module izplay;

import engine;
import levelstate;
import menustate;
import std.stdio;

void main(char[][] argv)
{
    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    LevelState lstate =  new LevelState;
    MenuState mstate = new MenuState;

    engine.addState( lstate );
    engine.addState( mstate );

    foreach(string filename; argv[1 .. $])
        engine.dlisp.parseEvalPrint("(LOAD \"" ~ filename ~ "\" T)", false);

    engine.start ("menu");

    engine.mainLoop();
}
