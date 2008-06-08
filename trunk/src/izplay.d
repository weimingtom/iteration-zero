module izplay;

import engine;
import levelstate;
import std.stdio;

import dlisp.dlisp;
import party;
import character;

void bindGame(Environment env)
{
    Party.bindClass(env);
    Character.bindClass(env);
}

void main(char[][] argv)
{
    Engine engine = new Engine;
    scope(exit) delete engine;      // when we exit, perform cleanup

    LevelState lstate =  new LevelState;

    engine.addState( lstate );
    bindGame(engine.dlisp.environment);
 
    foreach(string filename; argv[1 .. $])
        engine.dlisp.parseEvalPrint("(LOAD \"" ~ filename ~ "\" T)", false);

    engine.start ("menu");

    engine.mainLoop();
}
