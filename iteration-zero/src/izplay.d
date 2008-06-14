module izplay;

import engine;
import levelstate;
import std.stdio;

import dlisp.dlisp;
import party;
import character;

import material;
import dataset;

void bindGame(IEnvironment env)
{
    Party.bindClass(env);
    Character.bindClass(env);
    Material.bindClass(env);
    Dataset.instance.bindInstance(env,"*DB*");
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
