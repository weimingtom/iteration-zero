module menustate;
import engine;
import derelict.sdl.sdl;
import level;
import pipeline;
import std.stdio;
import std.utf;
import util;
import derelict.opengl.gl;

import dlisp.dlisp;
import dlisp.bind;

import gobject, party, character;
import dataset;

import turn;
import character;
import guichan.all;
import guichan.widgets.all;
import guichan.event;
import gamestate;

class MenuOption
{
    public:

        this(string name)
        {
            _name = name;
            int maxx = Engine.instance.xResolution;
            _button = new Button();
            _button.setCaption(caption);
            _button.setSize(maxx/2,40);
        }

        dstring caption() { return toUTF32(_name); }

        Widget widget() { return _button; }

        mixin BindClass!("Menu-Option");
        mixin BindConstructor!(MenuOption function(string));

    private:
        string _name;
        Button _button;
}

class MenuState : public GameState
{
    private:
        string _name;
        
        MenuOption[]  _options;

    public:
        Engine engine;
        DLisp dlisp;

    mixin BindClass!("C/Menu");

    this(string aname = "menu")
    {
        _name = aname;
        engine = Engine.instance;
        dlisp = engine.dlisp;
        MenuOption.bindClass(dlisp.environment);
        bindInstance(dlisp.environment,"*menu*");
    }

    void addOption(MenuOption option)
    {
        _options ~= option;
    }

    string name() { return _name; }

    void start()
    {
        auto topWidget = cast(Container)engine.gui.getTop;
        topWidget.setId( "top" );
        int maxx = engine.xResolution;
        int maxy = engine.yResolution;
        int x = maxx/4;
        int y = 100;
        foreach(MenuOption option; _options)
        {
            topWidget.add(option.widget,x,y);
            y += 50;
        }
    }

    void stop()
    {
    }

    mixin BindMethods!(addOption);
}
