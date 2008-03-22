module character;

import interfaces;
import gobject;
import dlisp.bind;

import std.string;

class Character : GObject, ICharacter
{
    private:
        string _name;

    public:
        this(string name)
        {
            _name = name;
        }

    string getName() { return _name; }

    mixin AttributedObject!();

    mixin BindClass!("C/CHARACTER");
    mixin BindConstructor!(Character function(string));
    mixin BindMethods!(getSTR,getDEX,getEND,getINT,getPER);
    mixin BindMethods!(setSTR,setDEX,setEND,setINT,setPER);
    mixin BindMethods!(getMaxHP,getHP,getMaxEnergy,getEnergy);
    mixin BindMethods!(getShieldEffect,getArmorEffect,getShaderEffect);
    mixin BindMethods!(isShieldActive,isShaderActive);
    mixin BindMethods!(getName);
}
