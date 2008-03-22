module character;

import interfaces;
import gobject;
import dlisp.bind;

class Character : GObject, ICharacter
{
    private:
        string _name;

    public:
        this(string name)
        {
            super(null,0,0);
            _name = name;
        }

    mixin AttributedObject!();

    mixin BindClass!("C/CHARACTER");
    mixin BindConstructor!(Character function(string));
    mixin BindMethods!(getSTR,getDEX,getEND,getINT,getPER);
    mixin BindMethods!(getMaxHP,getHP,getMaxEnergy,getEnergy);
    mixin BindMethods!(getShieldEffect,getArmorEffect,getShaderEffect);
    mixin BindMethods!(isShieldActive,isShaderActive);
}
