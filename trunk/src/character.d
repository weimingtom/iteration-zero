module character;

import interfaces;
import gobject;
import dlisp.bind;

class Character : GObject, ICharacter
{
    mixin AttributedObject!();

    mixin BindClass!("C/CHARACTER");
    mixin BindMethods!(getSTR,getDEX,getEND,getINT,getPER);
    mixin BindMethods!(getMaxHP,getHP,getMaxEnergy,getEnergy);
    mixin BindMethods!(getShieldEffect,getArmorEffect,getShaderEffect);
    mixin BindMethods!(isShieldActive,isShaderActive);
}
