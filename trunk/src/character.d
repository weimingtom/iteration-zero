module character;

import interfaces;
import gobject;
import dlisp.bind;

class Character : GObject, ICharacter
{
    mixin AttributedObject!();
//     int getArmorEffect();
//     int getShieldEff();

    // STATE VALUES
//     bool isShieldOn();
//     bool isShaded();

    // CAPABILITIES
//     bool canEquip(IItem item);

    mixin BindClass!("C/CHARACTER");
    mixin BindMethods!(getSTR,getDEX,getEND,getINT,getPER);
    mixin BindMethods!(getMaxHP,getHP,getMaxEnergy,getEnergy);
}
