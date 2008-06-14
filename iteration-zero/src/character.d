module character;

import interfaces;
import gobject;
import dlisp.bind;

import std.string;
import sofu = Sofu.Sofu;
import dataset;
import actor;

class Character : Actor, ICharacter
{
private:
    string _name;

public:
    this(string name)
    {
        _name = name;
    }

    static Character load(string filename)
    {
        sofu.Map map = sofu.loadFile(filename);
        return loadFromSofu(map);
    }

    static Character loadFromSofu(sofu.Map map)
    {
        Character c = new Character( map.value("name").toString() );
        c.setModel( Dataset.instance.getModel( map.value("model").toString() ) );
        sofu.Map attrs = map.map("attributes");
        c.setSTR( attrs.value("STR").toInt() );
        c.setDEX( attrs.value("DEX").toInt() );
        c.setEND( attrs.value("END").toInt() );
        c.setINT( attrs.value("INT").toInt() );
        c.setPER( attrs.value("PER").toInt() );

        c.calculateDerivedAttributes();
        c.setHP (c.getMaxHP());
        return c;
    }

    string toString()
    {
        sofu.Map map = new sofu.Map;
        map.setAttribute ("name", new sofu.Value (getName ()));
        map.setAttribute ("model", new sofu.Value (getModel().getName()));
        sofu.Map attrs = new sofu.Map;
        attrs.setAttribute ("STR" , new sofu.Value (getSTR ()));
        attrs.setAttribute ("DEX" , new sofu.Value (getDEX ()));
        attrs.setAttribute ("END" , new sofu.Value (getEND ()));
        attrs.setAttribute ("INT" , new sofu.Value (getINT ()));
        attrs.setAttribute ("PER" , new sofu.Value (getPER ()));
        map.setAttribute ("attributes", attrs);
        map.setAttribute ("inventory", new sofu.List);
        return map.outputString();
    }

    string getName() { return _name; }

    void calculateDerivedAttributes()
    {
        setMaxHP (2*getEND() + getSTR());
    }

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
