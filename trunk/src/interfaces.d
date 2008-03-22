module interfaces;

private
{
    import std.signals;
}

enum Attribute
{
    STR,
    DEX,
    END,
    INT,
    PER
}

class SignalObj(T ...)
{
    mixin Signal!(T);
}

class GObjectBase
{
    SignalObj!(GObjectBase,int,int) arrivedAt;

    this()
    {
        arrivedAt = new SignalObj!(GObjectBase,int,int);
    }
}

interface IGObject
{
}

interface ILevel
{
    bool isBlocked(int x, int y);
    void loadDataset(string);
}

enum ObjectType
{
    CHARACTER,
    ITEM,
    SCENERY,
    CRITTER
}

string typeToString(ObjectType t)
{
    switch(t)
    {
        case ObjectType.CHARACTER: return "character";
        case ObjectType.ITEM: return "item";
        case ObjectType.SCENERY: return "scenery";
        case ObjectType.CRITTER: return "critter";
        default:
            return "scenery";
    }
}

interface ICharacter
{
    dstring getName();
    int getAttribute(Attribute a);

    // PRIMARY ATTRIBUTES
    int getSTR();
    int getDEX();
    int getEND();
    int getINT();
    int getPER();

    // SECONDARY ATTRIBUTES
    float getWeight();

    int getMaxHP();
    int getCurrentHP();

    int getMaxEnergy();
    int getCurrentEnergy();

    int getArmorEff();
    int getShieldEff();

    // STATE VALUES
    bool isShieldOn();
    bool isShaded();

    // CAPABILITIES
    bool canEquip(IItem item);
}

interface IItem
{
}
