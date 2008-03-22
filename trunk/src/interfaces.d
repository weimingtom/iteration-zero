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
//     dstring getName();
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
    int getHP();

    int getMaxEnergy();
    int getEnergy();

//     int getArmorEff();
//     int getShieldEff();
// 
//     // STATE VALUES
//     bool isShieldOn();
//     bool isShaded();
// 
//     // CAPABILITIES
//     bool canEquip(IItem item);
}

interface IItem
{
}


template AttributedObject()
{
    private:
        Attribute[] _attributes;
        float _weight;

        int _max_hp;
        int _hp;

        int _max_energy;
        int _energy;

    public:

        int getAttribute(Attribute a)
        {
            return _attributes[a];
        }

        int getSTR() {
            return getAttribute(Attribute.STR);
        }
        int getDEX() {
            return getAttribute(Attribute.DEX);
        }
        int getEND() {
            return getAttribute(Attribute.END);
        }
        int getINT() {
            return getAttribute(Attribute.INT);
        }
        int getPER() {
            return getAttribute(Attribute.PER);
        }

        float getWeight() { return _weight; }

        int getMaxHP() { return _max_hp; }
        int getHP() { return _hp; }
        int getMaxEnergy() { return _max_energy; }
        int getEnergy() { return _energy; }
}