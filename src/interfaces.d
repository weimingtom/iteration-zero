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
    PER,
    MAX_ATTR
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
    void setLevel(ILevel l);
    void setPosition(int x,int y);
}

interface ILevel
{
    bool isBlocked(int x, int y);
    void loadDataset(string);
    void placeIObject(int x, int y, IGObject o);
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

enum BodySlot
{
    HEAD,
    BRAIN,
    TORSO,
    BACKPACK,
    WAIST,
    HAND,
    FEET
}

interface ICharacter
{
    string getName();
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

    int getArmorEffect();
    int getShieldEffect();
    int getShaderEffect();

    // STATE VALUES
    bool isShieldActive();
    bool isShaderActive();

// 
//     // CAPABILITIES
//     bool canEquip(IItem item);
}

enum ItemType
{
    WEAPON
}

interface IItem
{
    float getWeight();
    bool canBeEquipped();
    BodySlot getBodySlot();
}

interface IActor
{
}

template AttributedObject()
{
    private:
        int[Attribute.MAX_ATTR] _attributes;

        float _weight;

        int _max_hp;
        int _hp;

        int _max_energy;
        int _energy;

        int _armor_effect;
        int _shield_effect;
        int _shader_effect;

        bool _shield_on;
        bool _shader_on;

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

        void setAttribute(Attribute a,int new_value)
        {
            _attributes[a] = new_value;
        }

        void setSTR(int new_value)
        {
            setAttribute(Attribute.STR,new_value);
        }

        void setDEX(int new_value)
        {
            setAttribute(Attribute.DEX,new_value);
        }

        void setEND(int new_value)
        {
            setAttribute(Attribute.END,new_value);
        }

        void setINT(int new_value)
        {
            setAttribute(Attribute.INT,new_value);
        }

        void setPER(int new_value)
        {
            setAttribute(Attribute.PER,new_value);
        }

        float getWeight() { return _weight; }

        int getMaxHP() { return _max_hp; }
        int getHP() { return _hp; }
        int getMaxEnergy() { return _max_energy; }
        int getEnergy() { return _energy; }

        void setMaxHP(int max_hp) { _max_hp = max_hp; }
        void setHP(int hp) { _hp = hp; }

        int getArmorEffect() { return _armor_effect; }
        int getShieldEffect() { return _shield_effect; }
        int getShaderEffect() { return _shader_effect; }

        bool isShieldActive() { return _shield_on; }
        bool isShaderActive() { return _shader_on; }
}