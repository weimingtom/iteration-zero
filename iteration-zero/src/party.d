module party;

import dlisp.bind;
import interfaces;
import character;
import sofu = Sofu.Sofu;

import engine;

class Party
{
    private:
        Character[] _members;
        int _active = 0;
        string _currentLevel;

    public:
        this()
        {
        }

        void load(string filename)
        {
            _members.length = 0;
            auto map = sofu.loadFile(filename);
            foreach(sofu.SofuObject charInfo; map.list("members"))
            {
                add (Character.loadFromSofu (charInfo.asMap()));
            }
        }

        float getTotalWeight()
        {
            return 0;
        }

        Character get(int num)
        {
            return _members[num-1];
        }

        void remove(int num)
        {
            assert( num >= 0 && num < _members.length );
            int i;
            for(i=num; i < _members.length - 1 ; ++ i)
                _members[i] = _members[i+1];
            _members.length = _members.length - 1;
        }

        void add(Character c)
        {
            _members ~= c;
        }

        Character getActive()
        {
            return _members[_active];
        }

        void setActive(int i)
        {
            _active = i-1;
        }

        int getSize()
        {
            return _members.length;
        }

        void placeInLevel(ILevel level, int x, int y)
        {
            foreach(Character c; _members)
            {
                level.placeIObject(x,y,c);
                ++x;
            }
        }

        int opApply(int delegate(ref Character) dg)
        {
            int result = 0;
            foreach(Character c; _members)
            {
                result = dg(c);
                if( result ) break;
            }
            return result;
        }

        mixin BindClass!("C/PARTY");
        mixin BindMethods!(getTotalWeight,get,remove,add,getActive,getSize,setActive);

}
