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
            foreach(sofu.SofuObject charfile; map.list("members"))
            {
                add( Character.load(charfile.toString()) );
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


        mixin BindClass!("C/PARTY");
        mixin BindMethods!(getTotalWeight,get,remove,add,getActive,getSize);
}
