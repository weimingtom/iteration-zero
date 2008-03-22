module party;

import dlisp.bind;
import interfaces;
import character;

class Party
{
    private:
        Character[] _members;
        string _currentLevel;

    public:
        this()
        {
        }

        void load(string filename)
        {
        }

        void save(string filename)
        {
        }

        float getTotalWeight()
        {
            return 0;
        }

        mixin BindClass!("C/PARTY");
        mixin BindMethods!(load,save,getTotalWeight);
}
