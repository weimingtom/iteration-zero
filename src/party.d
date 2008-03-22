module party;

import dlisp.bind;
import interfaces;


class Party
{
    private:
        ICharacter[] _members;
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
