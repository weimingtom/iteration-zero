%module guichan

%{
#include <string>
#include <guichan/platform.hpp>
%}

namespace gcn
{
    /**
     * An interface for a model that represents a list. It is 
     * used in certain widgets, like the ListBox, to handle a 
     * lists with string elements. If you want to use widgets 
     * like ListBox, make a derived class from this class that 
     * represents your list.
     */
    class  ListModel
    {

    public:
        /**
         * Destructor.
         */
        virtual ~ListModel() { }

        /**
         * Gets the number of elements in the list.
         *
         * @return The number of elements in the list
         */
        virtual int getNumberOfElements() = 0;

        /**
         * Gets an element at a certain index in the list.
         *
         * @param i An index in the list.
         * @return An element as a string at the a certain index.
         */
        virtual std::string getElementAt(int i) = 0;
    };
}


