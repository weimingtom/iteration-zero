%{
#include <guichan/keyevent.hpp>
#include <guichan/platform.hpp>
%}

namespace gcn
{
    class Key;

    /**
     * Interface for listening for key events from widgets.
     *
     * @see Widget::addKeyListener, Widget::removeKeyListener
     */
    class KeyListener
    {
    public:

        /**
         * Destructor.
         */
        virtual ~KeyListener() { }

        /**
         * Called if a key is pressed when the widget has keyboard focus.
         * If a key is held down the widget will generate multiple key
         * presses.
         *
         * @param keyEvent Discribes the event.
         */
        virtual void keyPressed(KeyEvent& keyEvent) { }

        /**
         * Called if a key is released when the widget has keyboard focus.
         *
         * @param keyEvent Discribes the event.
         */
        virtual void keyReleased(KeyEvent& keyEvent) { }

    protected:
        /**
         * Constructor.
         *
         * You should not be able to make an instance of KeyListener,
         * therefore its constructor is protected.
         */
        KeyListener() { }
    };
}

