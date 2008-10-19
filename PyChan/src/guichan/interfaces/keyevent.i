%{
#include <guichan/inputevent.hpp>
#include <guichan/key.hpp>
%}

namespace gcn
{
    class Widget;

    /**
     * Represents a key event.
     */
    class KeyEvent: public InputEvent
    {
    public:
        /**
         * Key event types.
         */
        enum
        {
            Pressed = 0,
            Released
        };

        /**
         * Constructor.
         *
         * @param source The widget the event concerns..
         * @param distributor The distributor of the event.
         * @param isShiftPressed True if shift is pressed, false otherwise.
         * @param isControlPressed True if control is pressed, false otherwise.
         * @param isAltPressed True if alt is pressed, false otherwise.
         * @param isMetaPressed True if meta is pressed, false otherwise.
         * @param type The type of the event. A value from KeyEventType.
         * @param isNumericPad True if the event occured on the numeric pad,
         *                     false otherwise.
         * @param key The key of the event.
         */
        KeyEvent(Widget* source,
                 Widget* distributor,
                 bool isShiftPressed,
                 bool isControlPressed,
                 bool isAltPressed,
                 bool isMetaPressed,
                 unsigned int type,
                 bool isNumericPad,
                 const Key& key);

        /**
         * Destructor.
         */
        virtual ~KeyEvent();

        /**
         * Gets the type of the event.
         *
         * @return The type of the event.
         */
         unsigned int getType() const;

        /**
         * Checks if the key event occured on the numeric pad.
         *
         * @return True if key event occured on the numeric pad,
         *         false otherwise.
         *
         */
        bool isNumericPad() const;

        /**
         * Gets the key of the event.
         *
         * @return The key of the event.
         */
        const Key& getKey() const;

    };
}

