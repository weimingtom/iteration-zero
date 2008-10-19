%{
#include <guichan/event.hpp>
%}

namespace gcn
{
    /**
     * Base class for all events concerning input.
     *
     * @author Olof Naess�n
     * @since 0.6.0
     */
    class  InputEvent: public Event
    {
    public:

        /**
         * Constructor.
         *
         * @param source The widget the event concerns.
         * @param distributor The distributor of the event.
         * @param isShiftPressed True if shift is pressed, false otherwise.
         * @param isControlPressed True if control is pressed, false otherwise.
         * @param isAltPressed True if alt is pressed, false otherwise.
         * @param isMetaPressed True if meta is pressed, false otherwise.
         */
        InputEvent(Widget* source,
                   Widget* distributor,
                   bool isShiftPressed,
                   bool isControlPressed,
                   bool isAltPressed,
                   bool isMetaPressed);

        /**
         * Checks if shift is pressed.
         *
         * @return True if shift was pressed at the same time as the key,
         *         false otherwise.
         */
        bool isShiftPressed() const;

        /**
         * Checks if control is pressed.
         *
         * @return True if control was pressed at the same time as the key,
         *         false otherwise.
         */
        bool isControlPressed() const;

        /**
         * Checks if alt is pressed.
         *
         * @return True if alt was pressed at the same time as the key,
         *         false otherwise.
         */
        bool isAltPressed() const;

        /**
         * Checks whether meta is pressed.
         *
         * @return True if meta was pressed at the same time as the key,
         *         false otherwise.
         */
        bool isMetaPressed() const;

        /**
         * Marks the event as consumed. Input event listeners may discard
         * consumed input or act on consumed input. An example of a widget
         * that discards consumed input is the ScrollArea widget that 
         * discards consumed mouse wheel events so the ScrollArea will not 
         * scroll if for instance a Slider's value inside the ScrollArea was 
         * changed with the mouse wheel.
         *
         * @see isConsumed
         */
        void consume();

        /**
         * Checks if the input event is consumed.
         *
         * @return True if the input event is consumed, 
         *         false otherwise.
         * @see consume
         */
        bool isConsumed() const;

        /**
         * Gets the distributor of the event. The function is
         * used to tell which widget actually distributed the 
         * event. As input events bubbles up, the source of the event
         * may not be the same as the distributor of the event.
         */ 
        Widget* getDistributor() const;

    };
}

