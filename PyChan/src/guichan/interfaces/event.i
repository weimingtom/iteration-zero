%{
%}

namespace gcn
{
    class Widget;

    /**
     * Base class for all events. All events in Guichan should
     * inherit from this class.
     *
     * @author Olof Naess�n
     * @since 0.6.0
     */
    class Event
    {
    public:

        /**
         * Constructor.
         *
         * @param source The source widget of the event.
         */
        Event(Widget* source);

        /**
         * Destructor.
         */
        virtual ~Event();

        /**
         * Gets the source widget of the event. The function
         * is used to tell which widget fired an event.
         *
         * @return The source widget of the event.
         */
        Widget* getSource() const;
    };
}

