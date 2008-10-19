%{
#include <guichan/event.hpp>
%}
%include interfaces/event.i

namespace gcn
{
    class Container;
    class Widget;

    /**
     * Represents a container event. A container event is an event
     * that can be fired by a container whenever a widget is added
     * or removed.
     *
     * Any object can listen for actions from a container by implementing
     * the ContainerListener interface.
     *
     * @see Container::addContainerListener, Container::removeContainerListener,
     *      Container::distributeContainerEvent
     * @author Olof Naess�n
     * @since 0.9.0
     */
    class ContainerEvent: public Event
    {
    public:

        /**
         * Constructor.
         *
         * @param source The source widget of the event.
         * @param container The container the event concerns.
         */
        ContainerEvent(Widget* source, Container* container);

        /**
         * Destructor.
         */
        virtual ~ContainerEvent();

        /**
         * Gets the container the event concerns.
         * 
         * @return The container the event concerns.
         */ 
        Container* getContainer() const;

    };
}


