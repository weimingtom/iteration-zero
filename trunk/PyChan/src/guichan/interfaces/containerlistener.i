%{
#include <guichan/containerevent.hpp>
%}

namespace gcn
{
    /**
     * Interface for listening for action from a container, such as BasicContainer.
     *
     * @see BasicContainer::addContainerListener, BasicContainer::removeContainerListener,
     *      ContainerEvent
     * @author Olof Naess�n
     * @since 0.9.0
     */
    class ContainerListener
    {
    public:

        /**
         * Destructor.
         */
        virtual ~ContainerListener() { }

        /**
         * Called when a widget is added to a container.
         *
         * @param containerEvent The event of the action.
         * @since 0.9.0
         */
        virtual void widgetAdded(const ContainerEvent& containerEvent) = 0;
    
        /**
         * Called when a widget is removed from a container.
         *
         * @param containerEvent The event of the action.
         * @since 0.9.0
         */
        virtual void widgetRemoved(const ContainerEvent& containerEvent) = 0;
    };
}

