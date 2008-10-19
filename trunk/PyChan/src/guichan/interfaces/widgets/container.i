%{
#include <list>

#include <guichan/containerlistener.hpp>
#include <guichan/graphics.hpp>
#include <guichan/widget.hpp>
%}
%include "interfaces/containerlistener.i"

namespace gcn
{
    /**
     * An implementation of a container able to contain other widgets. A widget's 
     * position in the container is relative to the container itself and not the screen. 
     * A container is the most common widget to use as the Gui's top widget as makes the Gui
     * able to contain more than one widget.
     *
     * @see Gui::setTop
     */
    class Container: public Widget
    {
    public:

        /**
         * Constructor. A container is opauqe as default, if you want a
         * none opaque container call setQpaque(false).
         *
         * @see setOpaque, isOpaque
         */
        Container();

        /**
         * Destructor.
         */
        virtual ~Container();

        /**
         * Sets the container to be opaque or not. If the container
         * is opaque its background will be drawn, if it's not opaque
         * its background will not be drawn, and thus making the container
         * completely transparent.
         *
         * NOTE: This is not the same as to set visibility. A non visible
         *       container will not itself nor will it draw its content.
         *
         * @param opaque True if the container should be opaque, false otherwise.
         * @see isOpaque
         */
        void setOpaque(bool opaque);

        /**
         * Checks if the container is opaque or not.
         *
         * @return True if the container is opaque, false otherwise.
         * @see setOpaque
         */
        bool isOpaque() const;

        /**
         * Adds a widget to the container.
         *
         * @param widget The widget to add.
         * @see remove, clear
         */
        virtual void add(Widget* widget);

        /**
         * Adds a widget to the container and also specifies the widget's
         * position in the container. The position is relative to the container
         * and not relative to the screen.
         *
         * @param widget The widget to add.
         * @param x The x coordinate for the widget.
         * @param y The y coordinate for the widget.
         * @see remove, clear
         */
        virtual void add(Widget* widget, int x, int y);

        /**
         * Removes a widget from the Container.
         *
         * @param widget The widget to remove.
         * @throws Exception when the widget has not been added to the
         *                   container.
         * @see add, clear
         */
        virtual void remove(Widget* widget);

        /**
         * Clears the container of all widgets.
         *
         * @see add, remove
         */
        virtual void clear();

        /**
         * Finds a widget given an id.
         *
         * @param id The id to find a widget by.
         * @return A widget with a corrosponding id, NULL if no widget 
         *         is found.
         * @see Widget::setId
         */
        virtual Widget* findWidgetById(const std::string &id);

        /**
         * Adds a container listener to the container. When a widget is
         * added or removed an event will be sent to all container 
         * listeners of the container
         *
         * @param containerListener The container listener to add.
         * @since 0.9.0
         */
        void addContainerListener(ContainerListener* containerListener);

        /**
         * Removes a container listener from the container.
         *
         * @param containerListener The container listener to remove.
         * @since 0.9.0
         */
        void removeContainerListener(ContainerListener* containerListener);

        /**
         * Returns the children of the container.
         *
         * @return The children of the container.
         */
        const std::list<Widget*>& getChildren() const;

        /**
         * Resizes the Container's size to fit te content exactly.
         */
        void resizeToContent();


        // Inherited from Widget

        virtual void draw(Graphics* graphics);

        virtual Rectangle getChildrenArea();

    };
}

