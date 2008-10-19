%{
#include <string>

#include <guichan/actionevent.hpp>
%}

namespace gcn
{

    /**
     * Interface for listening for action events from widgets.
     *
     * @see Widget::addActionListener, Widget::removeActionListener,
     *      ActionEvent
     * @author Olof Naess�n
     * @author Per Larsson
     */
    class ActionListener
    {
    public:

        /**
         * Destructor.
         */
        virtual ~ActionListener() { }

        /**
         * Called when an action is recieved from a widget. It is used
         * to be able to recieve a notification that an action has
         * occured.
         *
         * @param actionEvent The event of the action.
         * @since 0.6.0
         */
        virtual void action(const ActionEvent& actionEvent) = 0;
    };
}

