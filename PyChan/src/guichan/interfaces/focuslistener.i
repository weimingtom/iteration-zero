%{
#include <string>

#include <guichan/event.hpp>
%}

%nodefaultctor;
namespace gcn
{
    /**
     * Interface for listening for focus events from widgets.
     *
     * @see Widget::addFocusListener, Widget::removeFocusListener
     * @author Olof Naess�n
     * @since 0.7.0
     */
    class FocusListener
    {
    public:

        /**
         * Destructor.
         */
        virtual ~FocusListener() { }

        /**
         * Called when a widget gains focus. 
         *
         * @param event Discribes the event.
         */
        virtual void focusGained(const Event& event) { };

        /**
         * Called when a widget loses focus. 
         *
         * @param event Discribes the event.
         */
        virtual void focusLost(const Event& event) { };
    };
}
%clearnodefaultctor;

