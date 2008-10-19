%{
#include <string>

#include <guichan/keylistener.hpp>
#include <guichan/mouselistener.hpp>
#include <guichan/widget.hpp>
%}

namespace gcn
{
    /**
     * An implementation of a check box where a user can select or deselect
     * the check box and where the status of the check box is displayed to the user.
     * A check box is capable of displaying a caption. 
     * 
     * If a check box's state changes an action event will be sent to all action 
     * listeners of the check box.
     */
    class CheckBox :
        public Widget,
        public MouseListener,
        public KeyListener
    {
    public:

        /**
         * Contructor.
         */
        CheckBox();

        /**
         * Constructor. The check box will be automatically resized
         * to fit the caption.
         *
         * @param caption The caption of the check box.
         * @param marked True if the check box is selected, false otherwise.
         */
        CheckBox(const std::string &caption, bool selected = false);

        /**
         * Destructor.
         */
        virtual ~CheckBox() { }

        /**
         * Checks if the check box is selected.
         *
         * @return True if the check box is selected, false otherwise.
         * @see setSelected
         */
        bool isSelected() const;

        /**
         * Sets the check box to be selected or not.
         *
         * @param selected True if the check box should be set as selected.
         * @see isSelected
         */
        void setSelected(bool selected);

        /**
         * Gets the caption of the check box.
         *
         * @return The caption of the check box.
         * @see setCaption
         */
        const std::string &getCaption() const;

        /**
         * Sets the caption of the check box. It's advisable to call
         * adjustSize after setting of the caption to adjust the
         * check box's size to fit the caption.
         *
         * @param caption The caption of the check box.
         * @see getCaption, adjustSize
         */
        void setCaption(const std::string& caption);

        /**
         * Adjusts the check box's size to fit the caption.
         */
        void adjustSize();


        // Inherited from Widget

        virtual void draw(Graphics* graphics);


        // Inherited from KeyListener

        virtual void keyPressed(KeyEvent& keyEvent);


        // Inherited from MouseListener

        virtual void mouseClicked(MouseEvent& mouseEvent);

        virtual void mouseDragged(MouseEvent& mouseEvent);

    };
}

