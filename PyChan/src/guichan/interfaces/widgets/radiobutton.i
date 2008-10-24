%{
#include <map>
#include <string>

#include <guichan/keylistener.hpp>
#include <guichan/mouselistener.hpp>
#include <guichan/widget.hpp>
%}
namespace gcn
{
    /**
     * An implementation of a radio button where a user can select or deselect
     * the radio button and where the status of the radio button is displayed to the user.
     * A radio button can belong to a group and when a radio button belongs to a
     * group only one radio button can be selected in the group. A radio button is
     * capable of displaying a caption.
     * 
     * If a radio button's state changes an action event will be sent to all action 
     * listeners of the check box.
     */
    class RadioButton :
        public Widget,
        public MouseListener,
        public KeyListener
    {
    public:

        /**
         * Constructor.
         */
        RadioButton();

        /**
         * Constructor. The radio button will be automatically resized
         * to fit the caption.
         *
         * @param caption The caption of the radio button.
         * @param group The group the radio button should belong to.
         * @param selected True if the radio button should be selected.
         */
        RadioButton(const std::string &caption,
                    const std::string &group,
                    bool selected = false);

        /**
         * Destructor.
         */
        virtual ~RadioButton();

        /**
         * Checks if the radio button is selected.
         *
         * @return True if the radio button is selecte, false otherwise.
         * @see setSelected
         */
        bool isSelected() const;

        /**
         * Sets the radio button to selected or not.
         *
         * @param selected True if the radio button should be selected,
         *                 false otherwise.
         * @see isSelected
         */
        void setSelected(bool selected);

        /**
         * Gets the caption of the radio button.
         *
         * @return The caption of the radio button.
         * @see setCaption
         */
        const std::string &getCaption() const;

        /**
         * Sets the caption of the radio button. It's advisable to call
         * adjustSize after setting of the caption to adjust the
         * radio button's size to fit the caption.
         *
         * @param caption The caption of the radio button.
         * @see getCaption, adjustSize
         */
        void setCaption(const std::string caption);

        /**
         * Sets the group the radio button should belong to. Note that
         * a radio button group is unique per application, not per Gui object
         * as the group is stored in a static map.
         *
         * @param group The name of the group.
         * @see getGroup
         */
        void setGroup(const std::string &group);

        /**
         * Gets the group the radio button belongs to.
         *
         * @return The group the radio button belongs to.
         * @see setGroup
         */
        const std::string &getGroup() const;

        /**
         * Adjusts the radio button's size to fit the caption.
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

