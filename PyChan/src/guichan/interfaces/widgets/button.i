%{
#include <string>

#include <guichan/focuslistener.hpp>
#include <guichan/graphics.hpp>
#include <guichan/keylistener.hpp>
#include <guichan/mouseevent.hpp>
#include <guichan/mouselistener.hpp>
#include <guichan/widget.hpp>
%}

namespace gcn
{
    /**
     * An implementation of a regular clickable button. A button is capable of
     * displaying a caption. 
     *
     * If a button is clicked an action event will be sent to all action listener's 
     * of the button.
     *
     * @see ImageButton
     */
    class Button : public Widget,
                                     public MouseListener,
                                     public KeyListener,
                                     public FocusListener
    {
    public:
        /**
         * Constructor.
         */
        Button();

        /**
         * Constructor. The button will be automatically resized
         * to fit the caption.
         *
         * @param caption The caption of the button.
         */
        Button(const std::string& caption);

        /**
         * Sets the caption of the button. It's advisable to call
         * adjustSize after setting of the caption to adjust the
         * button's size to fit the caption.
         *
         * @param caption The caption of the button.
         * @see getCaption, adjustSize
         */
        void setCaption(const std::string& caption);

        /**
         * Gets the caption of the button.
         *
         * @return The caption of the button.
         */
        const std::string& getCaption() const;

        /**
         * Sets the alignment of the caption. The alignment is relative
         * to the center of the button.
         *
         * @param alignment The alignment of the caption.
         * @see getAlignment, Graphics
         */
        void setAlignment(Graphics::Alignment alignment);

        /**
         * Gets the alignment of the caption.
         *
         * @return The alignment of the caption.
         * @see setAlignment, Graphics
         */
        Graphics::Alignment getAlignment() const;

        /**
         * Sets the spacing between the border of the button and its caption.
         *
         * @param spacing The default value for spacing is 4 and can be changed 
         *                using this method.
         * @see getSpacing
         */
        void setSpacing(unsigned int spacing);

        /**
         * Gets the spacing between the border of the button and its caption.
         *
         * @return spacing.
         * @see setSpacing
         */
        unsigned int getSpacing() const;

        /**
         * Adjusts the button's size to fit the caption.
         */
        void adjustSize();


        //Inherited from Widget

        virtual void draw(Graphics* graphics);


        // Inherited from FocusListener

        virtual void focusLost(const Event& event);


        // Inherited from MouseListener

        virtual void mousePressed(MouseEvent& mouseEvent);

        virtual void mouseReleased(MouseEvent& mouseEvent);

        virtual void mouseEntered(MouseEvent& mouseEvent);

        virtual void mouseExited(MouseEvent& mouseEvent);

        virtual void mouseDragged(MouseEvent& mouseEvent);


        // Inherited from KeyListener

        virtual void keyPressed(KeyEvent& keyEvent);

        virtual void keyReleased(KeyEvent& keyEvent);

    };
}

