%{
#include <guichan/mouselistener.hpp>
#include <guichan/widgets/container.hpp>
%}

namespace gcn
{
    /**
     * An implementation of a movable window that can contain other widgets.
     */
    class Window : public Container, public MouseListener
    {
    public:
        /**
         * Constructor.
         */
        Window();

        /**
         * Constructor. The window will be automatically resized in height
         * to fit the caption.
         *
         * @param caption the caption of the window.
         */
        Window(const std::string& caption);

        /**
         * Destructor.
         */
        virtual ~Window();

        /**
         * Sets the caption of the window.
         *
         * @param caption The caption of the window.
         * @see getCaption
         */
        void setCaption(const std::string& caption);

        /**
         * Gets the caption of the window.
         *
         * @return the caption of the window.
         * @see setCaption
         */
        const std::string& getCaption() const;

        /**
         * Sets the alignment of the caption.
         *
         * @param alignment The alignment of the caption.
         * @see getAlignment, Graphics
         */
        void setAlignment(Graphics::Alignment alignment);

        /**
         * Gets the alignment of the caption.
         *
         * @return The alignment of caption.
         * @see setAlignment, Graphics
         */
        Graphics::Alignment getAlignment() const;

        /**
         * Sets the padding of the window. The padding is the distance between the
         * window border and the content.
         *
         * @param padding The padding of the window.
         * @see getPadding
         */
        void setPadding(unsigned int padding);

        /**
         * Gets the padding of the window. The padding is the distance between the
         * window border and the content.
         *
         * @return The padding of the window.
         * @see setPadding
         */
        unsigned int getPadding() const;

        /**
         * Sets the title bar height.
         *
         * @param height The title height value.
         * @see getTitleBarHeight
         */
        void setTitleBarHeight(unsigned int height);

        /**
         * Gets the title bar height.
         *
         * @return The title bar height.
         * @see setTitleBarHeight
         */
        unsigned int getTitleBarHeight();

        /**
         * Sets the window to be moveble or not.
         *
         * @param movable True if the window should be movable, false otherwise.
         * @see isMovable
         */
        void setMovable(bool movable);

        /**
         * Checks if the window is movable.
         *
         * @return True if the window is movable, false otherwise.
         * @see setMovable
         */
        bool isMovable() const;

        /**
         * Sets the window to be opaque or not. An opaque window will draw it's background
         * and it's content. A non opaque window will only draw it's content.
         *
         * @param opaque True if the window should be opaque, false otherwise.
         * @see isOpaque
         */
        void setOpaque(bool opaque);

        /**
         * Checks if the window is opaque.
         *
         * @return True if the window is opaque, false otherwise.
         * @see setOpaque
         */
        bool isOpaque();

        /**
         * Resizes the window to fit the content.
         */
        virtual void resizeToContent();


        // Inherited from BasicContainer

        virtual Rectangle getChildrenArea();


        // Inherited from Widget

        virtual void draw(Graphics* graphics);


        // Inherited from MouseListener

        virtual void mousePressed(MouseEvent& mouseEvent);

        virtual void mouseDragged(MouseEvent& mouseEvent);

        virtual void mouseReleased(MouseEvent& mouseEvent);

    };
}

