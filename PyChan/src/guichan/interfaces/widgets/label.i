%{
#include <string>

#include <guichan/graphics.hpp>
#include <guichan/widget.hpp>
%}

namespace gcn
{
    /**
     * Implementation of a label capable of displaying a caption.
     */
    class Label: public Widget
    {
    public:
        /**
         * Constructor.
         */
        Label();

        /**
         * Constructor. The label will be automatically resized
         * to fit the caption.
         *
         * @param caption The caption of the label.
         */
        Label(const std::string& caption);

        /**
         * Gets the caption of the label.
         *
         * @return The caption of the label.
         * @see setCaption
         */
        const std::string &getCaption() const;

        /**
         * Sets the caption of the label. It's advisable to call
         * adjustSize after setting of the caption to adjust the
         * label's size to fit the caption.
         *
         * @param caption The caption of the label.
         * @see getCaption, adjustSize
         */
        void setCaption(const std::string& caption);

        /**
         * Sets the alignment of the caption. The alignment is relative
         * to the center of the label.
         *
         * @param alignemnt The alignment of the caption of the label.
         * @see getAlignment, Graphics
         */
        void setAlignment(Graphics::Alignment alignment);

        /**
         * Gets the alignment of the caption. The alignment is relative to
         * the center of the label.
         *
         * @return The alignment of caption of the label.
         * @see setAlignmentm Graphics
         */
        Graphics::Alignment getAlignment() const;

        /**
         * Adjusts the label's size to fit the caption.
         */
        void adjustSize();


        // Inherited from Widget

        virtual void draw(Graphics* graphics);
    };
}

