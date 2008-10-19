%{
#include <guichan/image.hpp>
#include <guichan/widget.hpp>
%}

namespace gcn
{
    /**
     * Implements an icon capable of displaying an image.
     */
    class Icon: public Widget
    {
    public:
        /**
         * Default constructor.
         */
        Icon();

        /**
         * Constructor.
         *
         * @param filename The filename of the image to display.
         */
        Icon(const std::string& filename);

        /**
         * Constructor.
         *
         * @param image The image to display.
         */
        Icon(const Image* image);

        /**
         * Descructor.
         */
        virtual ~Icon();

        /**
         * Sets the image to display. Existing image is freed automatically
         * if it was loaded internally.
         *
         * @param image The image to display. 
         */
        void setImage(const Image* image);

        /**
         * Gets the current image.
         *
         * @return The current image.
         */
        const Image* getImage() const;


        // Inherited from Widget

        virtual void draw(Graphics* graphics);
    };
}

