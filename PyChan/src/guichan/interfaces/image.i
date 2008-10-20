%{
#include <string>
%}

namespace gcn
{
    class Color;
    class ImageLoader;

    /**
     * Holds an image. To be able to use this class you must first set an
     * ImageLoader in Image by calling
     * @code Image::setImageLoader(myImageLoader) @endcode
     * The function is static. If this is not done, the constructor taking a
     * filename will throw an exception. The ImageLoader you use must be
     * compatible with the Graphics object you use.
     *
     * EXAMPLE: If you use SDLGraphics you should use SDLImageLoader.
     *          Otherwise your program might crash in a most bizarre way.
     * @see AllegroImageLoader, HGEImageLoader, OpenLayerImageLoader,
     *      OpenGLAllegroImageLoader, OpenGLSDLImageLoader, SDLImageLoader
     * @since 0.1.0
     */
    class Image
    {
    public:

        /**
         * Constructor.
         */
        Image();

        /**
         * Destructor.
         */
        virtual ~Image();

        /**
         * Loads an image by using the class' image laoder. All image loaders implemented
         * in Guichan return a newly instantiated image which must be deleted in
         * order to avoid a memory leak.
         *
         * NOTE: The functions getPixel and putPixel are only guaranteed to work
         *       before an image has been converted to display format.
         *
         * @param filename The file to load.
         * @param convertToDisplayFormat True if the image should be converted
         *                               to display, false otherwise.
         * @since 0.5.0
         */
        static Image* load(const std::string& filename, bool convertToDisplayFormat = true);

        /**
         * Gets the image loader used for loading images.
         *
         * @return The image loader used for loading images.
         * @see setImageLoader, AllegroImageLoader, HGEImageLoader, 
         *      OpenLayerImageLoader, OpenGLAllegroImageLoader, 
         *      OpenGLSDLImageLoader, SDLImageLoader
         * @since 0.1.0
         */
        static ImageLoader* getImageLoader();

        /**
         * Sets the ImageLoader to be used for loading images.
         *
         * IMPORTANT: The image loader is static and MUST be set before 
         *            loading images!
         *
         * @param imageLoader The image loader to be used for loading images.
         * @see getImageLoader, AllegroImageLoader, HGEImageLoader, 
         *      OpenLayerImageLoader, OpenGLAllegroImageLoader, 
         *      OpenGLSDLImageLoader, SDLImageLoader
         * @since 0.1.0
         */
        static void setImageLoader(ImageLoader* imageLoader);

        /**
         * Frees an image.
         *
         * @since 0.5.0
         */
        virtual void free() = 0;

        /**
         * Gets the width of the image.
         *
         * @return The width of the image.
         *
         * @since 0.1.0
         */
        virtual int getWidth() const = 0;

        /**
         * Gets the height of the image.
         *
         * @return The height of the image.
         *
         * @since 0.1.0
         */
        virtual int getHeight() const = 0;

        /**
         * Gets the color of a pixel at coordinate (x, y) in the image.
         *
         * IMPORTANT: Only guaranteed to work before the image has been
         *            converted to display format.
         *
         * @param x The x coordinate.
         * @param y The y coordinate.
         * @return The color of the pixel.
         *
         * @since 0.5.0
         */
        virtual Color getPixel(int x, int y) = 0;

        /**
         * Puts a pixel with a certain color at coordinate (x, y).
         *
         * @param x The x coordinate.
         * @param y The y coordinate.
         * @param color The color of the pixel to put.
         * @since 0.5.0
         */
        virtual void putPixel(int x, int y, const Color& color) = 0;

        /**
         * Converts the image, if possible, to display format.
         *
         * IMPORTANT: Only guaranteed to work before the image has been
         *            converted to display format.
         * @since 0.5.0
         */
        virtual void convertToDisplayFormat() = 0;
    };
}
