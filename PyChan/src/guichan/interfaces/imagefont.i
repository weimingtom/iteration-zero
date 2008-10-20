%{
#include <string>

#include <guichan/font.hpp>
#include <guichan/rectangle.hpp>
%}

namespace gcn
{
    class Color;
    class Graphics;
    class Image;

    /**
     * A font using an image containing the font data. ImageFont can be used
     * with any image supported by the currently used ImageLoader.
     *
     * These are two examples of an image containing a font.
     *  \image html imagefontexample.bmp
     *  \image html imagefontexample2.bmp
     *
     * The first pixel at coordinate (0,0) tells which color the image font 
     * looks for when seperating glyphs. The glyphs in the image is provided
     * to the image font's constructor in the order they appear in the image.
     *
     * To create an ImageFont from the first image example above the following
     * constructor call should be made:
pqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"); @endcode
     *
     * Noteworthy is that the first glyph actually gives the width of space.
     * Glyphs can, as seen in the second image example above, be seperated with
     * horizontal lines making it possible to draw glyphs on more then one
     * line in the image. However, these horizontal lines must have a height of
     * one pixel!
     */
    class  ImageFont: public Font
    {
    public:

        /**
         * Constructor. Takes an image file containing the font and
         * a string containing the glyphs. The glyphs in the string should
         * be in the same order as they appear in the font image.
         *
         * @param filename The filename of the image.
         * @param glyphs The glyphs found in the image.
         * @throws Exception when glyph list is incorrect or the font file is
         *                   corrupt or if no ImageLoader exists.
         */
        ImageFont(const std::string& filename, const std::string& glyphs);

        /**
         * Constructor. Takes an image containing the font and
         * a string containing the glyphs. The glyphs in the string should
         * be in the same order as they appear in the font image.
         * The image will be deleted in the destructor.
         *
         * @param image The image with font glyphs.
         * @param glyphs The glyphs found in the image.
         * @throws Exception when glyph list is incorrect or the font image is
         *                   is missing.
         */
        ImageFont(Image* image, const std::string& glyphs);

        /**
         * Constructor. Takes an image file containing the font and
         * two boundaries of ASCII values. The font image should include
         * all glyphs specified with the boundaries in increasing ASCII
         * order. The boundaries are inclusive.
         *
         * @param filename The filename of the image.
         * @param glyphsFrom The ASCII value of the first glyph found in the
         *                   image.
         * @param glyphsTo The ASCII value of the last glyph found in the
         *                 image.
         * @throws Exception when glyph bondaries are incorrect or the font
         *                   file is corrupt or if no ImageLoader exists.
         */
        ImageFont(const std::string& filename, 
                  unsigned char glyphsFrom=32,
                  unsigned char glyphsTo=126);

        /**
         * Destructor.
         */
        virtual ~ImageFont();

        /**
         * Draws a glyph.
         *
         * NOTE: You normally won't use this function to draw text since
         *       the Graphics class contains better functions for drawing
         *       text.
         *
         * @param graphics A graphics object used for drawing.
         * @param glyph A glyph to draw.
         * @param x The x coordinate where to draw the glyph.
         * @param y The y coordinate where to draw the glyph.
         * @return The width of the glyph in pixels.
         */
        virtual int drawGlyph(Graphics* graphics, unsigned char glyph,
                              int x, int y);

        /**
         * Sets the space between rows in pixels. Default is 0 pixels.
         * The space can be negative.
         *
         * @param spacing The space between rows in pixels.
         * @see getRowSpacing
         */
        virtual void setRowSpacing(int spacing);

        /**
         * Gets the space between rows in pixels.
         *
         * @return The space between rows in pixels.
         * @see setRowSpacing
         */
        virtual int getRowSpacing();

        /**
         * Sets the spacing between glyphs in pixels. Default is 0 pixels.
         * The space can be negative.
         *
         * @param spacing The glyph space in pixels.
         * @see getGlyphSpacing
         */
        virtual void setGlyphSpacing(int spacing);

        /**
         * Gets the spacing between letters in pixels.
         *
         * @return the spacing.
         * @see setGlyphSpacing
         */
        virtual int getGlyphSpacing();

        /**
         * Gets a width of a glyph in pixels.
         *
         * @param glyph The glyph which width will be returned.
         * @return The width of a glyph in pixels.
         */
        virtual int getWidth(unsigned char glyph) const;


        // Inherited from Font

        virtual int getWidth(const std::string& text) const;

        virtual void drawString(Graphics* graphics, const std::string& text,
                                int x, int y);

        virtual int getHeight() const;

        virtual int getStringIndexAt(const std::string& text, int x) const;

    };
}

