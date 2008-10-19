%{
#include <string>
%}

namespace gcn
{
    class Graphics;

    /**
     * Interface for a font.
     *
     * @see ImageFont
     */
    class Font
    {
    public:

        /**
         * Destructor.
         */
        virtual ~Font(){ }

        /**
         * Gets the width of a string. The width of a string is not necesserily
         * the sum of all the widths of it's glyphs.
         *
         * @param text The string to return the width of.
         * @return The width of a string.
         */
        virtual int getWidth(const std::string& text) const = 0;

        /**
         * Gets the height of the glyphs in the font.
         *
         * @return The height of the glyphs int the font.
         */
        virtual int getHeight() const = 0;

        /**
         * Gets a string index in a string providing an x coordinate.
         * Used to retrive a string index (for a character in a
         * string) at a certain x position. It is especially useful
         * when a mouse clicks in a TextField and you want to know which
         * character was clicked.
         *
         * @return A string index in a string providing an x coordinate.
         */
        virtual int getStringIndexAt(const std::string& text, int x) const;

        /**
         * Draws a string.
         *
         * NOTE: You normally won't use this function to draw text since
         *       Graphics contains better functions for drawing text.
         *
         * @param graphics A Graphics object to use for drawing.
         * @param text The string to draw.
         * @param x The x coordinate where to draw the string.
         * @param y The y coordinate where to draw the string.
         */
        virtual void drawString(Graphics* graphics, const std::string& text,
                                int x, int y) = 0;
    };
}

