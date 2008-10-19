%module guichan

%{
#include <iostream>
%}

namespace gcn
{
    /**
     * Represents a color with red, green, blue and alpha components.
     */
    class Color
    {
    public:

        /**
         * Constructor. Initializes the color to black.
         */
        Color();

        /**
         * Constructor. Constructs a color from the bytes in an integer. 
         * Call it with a hexadecimal constant for HTML-style color representation.
         * The alpha component is 255 by default.
         *
         * EXAMPLE: Color(0xff50a0) constructs a very nice pinkish color.
         *
         * NOTE: Because of this constructor, integers will be automatically
         *       casted to a color by your compiler.
         *
         * @param color The color to initialise the object with.
         */
        Color(int color);

        /**
         * Constructor. The default alpha value is 255.
         *
         * @param r Red color component (range 0-255).
         * @param g Green color component  (range 0-255).
         * @param b Blue color component (range 0-255).
         * @param a Alpha, used for transparency. A value of 0 means
         *          totaly transparent, 255 is totaly opaque.
         */
        Color(int r, int g, int b, int a = 255);

        /**
         * Adds the RGB values of two colors together. The values will be
         * clamped if they go out of range. 
         *
         * WARNING: This function will reset the alpha value of the
         *          returned color to 255.
         *
         * @param color A color to add to this color.
         * @return The added colors with an alpha value set to 255.
         */
        Color operator+(const Color& color) const;

        /**
         * Subtracts the RGB values of one color from another.
         * The values will be clamped if they go out of range.
         *
         * WARNING: This function will reset the alpha value of the
         *          returned color to 255.
         *
         * @param color A color to subtract from this color.
         * @return The subtracted colors with an alpha value set to 255.
         */
        Color operator-(const Color& color) const;

        /**
         * Multiplies the RGB values of a color with a float value.
         * The values will be clamped if they go out of range.
         *
         * @param value The value to multiply the color with.
         * @return The multiplied colors. The alpha value will, unlike
         *         the add and subtract operations, be multiplied as
         *         well.
         */
        Color operator*(float value) const;

        /**
         * Compares two colors.
         *
         * @return True if the two colors have the same RGBA components
         *         false otherwise.
         */
        bool operator==(const Color& color) const;

        /**
         * Compares two colors.
         *
         * @return True if the two colors have different RGBA components,
         *         false otherwise.
         */
        bool operator!=(const Color& color) const;


        /**
         * Holds the red color component (range 0-255).
         */
        int r;

        /**
         *  Holds the green color component (range 0-255).
         */
        int g;

        /**
         *  Holds the blue color component (range 0-255).
         */
        int b;

        /**
         * Holds the alpha color component. A value of 0 means totally
         * transparent while a value of 255 is considered opaque.
         */
        int a;
    };
}


