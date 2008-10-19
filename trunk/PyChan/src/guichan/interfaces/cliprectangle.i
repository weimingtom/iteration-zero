%{
#include <guichan/rectangle.hpp>
%}
%include "rectangle.i"

namespace gcn
{
    /**
     * A rectangle used when dealing with clipping. A clip rectangle is
     * a regular rectangle extended with variables for x offsets and y 
     * offsets. The offsets are used for calculations from relative
     * screen coordinates to actual screen coordinates.
     */
    class ClipRectangle : public Rectangle
    {
    public:

        /**
         * Constructor.
         */
        ClipRectangle();

        /**
         * Constructor.
         *
         * @param x The rectangle x coordinate.
         * @param y The rectangle y coordinate.
         * @param width The rectangle width.
         * @param height The rectangle height.
         * @param xOffset The offset of the x coordinate. Used to for
         *                calculating the actual screen coordinate from
         *                the relative screen coordinate.
         * @param yOffset The offset of the y coordinate. Used to for
         *                calculating the actual screen coordinate from
         *                the relative screen coordinate.
         */
        ClipRectangle(int x, 
                      int y, 
                      int width, 
                      int height,
                      int xOffset, 
                      int yOffset);

        /**
         * Copy constructor. Copies x, y, width and height 
         * field from a rectangle to a clip rectangle.
         *
         * @param other The rectangle to copy data from.
         * @returns A clip rectangle with data copyied from a rectangle.
         */
/*         const ClipRectangle& operator=(const Rectangle& other); */

        /**
         * Holds the x offset of the x coordinate.
         */
        int xOffset;

        /**
         * Holds the y offset of the y coordinate.
         */
        int yOffset;
    };
}

