%{
#include <guichan/platform.hpp>
#include <iostream>
%}

namespace gcn
{
    /**
     * Represents a rectangle.
     *
     * @since 0.1.0
     */
    class Rectangle
    {
    public:

        /**
         * Constructor. The default rectangle is an empty rectangle
         * at the coordinates (0,0).
         */
        Rectangle();

        /**
         * Constructor.
         *
         * @param x The x coordinate of the rectangle.
         * @param y The y coordinate of the rectangle.
         * @param width The width of the rectangle.
         * @param height The height of the rectangle.
         * @since 0.1.0
         */
        Rectangle(int x, int y, int width, int height);

        /**
         * Sets the dimension of a rectangle.
         *
         * @param x The x coordinate of the rectangle.
         * @param y The y coordinate of the rectangle.
         * @param width The width of the rectangle.
         * @param height The height of the rectangle.
         * @since 0.1.0
         */
        void setAll(int x, int y, int width, int height);

        /**
         * Checks if another rectangle intersects with the rectangle.
         *
         * @param rectangle Another rectangle to check for intersection.
         * @return True if the rectangles intersect, false otherwise.
         * @since 0.1.0
         */
        bool isIntersecting(const Rectangle& rectangle) const;

        /**
         * Checks the rectangle contains a point.
         *
         * @param x The x coordinate of the point.
         * @param y The y coordinate of the point.
         * @return True if the rectangle contains the point,
         *         false otherwise.
         * @since 0.9.0
         */
        bool isContaining(int x, int y) const;

        /**
         * Checks if the rectangle contains a rectangle.
         *
         * @param other The rectangle to check.
         * @return True if the rectangle contains the rectangle,
         *         false otherwise.
         * @since 0.9.0
         */
        bool isContaining(const Rectangle& other) const;

        /**
         * Checks whether the rectangle is empty or not. A rectangle
         * is considered empty when it's width or height is either
         * zero or negative.
         *
         * @return True if the rectangle is empty, false otherwise.
         */
        bool isEmpty() const;

        /**
         * Adds a rectangle to this rectangle. The resulting rectangle
         * is the union of the two rectangles.
         *
         * @param rh The rectangle to add.
         * @return The union of the two rectangles.
         */
        Rectangle operator+(const Rectangle& rh) const;

        /**
         * Adds a rectangle to this rectangle. This rectangle will be
         * the union of the two rectangles.
         *
         * @param rh The rectangle to add.
         * @return A reference to this rectangle.
         */
        const Rectangle& operator+=(const Rectangle& rh);

        /**
         * Gets the intersection between two rectangles.
         *
         * @param rh The rectangle to calculate the intersection with.
         * @return The intersection between two rectangles.
         */
        Rectangle intersection(const Rectangle& rh) const;

        /**
         * Holds the x coordinate of the rectangle.
         */
        int x;

        /**
         * Holds the x coordinate of the rectangle.
         */
        int y;

        /**
         * Holds the width of the rectangle.
         */
        int width;

        /**
         * Holds the height of the rectangle.
         */
        int height;
    };
}

