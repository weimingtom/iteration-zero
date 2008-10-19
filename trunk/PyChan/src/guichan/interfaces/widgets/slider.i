
%{
#include <guichan/keylistener.hpp>
#include <guichan/mouselistener.hpp>
#include <guichan/widget.hpp>
%}

namespace gcn
{
    /**
     * An implementation of a slider where a user can select different values by
     * sliding between a start value and an end value of a scale. 
     *
     * If the selected value is changed an action event will be sent to all
     * action listeners of the slider.
     */
    class  Slider :
        public Widget,
        public MouseListener,
        public KeyListener
    {
    public:

        /**
         * Draw orientations for the slider. A slider can be drawn vertically or
         * horizontally.
         */
        enum Orientation
        {
            Horizontal = 0,
            Vertical
        };

        /**
         * Constructor. The default start value of the slider scale is zero.
         *
         * @param scaleEnd The end value of the slider scale.
         */
        Slider(double scaleEnd = 1.0);

        /**
         * Constructor.
         *
         * @param scaleStart The start value of the slider scale.
         * @param scaleEnd The end value of the slider scale.
         */
        Slider(double scaleStart, double scaleEnd);

        /**
         * Destructor.
         */
        virtual ~Slider() { }

        /**
         * Sets the scale of the slider.
         *
         * @param scaleStart The start value of the scale.
         * @param scaleEnd tThe end of value the scale.
         * @see getScaleStart, getScaleEnd
         */
        void setScale(double scaleStart, double scaleEnd);

        /**
         * Gets the start value of the scale.
         *
         * @return The start value of the scale.
         * @see setScaleStart, setScale
         */
        double getScaleStart() const;

        /**
         * Sets the start value of the scale.
         *
         * @param scaleStart The start value of the scale.
         * @see getScaleStart
         */
        void setScaleStart(double scaleStart);

        /**
         * Gets the end value of the scale.
         *
         * @return The end value of the scale.
         * @see setScaleEnd, setScale
         */
        double getScaleEnd() const;

        /**
         * Sets the end value of the scale.
         *
         * @param scaleEnd The end value of the scale.
         * @see getScaleEnd
         */
        void setScaleEnd(double scaleEnd);

        /**
         * Gets the current selected value.
         *
         * @return The current selected value.
         * @see setValue
         */
        double getValue() const;

        /**
         * Sets the current selected value.
         *
         * @param value The current selected value.
         * @see getValue
         */
        void setValue(double value);

        /**
         * Sets the length of the marker.
         *
         * @param length The length for the marker.
         * @see getMarkerLength
         */
        void setMarkerLength(int length);

        /**
         * Gets the length of the marker.
         *
         * @return The length of the marker.
         * @see setMarkerLength
         */
        int getMarkerLength() const;

        /**
         * Sets the orientation of the slider. A slider can be drawn vertically
         * or horizontally.
         *
         * @param orientation The orientation of the slider.
         * @see getOrientation
         */
        void setOrientation(Orientation orientation);

        /**
         * Gets the orientation of the slider. A slider can be drawn vertically
         * or horizontally.
         *
         * @return The orientation of the slider.
         * @see setOrientation
         */
        Orientation getOrientation() const;

        /**
         * Sets the step length. The step length is used when the keys LEFT 
         * and RIGHT are pressed to step in the scale.
         *
         * @param length The step length.
         * @see getStepLength
         */
        void setStepLength(double length);

        /**
         * Gets the step length. The step length is used when the keys LEFT 
         * and RIGHT are pressed to step in the scale.
         *
         * @return the step length.
         * @see setStepLength
         */
        double getStepLength() const;


        // Inherited from Widget

        virtual void draw(Graphics* graphics);


        // Inherited from MouseListener.

        virtual void mousePressed(MouseEvent& mouseEvent);

        virtual void mouseDragged(MouseEvent& mouseEvent);

        virtual void mouseWheelMovedUp(MouseEvent& mouseEvent);

        virtual void mouseWheelMovedDown(MouseEvent& mouseEvent);


        // Inherited from KeyListener

        virtual void keyPressed(KeyEvent& keyEvent);

    };
}
