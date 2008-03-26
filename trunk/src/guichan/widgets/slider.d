/*      _______   __   __   __   ______   __   __   _______   __   __
 *     / _____/\ / /\ / /\ / /\ / ____/\ / /\ / /\ / ___  /\ /  |\/ /\
 *    / /\____\// / // / // / // /\___\// /_// / // /\_/ / // , |/ / /
 *   / / /__   / / // / // / // / /    / ___  / // ___  / // /| ' / /
 *  / /_// /\ / /_// / // / // /_/_   / / // / // /\_/ / // / |  / /
 * /______/ //______/ //_/ //_____/\ /_/ //_/ //_/ //_/ //_/ /|_/ /
 * \______\/ \______\/ \_\/ \_____\/ \_\/ \_\/ \_\/ \_\/ \_\/ \_\/
 *
 * Copyright (c) 2004 - 2008 Olof Naess�n and Per Larsson
 *
 *
 * Per Larsson a.k.a finalman
 * Olof Naess�n a.k.a jansem/yakslem
 *
 * Visit: http://guichan.sourceforge.net
 *
 * License: (BSD)
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of Guichan nor the names of its contributors may
 *    be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * For comments regarding functions please see the header file.
 */

module guichan.widgets.slider;

import std.stdio : writefln;

private import guichan.all;

/**
  * Draw orientations for the slider. A slider can be drawn vertically or
  * horizontally.
  */
enum Orientation
{
    HORIZONTAL = 0,
    VERTICAL
}

class Slider : Widget, MouseListener, KeyListener
{
    this(double scaleEnd)
    {
        mDragged = false;

        mScaleStart = 0;
        mScaleEnd = scaleEnd;

        setFocusable(true);
        setFrameSize(1);
        setOrientation(Orientation.HORIZONTAL);
        setValue(0);
        setStepLength(scaleEnd / 10);
        setMarkerLength(10);

        addMouseListener(this);
        addKeyListener(this);
    }

    this(double scaleStart, double scaleEnd)
    {
        mDragged = false;

        mScaleStart = scaleStart;
        mScaleEnd = scaleEnd;

        setFocusable(true);
        setFrameSize(1);
        setOrientation(Orientation.HORIZONTAL);
        setValue(scaleStart);
        setStepLength((scaleEnd  - scaleStart)/ 10);
        setMarkerLength(10);

        addMouseListener(this);
        addKeyListener(this);
    }

    void setScale(double scaleStart, double scaleEnd)
    {
        mScaleStart = scaleStart;
        mScaleEnd = scaleEnd;
    }

    double getScaleStart()
    {
        return mScaleStart;
    }

    void setScaleStart(double scaleStart)
    {
        mScaleStart = scaleStart;
    }

    double getScaleEnd()
    {
        return mScaleEnd;
    }

    void setScaleEnd(double scaleEnd)
    {
        mScaleEnd = scaleEnd;
    }

    void draw(Graphics graphics)
    {
        graphics.setColor(getFrameColor());
        graphics.fillRectangle(Rectangle(0,0,getWidth(),getHeight()));

        drawMarker(graphics);
    }

    void drawMarker(Graphics graphics)
    {
        graphics.setColor(getForegroundColor());

        if (getOrientation() == Orientation.HORIZONTAL)
        {
            int v = getMarkerPosition();
            graphics.fillRectangle(Rectangle(v + 1, 1, getMarkerLength() - 2, getHeight() - 2));
//             graphics.setColor(highlightColor);
//             graphics.drawLine(v, 0, v + getMarkerLength() - 1,0);
//             graphics.drawLine(v, 0, v, getHeight() - 1);
//             graphics.setColor(shadowColor);
//             graphics.drawLine(v + getMarkerLength() - 1, 1, v + getMarkerLength() - 1, getHeight() - 1);
//             graphics.drawLine(v + 1, getHeight() - 1, v + getMarkerLength() - 1, getHeight() - 1);

            if (isFocused())
            {
                graphics.setColor(getForegroundColor());
                graphics.drawRectangle(Rectangle(v + 2, 2, getMarkerLength() - 4, getHeight() - 4));
            }
        }
        else
        {
            int v = (getHeight() - getMarkerLength()) - getMarkerPosition();
            graphics.fillRectangle(Rectangle(1, v + 1, getWidth() - 2, getMarkerLength() - 2));
//             graphics.setColor(highlightColor);
//             graphics.drawLine(0, v, 0, v + getMarkerLength() - 1);
//             graphics.drawLine(0, v, getWidth() - 1, v);
//             graphics.setColor(shadowColor);
//             graphics.drawLine(1, v + getMarkerLength() - 1, getWidth() - 1, v + getMarkerLength() - 1);
//             graphics.drawLine(getWidth() - 1, v + 1, getWidth() - 1, v + getMarkerLength() - 1);

            if (isFocused())
            {
                graphics.setColor(getForegroundColor());
                graphics.drawRectangle(Rectangle(2, v + 2, getWidth() - 4, getMarkerLength() - 4));
            }
        }
    }

    void mousePressed(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT
            && mouseEvent.getX() >= 0
            && mouseEvent.getX() <= getWidth()
            && mouseEvent.getY() >= 0
            && mouseEvent.getY() <= getHeight())
        {
            if (getOrientation() == Orientation.HORIZONTAL)
            {
                setValue(markerPositionToValue(mouseEvent.getX() - getMarkerLength() / 2));
            }
            else
            {
                setValue(markerPositionToValue(getHeight() - mouseEvent.getY() - getMarkerLength() / 2));
            }

            distributeActionEvent();
        }
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        if (getOrientation() == Orientation.HORIZONTAL)
        {
            setValue(markerPositionToValue(mouseEvent.getX() - getMarkerLength() / 2));
        }
        else
        {
            setValue(markerPositionToValue(getHeight() - mouseEvent.getY() - getMarkerLength() / 2));
        }

        distributeActionEvent();

        mouseEvent.consume();
    }

    void setValue(double value)
    {
        if (value > getScaleEnd())
        {
            mValue = getScaleEnd();
            return;
        }

        if (value < getScaleStart())
        {
            mValue = getScaleStart();
            return;
        }

        mValue = value;
    }

    double getValue()
    {
        return mValue;
    }

    int getMarkerLength()
    {
        return mMarkerLength;
    }

    void setMarkerLength(int length)
    {
        mMarkerLength = length;
    }

    void keyPressed(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if (getOrientation() == Orientation.HORIZONTAL)
        {
            if (key.getValue() == Key.RIGHT)
            {
                setValue(getValue() + getStepLength());
                distributeActionEvent();
                keyEvent.consume();
            }
            else if (key.getValue() == Key.LEFT)
            {
                setValue(getValue() - getStepLength());
                distributeActionEvent();
                keyEvent.consume();
            }            
        }
        else
        {
            if (key.getValue() == Key.UP)
            {
                setValue(getValue() + getStepLength());
                distributeActionEvent();
                keyEvent.consume();
            }
            else if (key.getValue() == Key.DOWN)
            {
                setValue(getValue() - getStepLength());
                distributeActionEvent();
                keyEvent.consume();
            }
        }
    }

    void keyReleased(KeyEvent keyEvent)
    {
    }

    void setOrientation(Orientation orientation)
    {
        mOrientation = orientation;
    }

    Orientation getOrientation()
    {
        return mOrientation;
    }

    double markerPositionToValue(int v)
    {
        int w;
        if (getOrientation() == Orientation.HORIZONTAL)
        {
            w = getWidth();
        }
        else
        {
            w = getHeight();
        }

        double pos = v / (cast(double)w - getMarkerLength());
        return (1.0 - pos) * getScaleStart() + pos * getScaleEnd();

    }

    int valueToMarkerPosition(double value)
    {
        int v;
        if (getOrientation() == Orientation.HORIZONTAL)
        {
            v = getWidth();
        }
        else
        {
            v = getHeight();
        }

        int w =  cast(int)((v - getMarkerLength())
                       * (value  - getScaleStart())
                       / (getScaleEnd() - getScaleStart()));

        if (w < 0)
        {
            return 0;
        }

        if (w > v - getMarkerLength())
        {
            return v - getMarkerLength();
        }

        return w;
    }

    void setStepLength(double length)
    {
        mStepLength = length;
    }

    double getStepLength()
    {
        return mStepLength;
    }

    int getMarkerPosition()
    {
        return valueToMarkerPosition(getValue());
    }

    void mouseEntered(MouseEvent mouseEvent)
    {
//         writefln("mouseEntered");
    }

    void mouseExited(MouseEvent mouseEvent)
    {
//         writefln("mouseExited");
    }

    void mouseClicked(MouseEvent mouseEvent){
//       writefln("mouseClicked");
    }
    void mouseReleased(MouseEvent mouseEvent){
//       writefln("mouseClicked");
    }
    void mouseMoved(MouseEvent mouseEvent){
//       writefln("mouseMoved");
    }

    void mouseWheelMovedUp(MouseEvent mouseEvent)
    {
        setValue(getValue() + getStepLength());
//         distributeActionEvent();

        mouseEvent.consume();
    }

    void mouseWheelMovedDown(MouseEvent mouseEvent)
    {
        setValue(getValue() - getStepLength());
//         distributeActionEvent();

        mouseEvent.consume();
    }

  protected:
    /**
      * True if the slider is dragged, false otherwise.
      */
    bool mDragged;

    /**
      * Holds the current selected value.
      */
    double mValue;

    /**
      * Holds the step length. The step length is used when the keys LEFT 
      * and RIGHT are pressed to step in the scale.
      */
    double mStepLength;

    /**
      * Holds the length of the marker.
      */
    int mMarkerLength;

    /**
      * Holds the start value of the scale.
      */
    double mScaleStart;

    /**
      * Holds the end value of the scale.
      */
    double mScaleEnd;

    /**
      * Holds the orientation of the slider. A slider can be drawn 
      * vertically or horizontally.
      */
    Orientation mOrientation;
}
