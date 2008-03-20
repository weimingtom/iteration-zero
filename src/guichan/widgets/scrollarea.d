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

module guichan.widgets.scrollarea;

import guichan.all;
import guichan.basiccontainer;


/**
  * Scrollpolicies for the horizontal and vertical scrollbar.
  * The policies are:
  *
  * SHOW_ALWAYS - Always show the scrollbars no matter what.
  * SHOW_NEVER  - Never show the scrollbars no matter waht.
  * SHOW_AUTO   - Show the scrollbars only when needed. That is if the
  *               content grows larger then the ScrollArea.
  */
enum ScrollPolicy
{
    SHOW_ALWAYS = 0,
    SHOW_NEVER,
    SHOW_AUTO
}

/**
  * Implementation if a scrollable area used to view widgets larger than the scroll area.
  * A scroll area can be customized to always show scroll bars or to show them only when
  * necessary.
  */
class ScrollArea: BasicContainer
{
public:


    /**
      * Constructor.
      */
    this()
    {
        mVScroll = 0;
        mHScroll = 0;
        mHPolicy = ScrollPolicy.SHOW_AUTO;
        mVPolicy = ScrollPolicy.SHOW_AUTO;
        mScrollbarWidth = 12;
        mUpButtonPressed = false;
        mDownButtonPressed = false;
        mLeftButtonPressed = false;
        mRightButtonPressed = false;
        mUpButtonScrollAmount = 10;
        mDownButtonScrollAmount = 10;
        mLeftButtonScrollAmount = 10;
        mRightButtonScrollAmount = 10;
        mIsVerticalMarkerDragged = false;
        mIsHorizontalMarkerDragged =false;

        addMouseListener(this);
    }

    /**
      * Constructor.
      *
      * @param content The content of the scroll area.
      */
    this(Widget content)
    {
        mVScroll = 0;
        mHScroll = 0;
        mHPolicy = ScrollPolicy.SHOW_AUTO;
        mVPolicy = ScrollPolicy.SHOW_AUTO;
        mScrollbarWidth = 12;
        mUpButtonPressed = false;
        mDownButtonPressed = false;
        mLeftButtonPressed = false;
        mRightButtonPressed = false;
        mUpButtonScrollAmount = 10;
        mDownButtonScrollAmount = 10;
        mLeftButtonScrollAmount = 10;
        mRightButtonScrollAmount = 10;
        mIsVerticalMarkerDragged = false;
        mIsHorizontalMarkerDragged =false;

        setContent(content);
        addMouseListener(this);
    }

    /**
      * Constructor.
      *
      * @param content The content of the scroll area.
      * @param hPolicy The policy for the horizontal scrollbar. See enum with
      *                policies.
      * @param vPolicy The policy for the vertical scrollbar. See enum with
      *                policies.
      */
    this(Widget content, ScrollPolicy hPolicy, ScrollPolicy vPolicy)
    {
        mVScroll = 0;
        mHScroll = 0;
        mHPolicy = hPolicy;
        mVPolicy = vPolicy;
        mScrollbarWidth = 12;
        mUpButtonPressed = false;
        mDownButtonPressed = false;
        mLeftButtonPressed = false;
        mRightButtonPressed = false;
        mUpButtonScrollAmount = 10;
        mDownButtonScrollAmount = 10;
        mLeftButtonScrollAmount = 10;
        mRightButtonScrollAmount = 10;
        mIsVerticalMarkerDragged = false;
        mIsHorizontalMarkerDragged =false;

        setContent(content);
        addMouseListener(this);
    }

    /**
      * Sets the content.
      *
      * @param widget The content of the scroll area.
      */
    void setContent(Widget widget)
    {
        if (widget !is null)
        {
            clear();
            add(widget);
            widget.setPosition(0,0);
        }
        else
        {
            clear();
        }

        checkPolicies();
    }

    /**
      * Gets the content.
      *
      * @return The content of the scroll area.
      */
    Widget getContent()
    {
        if (mWidgets.length > 0)
        {
            return mWidgets[0];
        }

        return null;
    }

    /**
      * Sets the horizontal scrollbar policy. See enum with policies.
      *
      * @param hPolicy The policy for the horizontal scrollbar.
      * @see getHorizontalScrollPolicy
      */
    void setHorizontalScrollPolicy(ScrollPolicy hPolicy)
    {
        mHPolicy = hPolicy;
        checkPolicies();
    }


    /**
      * Gets the horizontal scrollbar policy. See enum with policies.
      *
      * @return The policy for the horizontal scrollbar policy.
      * @see setHorizontalScrollPolicy, setScrollPolicy
      */
    ScrollPolicy getHorizontalScrollPolicy()
    {
        return mHPolicy;
    }


    /**
      * Sets the vertical scrollbar policy. See enum with policies.
      *
      * @param vPolicy The policy for the vertical scrollbar.
      * @see getVerticalScrollPolicy
      */
    void setVerticalScrollPolicy(ScrollPolicy vPolicy)
   {
        mVPolicy = vPolicy;
        checkPolicies();
    }


    /**
      * Gets the vertical scrollbar policy. See enum with policies.
      *
      * @return The policy for the vertical scrollbar.
      * @see setVerticalScrollPolicy, setScrollPolicy
      */
    ScrollPolicy getVerticalScrollPolicy()
    {
        return mVPolicy;
    }

    /**
      * Sets the horizontal and vertical scrollbar policy.
      *
      * @param hPolicy The policy for the horizontal scrollbar.
      * @param vPolicy The policy for the vertical scrollbar.
      * @see getVerticalScrollPolicy, getHorizontalScrollPolicy
      */
    void setScrollPolicy(ScrollPolicy hPolicy, ScrollPolicy vPolicy)
    {
        mHPolicy = hPolicy;
        mVPolicy = vPolicy;
        checkPolicies();
    }

    /**
      * Sets the amount to scroll vertically.
      *
      * @param vScroll The amount to scroll.
      * @see getVerticalScrollAmount
      */
    void setVerticalScrollAmount(int vScroll)
    {
        int max = getVerticalMaxScroll();

        mVScroll = vScroll;

        if (vScroll > max)
        {
            mVScroll = max;
        }

        if (vScroll < 0)
        {
            mVScroll = 0;
        }
    }

    /**
      * Gets the amount that is scrolled vertically.
      *
      * @return The scroll amount on vertical scroll.
      * @see setVerticalScrollAmount, setScrollAmount
      */
    int getVerticalScrollAmount()
    {
        return mVScroll;
    }


    /**
      * Sets the amount to scroll horizontally.
      *
      * @param hScroll The amount to scroll.
      * @see getHorizontalScrollAmount
      */
    void setHorizontalScrollAmount(int hScroll)
    {
        int max = getHorizontalMaxScroll();

        mHScroll = hScroll;

        if (hScroll > max)
        {
            mHScroll = max;
        }
        else if (hScroll < 0)
        {
            mHScroll = 0;
        }
    }

    /**
      * Gets the amount that is scrolled horizontally.
      *
      * @return The scroll amount on horizontal scroll.
      * @see setHorizontalScrollAmount, setScrollAmount
      */
    int getHorizontalScrollAmount()
    {
        return mHScroll;
    }

    /**
      * Sets the amount to scroll horizontally and vertically.
      *
      * @param hScroll The amount to scroll on horizontal scroll.
      * @param vScroll The amount to scroll on vertical scroll.
      * @see getHorizontalScrollAmount, getVerticalScrollAmount
      */
    void setScrollAmount(int hScroll, int vScroll)
    {
        setHorizontalScrollAmount(hScroll);
        setVerticalScrollAmount(vScroll);
    }

    /**
      * Gets the maximum amount of horizontal scroll.
      *
      * @return The horizontal max scroll.
      */
    int getHorizontalMaxScroll()
    {
        checkPolicies();

        if (getContent() is null)
        {
            return 0;
        }

        int value = getContent().getWidth() - getChildrenArea().width +
            2 * getContent().getFrameSize();

        if (value < 0)
        {
            return 0;
        }

        return value;
    }

    /**
      * Gets the maximum amount of vertical scroll.
      *
      * @return The vertical max scroll.
      */
    int getVerticalMaxScroll()
    {
        checkPolicies();

        if (getContent() is null)
        {
            return 0;
        }

        int value;

        value = getContent().getHeight() - getChildrenArea().height +
            2 * getContent().getFrameSize();

        if (value < 0)
        {
            return 0;
        }

        return value;
    }

    /**
      * Sets the width of the scroll bars.
      *
      * @param width The width of the scroll bars.
      * @see getScrollbarWidth
      */
    void setScrollbarWidth(int width)
    {
        if (width > 0)
        {
            mScrollbarWidth = width;
        }
        else
        {
            throw new GCN_Exception("Width should be greater then 0.");
        }
    }

    /**
      * Gets the width of the scroll bars.
      *
      * @return the width of the ScrollBar.
      * @see setScrollbarWidth
      */
    int getScrollbarWidth()
    {
        return mScrollbarWidth;
    }

 
    /**
      * Sets the amount to scroll in pixels when the left scroll button is
      * pushed.
      *
      * @param amount The amount to scroll in pixels. 
      * @see getLeftButtonScrollAmount
      */
    void setLeftButtonScrollAmount(int amount)
    {
        mLeftButtonScrollAmount = amount;
    }


    /**
      * Sets the amount to scroll in pixels when the right scroll button is
      * pushed.
      *
      * @param amount The amount to scroll in pixels.
      * @see getRightButtonScrollAmount
      */
    void setRightButtonScrollAmount(int amount)
    {
        mRightButtonScrollAmount = amount;
    }


    /**
      * Sets the amount to scroll in pixels when the up scroll button is
      * pushed.
      *
      * @param amount The amount to scroll in pixels.
      * @see getUpButtonScrollAmount
      */
    void setUpButtonScrollAmount(int amount)
    {
        mUpButtonScrollAmount = amount;
    }

    /**
      * Sets the amount to scroll in pixels when the down scroll button is
      * pushed.
      *
      * @param amount The amount to scroll in pixels.
      * @see getDownButtonScrollAmount
      */
    void setDownButtonScrollAmount(int amount)
    {
        mDownButtonScrollAmount = amount;
    }

    /**
      * Gets the amount to scroll in pixels when the left scroll button is
      * pushed.
      *
      * @return The amount to scroll in pixels.
      * @see setLeftButtonScrollAmount
      */
    int getLeftButtonScrollAmount()
    {
        return mLeftButtonScrollAmount;
    }

    /**
      * Gets the amount to scroll in pixels when the right scroll button is
      * pushed.
      *
      * @return The amount to scroll in pixels.
      * @see setRightButtonScrollAmount
      */
    int getRightButtonScrollAmount()
    {
        return mRightButtonScrollAmount;
    }

    /**
      * Gets the amount to scroll in pixels when the up scroll button is
      * pushed.
      *
      * @return The amount to scroll in pixels.
      * @see setUpButtonScrollAmount
      */
    int getUpButtonScrollAmount()
    {
        return mUpButtonScrollAmount;
    }

    /**
      * Gets the amount to scroll in pixels when the down scroll button is
      * pushed.
      *
      * @return The amount to scroll in pixels.
      * @see setDownButtonScrollAmount
      */
    int getDownButtonScrollAmount()
    {
        return mDownButtonScrollAmount;
    }


    // Inherited from BasicContainer

    void showWidgetPart(Widget widget, Rectangle area)
    {
        if (widget !is getContent())
        {
            throw new GCN_Exception("Widget not content widget");
        }

        BasicContainer.showWidgetPart(widget, area);

        setHorizontalScrollAmount(getContent().getFrameSize() - getContent().getX());
        setVerticalScrollAmount(getContent().getFrameSize() - getContent().getY());
    }


    Rectangle getChildrenArea()
    {
        if (mVBarVisible && mHBarVisible)
        {
            return Rectangle(0, 0, getWidth() - mScrollbarWidth,
                             getHeight() - mScrollbarWidth);
        }

        if (mVBarVisible)
        {
            return Rectangle(0, 0, getWidth() - mScrollbarWidth, getHeight());
        }

        if (mHBarVisible)
        {
            return Rectangle(0, 0, getWidth(), getHeight() - mScrollbarWidth);
        }

        return Rectangle(0, 0, getWidth(), getHeight());
    }


    Widget getWidgetAt(int x, int y)
    {
        if (getChildrenArea().isPointInRect(x, y))
        {
            return getContent();
        }

        return null;
    }


    // Inherited from Widget

    void draw(Graphics graphics)
    {
        drawBackground(graphics);

        if (mVBarVisible)
        {
            drawUpButton(graphics);
            drawDownButton(graphics);
            drawVBar(graphics);
            drawVMarker(graphics);
        }

        if (mHBarVisible)
        {
            drawLeftButton(graphics);
            drawRightButton(graphics);
            drawHBar(graphics);
            drawHMarker(graphics);
        }

        if (mHBarVisible && mVBarVisible)
        {
            graphics.setColor(getFrameColor());
            graphics.fillRectangle(Rectangle(getWidth() - mScrollbarWidth,
                                              getHeight() - mScrollbarWidth,
                                              mScrollbarWidth,
                                              mScrollbarWidth));
        }

        drawChildren(graphics);
    }

    void logic()
    {
        checkPolicies();

        setVerticalScrollAmount(getVerticalScrollAmount());
        setHorizontalScrollAmount(getHorizontalScrollAmount());

        if (getContent() !is null)
        {
            getContent().setPosition(-mHScroll + getContent().getFrameSize(),
                                      -mVScroll + getContent().getFrameSize());
            getContent().logic();
        }
    }

    void setWidth(int width)
    {
        super.setWidth(width);
        checkPolicies();
    }


    void setHeight(int height)
    {
        super.setHeight(height);
        checkPolicies();
    }

    void setDimension(Rectangle dimension)
    {
        super.setDimension(dimension);
        checkPolicies();
    }

    // Inherited from MouseListener

    void mousePressed(MouseEvent mouseEvent)
    {
        int x = mouseEvent.getX();
        int y = mouseEvent.getY();

        if (getUpButtonDimension().isPointInRect(x, y))
        {
            setVerticalScrollAmount(getVerticalScrollAmount()
                                    - mUpButtonScrollAmount);
            mUpButtonPressed = true;
        }
        else if (getDownButtonDimension().isPointInRect(x, y))
        {
            setVerticalScrollAmount(getVerticalScrollAmount()
                                    + mDownButtonScrollAmount);
            mDownButtonPressed = true;
        }
        else if (getLeftButtonDimension().isPointInRect(x, y))
        {
            setHorizontalScrollAmount(getHorizontalScrollAmount()
                                      - mLeftButtonScrollAmount);
            mLeftButtonPressed = true;
        }
        else if (getRightButtonDimension().isPointInRect(x, y))
        {
            setHorizontalScrollAmount(getHorizontalScrollAmount()
                                      + mRightButtonScrollAmount);
            mRightButtonPressed = true;
        }
        else if (getVerticalMarkerDimension().isPointInRect(x, y))
        {
            mIsHorizontalMarkerDragged = false;
            mIsVerticalMarkerDragged = true;

            mVerticalMarkerDragOffset = y - getVerticalMarkerDimension().y;
        }
        else if (getVerticalBarDimension().isPointInRect(x,y))
        {
            if (y < getVerticalMarkerDimension().y)
            {
                setVerticalScrollAmount(getVerticalScrollAmount()
                                        - cast(int)(getChildrenArea().height * 0.95));
            }
            else
            {
                setVerticalScrollAmount(getVerticalScrollAmount()
                                        + cast(int)(getChildrenArea().height * 0.95));
            }
        }
        else if (getHorizontalMarkerDimension().isPointInRect(x, y))
        {
            mIsHorizontalMarkerDragged = true;
            mIsVerticalMarkerDragged = false;

            mHorizontalMarkerDragOffset = x - getHorizontalMarkerDimension().x;
        }
        else if (getHorizontalBarDimension().isPointInRect(x,y))
        {
            if (x < getHorizontalMarkerDimension().x)
            {
                setHorizontalScrollAmount(getHorizontalScrollAmount()
                                          - cast(int)(getChildrenArea().width * 0.95));
            }
            else
            {
                setHorizontalScrollAmount(getHorizontalScrollAmount()
                                          + cast(int)(getChildrenArea().width * 0.95));
            }
        }
    }

    void mouseReleased(MouseEvent mouseEvent)
    {
        mUpButtonPressed = false;
        mDownButtonPressed = false;
        mLeftButtonPressed = false;
        mRightButtonPressed = false;
        mIsHorizontalMarkerDragged = false;
        mIsVerticalMarkerDragged = false;

        mouseEvent.consume();
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        if (mIsVerticalMarkerDragged)
        {
            int pos = mouseEvent.getY() - getVerticalBarDimension().y  - mVerticalMarkerDragOffset;
            int length = getVerticalMarkerDimension().height;

            Rectangle barDim = getVerticalBarDimension();

            if ((barDim.height - length) > 0)
            {
                setVerticalScrollAmount((getVerticalMaxScroll() * pos)
                                         / (barDim.height - length));
            }
            else
            {
                setVerticalScrollAmount(0);
            }
        }

        if (mIsHorizontalMarkerDragged)
        {
            int pos = mouseEvent.getX() - getHorizontalBarDimension().x  - mHorizontalMarkerDragOffset;
            int length = getHorizontalMarkerDimension().width;

            Rectangle barDim = getHorizontalBarDimension();

            if ((barDim.width - length) > 0)
            {
                setHorizontalScrollAmount((getHorizontalMaxScroll() * pos)
                                          / (barDim.width - length));
            }
            else
            {
                setHorizontalScrollAmount(0);
            }
        }

        mouseEvent.consume();
    }

    void mouseWheelMovedUp(MouseEvent mouseEvent)
    {
        if (mouseEvent.isConsumed())
        {
            return;
        }

        setVerticalScrollAmount(getVerticalScrollAmount() - getChildrenArea().height / 8);

        mouseEvent.consume();
    }

    void mouseWheelMovedDown(MouseEvent mouseEvent)
    {
        if (mouseEvent.isConsumed())
        {
            return;
        }

        setVerticalScrollAmount(getVerticalScrollAmount() + getChildrenArea().height / 8);

        mouseEvent.consume();
    }

protected:
    /**
      * Draws the background of the scroll area, that is
      * the area behind the content.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawBackground(Graphics graphics)
    {
        graphics.setColor(getBackgroundColor());
        graphics.fillRectangle(getChildrenArea());
    }


    /**
      * Draws the up button.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawUpButton(Graphics graphics)
    {
        Rectangle dim = getUpButtonDimension();
        graphics.pushClipArea(dim);

        Color highlightColor;
        Color shadowColor;
        Color faceColor;
        int offset;
        int alpha = getFrameColor().a;

        if (mUpButtonPressed)
        {
            faceColor = getFrameColor() - 0x303030;
            faceColor.a = alpha;
            highlightColor = faceColor - 0x303030;
            highlightColor.a = alpha;
            shadowColor = getFrameColor();
            shadowColor.a = alpha;

            offset = 1;
        }
        else
        {
            faceColor = getFrameColor();
            faceColor.a = alpha;
            highlightColor = faceColor + 0x303030;
            highlightColor.a = alpha;
            shadowColor = faceColor - 0x303030;
            shadowColor.a = alpha;

            offset = 0;
        }

        graphics.setColor(faceColor);
        graphics.fillRectangle(Rectangle(0, 0, dim.width, dim.height));

        graphics.setColor(highlightColor);
        graphics.drawLine(0, 0, dim.width - 1, 0);
        graphics.drawLine(0, 1, 0, dim.height - 1);

        graphics.setColor(shadowColor);
        graphics.drawLine(dim.width - 1, 0, dim.width - 1, dim.height - 1);
        graphics.drawLine(1, dim.height - 1, dim.width - 1, dim.height - 1);

        graphics.setColor(getForegroundColor());

        int i;
        int w = dim.height / 2;
        int h = w / 2 + 2;
        for (i = 0; i < w / 2; ++i)
        {
            graphics.drawLine(w - i + offset,
                               i + h + offset,
                               w + i + offset,
                               i + h + offset);
        }

        graphics.popClipArea();
    }

    /**
      * Draws the down button.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawDownButton(Graphics graphics)
    {
        Rectangle dim = getDownButtonDimension();
        graphics.pushClipArea(dim);

        Color highlightColor;
        Color shadowColor;
        Color faceColor;
        int offset;
        int alpha = getFrameColor().a;

        if (mDownButtonPressed)
        {
            faceColor = getFrameColor() - 0x303030;
            faceColor.a = alpha;
            highlightColor = faceColor - 0x303030;
            highlightColor.a = alpha;
            shadowColor = getFrameColor();
            shadowColor.a = alpha;

            offset = 1;
        }
        else
        {
            faceColor = getFrameColor();
            faceColor.a = alpha;
            highlightColor = faceColor + 0x303030;
            highlightColor.a = alpha;
            shadowColor = faceColor - 0x303030;
            shadowColor.a = alpha;

            offset = 0;
        }

        graphics.setColor(faceColor);
        graphics.fillRectangle(Rectangle(0, 0, dim.width, dim.height));

        graphics.setColor(highlightColor);
        graphics.drawLine(0, 0, dim.width - 1, 0);
        graphics.drawLine(0, 1, 0, dim.height - 1);

        graphics.setColor(shadowColor);
        graphics.drawLine(dim.width - 1, 0, dim.width - 1, dim.height - 1);
        graphics.drawLine(1, dim.height - 1, dim.width - 1, dim.height - 1);

        graphics.setColor(getForegroundColor());

        int i;
        int w = dim.height / 2;
        int h = w + 1;
        for (i = 0; i < w / 2; ++i)
        {
            graphics.drawLine(w - i + offset,
                               -i + h + offset,
                               w + i + offset,
                               -i + h + offset);
        }

        graphics.popClipArea();
    }

    /**
      * Draws the left button.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawLeftButton(Graphics graphics)
    {
        Rectangle dim = getLeftButtonDimension();
        graphics.pushClipArea(dim);

        Color highlightColor;
        Color shadowColor;
        Color faceColor;
        int offset;
        int alpha = getFrameColor().a;

        if (mLeftButtonPressed)
        {
            faceColor = getFrameColor() - 0x303030;
            faceColor.a = alpha;
            highlightColor = faceColor - 0x303030;
            highlightColor.a = alpha;
            shadowColor = getFrameColor();
            shadowColor.a = alpha;

            offset = 1;
        }
        else
        {
            faceColor = getFrameColor();
            faceColor.a = alpha;
            highlightColor = faceColor + 0x303030;
            highlightColor.a = alpha;
            shadowColor = faceColor - 0x303030;
            shadowColor.a = alpha;

            offset = 0;
        }

        graphics.setColor(faceColor);
        graphics.fillRectangle(Rectangle(0, 0, dim.width, dim.height));

        graphics.setColor(highlightColor);
        graphics.drawLine(0, 0, dim.width - 1, 0);
        graphics.drawLine(0, 1, 0, dim.height - 1);

        graphics.setColor(shadowColor);
        graphics.drawLine(dim.width - 1, 0, dim.width - 1, dim.height - 1);
        graphics.drawLine(1, dim.height - 1, dim.width - 1, dim.height - 1);

        graphics.setColor(getForegroundColor());

        int i;
        int w = dim.width / 2;
        int h = w - 2;
        for (i = 0; i < w / 2; ++i)
        {
            graphics.drawLine(i + h + offset,
                               w - i + offset,
                               i + h + offset,
                               w + i + offset);
        }

        graphics.popClipArea();
    }

    /**
      * Draws the right button.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawRightButton(Graphics graphics)
    {
        Rectangle dim = getRightButtonDimension();
        graphics.pushClipArea(dim);

        Color highlightColor;
        Color shadowColor;
        Color faceColor;
        int offset;
        int alpha = getFrameColor().a;

        if (mRightButtonPressed)
        {
            faceColor = getFrameColor() - 0x303030;
            faceColor.a = alpha;
            highlightColor = faceColor - 0x303030;
            highlightColor.a = alpha;
            shadowColor = getFrameColor();
            shadowColor.a = alpha;

            offset = 1;
        }
        else
        {
            faceColor = getFrameColor();
            faceColor.a = alpha;
            highlightColor = faceColor + 0x303030;
            highlightColor.a = alpha;
            shadowColor = faceColor - 0x303030;
            shadowColor.a = alpha;

            offset = 0;
        }

        graphics.setColor(faceColor);
        graphics.fillRectangle(Rectangle(0, 0, dim.width, dim.height));

        graphics.setColor(highlightColor);
        graphics.drawLine(0, 0, dim.width - 1, 0);
        graphics.drawLine(0, 1, 0, dim.height - 1);

        graphics.setColor(shadowColor);
        graphics.drawLine(dim.width - 1, 0, dim.width - 1, dim.height - 1);
        graphics.drawLine(1, dim.height - 1, dim.width - 1, dim.height - 1);

        graphics.setColor(getForegroundColor());

        int i;
        int w = dim.width / 2;
        int h = w + 1;
        for (i = 0; i < w / 2; ++i)
        {
            graphics.drawLine(-i + h + offset,
                               w - i + offset,
                               -i + h + offset,
                               w + i + offset);
        }

        graphics.popClipArea();
    }

    /**
      * Draws the vertical scroll bar.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawVBar(Graphics graphics)
    {
        Rectangle dim = getVerticalBarDimension();

        graphics.pushClipArea(dim);

        int alpha = getFrameColor().a;
        Color trackColor = getFrameColor() - 0x101010;
        trackColor.a = alpha;
        Color shadowColor = getFrameColor() - 0x303030;
        shadowColor.a = alpha;

        graphics.setColor(trackColor);
        graphics.fillRectangle(Rectangle(0, 0, dim.width, dim.height));

        graphics.setColor(shadowColor);
        graphics.drawLine(0, 0, 0, dim.height);

        graphics.popClipArea();
    }

    /**
      * Draws the horizontal scroll bar.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawHBar(Graphics graphics)
    {
        Rectangle dim = getHorizontalBarDimension();

        graphics.pushClipArea(dim);

        int alpha = getFrameColor().a;
        Color trackColor = getFrameColor() - 0x101010;
        trackColor.a = alpha;
        Color shadowColor = getFrameColor() - 0x303030;
        shadowColor.a = alpha;

        graphics.setColor(trackColor);
        graphics.fillRectangle(Rectangle(0, 0, dim.width, dim.height));

        graphics.setColor(shadowColor);
        graphics.drawLine(0, 0, dim.width, 0);

        graphics.popClipArea();
    }

    /**
      * Draws the vertical marker.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawVMarker(Graphics graphics)
    {
        Rectangle dim = getVerticalMarkerDimension();
        graphics.pushClipArea(dim);

        int alpha = getFrameColor().a;
        Color faceColor = getFrameColor();
        faceColor.a = alpha;
        Color highlightColor = faceColor + 0x303030;
        highlightColor.a = alpha;
        Color shadowColor = faceColor - 0x303030;
        shadowColor.a = alpha;

        graphics.setColor(faceColor);
        graphics.fillRectangle(Rectangle(1, 1, dim.width - 1, dim.height - 1));

        graphics.setColor(highlightColor);
        graphics.drawLine(0, 0, dim.width - 1, 0);
        graphics.drawLine(0, 1, 0, dim.height - 1);

        graphics.setColor(shadowColor);
        graphics.drawLine(1, dim.height - 1, dim.width - 1, dim.height - 1);
        graphics.drawLine(dim.width - 1, 0, dim.width - 1, dim.height - 1);

        graphics.popClipArea();
    }


    /**
      * Draws the horizontal marker.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawHMarker(Graphics graphics)
    {
        Rectangle dim = getHorizontalMarkerDimension();
        graphics.pushClipArea(dim);

        int alpha = getFrameColor().a;
        Color faceColor = getFrameColor();
        faceColor.a = alpha;
        Color highlightColor = faceColor + 0x303030;
        highlightColor.a = alpha;
        Color shadowColor = faceColor - 0x303030;
        shadowColor.a = alpha;

        graphics.setColor(faceColor);
        graphics.fillRectangle(Rectangle(1, 1, dim.width - 1, dim.height - 1));

        graphics.setColor(highlightColor);
        graphics.drawLine(0, 0, dim.width - 1, 0);
        graphics.drawLine(0, 1, 0, dim.height - 1);

        graphics.setColor(shadowColor);
        graphics.drawLine(1, dim.height - 1, dim.width - 1, dim.height - 1);
        graphics.drawLine(dim.width - 1, 0, dim.width - 1, dim.height - 1);

        graphics.popClipArea();
    }

    /**
      * Checks the policies for the scroll bars.
      */
    void checkPolicies()
    {
        int w = getWidth();
        int h = getHeight();

        mHBarVisible = false;
        mVBarVisible = false;


        if (!getContent())
        {
            mHBarVisible = (mHPolicy == ScrollPolicy.SHOW_ALWAYS);
            mVBarVisible = (mVPolicy == ScrollPolicy.SHOW_ALWAYS);
            return;
        }

        if (mHPolicy == ScrollPolicy.SHOW_AUTO &&
            mVPolicy == ScrollPolicy.SHOW_AUTO)
        {
            if (getContent().getWidth() <= w
                && getContent().getHeight() <= h)
            {
                mHBarVisible = false;
                mVBarVisible = false;
            }

            if (getContent().getWidth() > w)
            {
                mHBarVisible = true;
            }

            if ((getContent().getHeight() > h)
                || (mHBarVisible && getContent().getHeight() > h - mScrollbarWidth))
            {
                mVBarVisible = true;
            }

            if (mVBarVisible && getContent().getWidth() > w - mScrollbarWidth)
            {
                mHBarVisible = true;
            }

            return;
        }

        switch (mHPolicy)
        {
          case ScrollPolicy.SHOW_NEVER:
              mHBarVisible = false;
              break;

          case ScrollPolicy.SHOW_ALWAYS:
              mHBarVisible = true;
              break;

          case ScrollPolicy.SHOW_AUTO:
              if (mVPolicy == ScrollPolicy.SHOW_NEVER)
              {
                  mHBarVisible = getContent().getWidth() > w;
              }
              else // (mVPolicy == ScrollPolicy.SHOW_ALWAYS)
              {
                  mHBarVisible = getContent().getWidth() > w - mScrollbarWidth;
              }
              break;

          default:
              throw new GCN_Exception("Horizontal scroll policy invalid.");
        }

        switch (mVPolicy)
        {
          case ScrollPolicy.SHOW_NEVER:
              mVBarVisible = false;
              break;

          case ScrollPolicy.SHOW_ALWAYS:
              mVBarVisible = true;
              break;

          case ScrollPolicy.SHOW_AUTO:
              if (mHPolicy == ScrollPolicy.SHOW_NEVER)
              {
                  mVBarVisible = getContent().getHeight() > h;
              }
              else // (mHPolicy == ScrollPolicy.SHOW_ALWAYS)
              {
                  mVBarVisible = getContent().getHeight() > h - mScrollbarWidth;
              }
              break;
          default:
              throw new GCN_Exception("Vertical scroll policy invalid.");
        }
    }

    /**
      * Gets the up button dimension.
      *
      * @return the dimension of the up button.
      */
    Rectangle getUpButtonDimension()
    {
        if (!mVBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        return Rectangle(getWidth() - mScrollbarWidth,
                         0,
                         mScrollbarWidth,
                         mScrollbarWidth);
    }


    /**
      * Gets the down button dimension.
      *
      * @return the dimension of the down button.
      */
    Rectangle getDownButtonDimension()
    {
        if (!mVBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        if (mVBarVisible && mHBarVisible)
        {
            return Rectangle(getWidth() - mScrollbarWidth,
                             getHeight() - mScrollbarWidth*2,
                             mScrollbarWidth,
                             mScrollbarWidth);
        }

        return Rectangle(getWidth() - mScrollbarWidth,
                         getHeight() - mScrollbarWidth,
                         mScrollbarWidth,
                         mScrollbarWidth);
    }


    /**
      * Gets the left button dimension.
      *
      * @return the dimension of the left button.
      */
    Rectangle getLeftButtonDimension()
    {
        if (!mHBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        return Rectangle(0,
                         getHeight() - mScrollbarWidth,
                         mScrollbarWidth,
                         mScrollbarWidth);
    }


    /**
      * Gets the right button dimension.
      *
      * @return the dimension of the right button.
      */
    Rectangle getRightButtonDimension()
    {
        if (!mHBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        if (mVBarVisible && mHBarVisible)
        {
            return Rectangle(getWidth() - mScrollbarWidth*2,
                             getHeight() - mScrollbarWidth,
                             mScrollbarWidth,
                             mScrollbarWidth);
        }

        return Rectangle(getWidth() - mScrollbarWidth,
                         getHeight() - mScrollbarWidth,
                         mScrollbarWidth,
                         mScrollbarWidth);
    }


    /**
      * Gets the vertical scrollbar dimension.
      *
      * @return the dimension of the vertical scrollbar.
      */
    Rectangle getVerticalBarDimension()
    {
        if (!mVBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        if (mHBarVisible)
        {
            return Rectangle(getWidth() - mScrollbarWidth,
                             getUpButtonDimension().height,
                             mScrollbarWidth,
                             getHeight()
                             - getUpButtonDimension().height
                             - getDownButtonDimension().height
                             - mScrollbarWidth);
        }

        return Rectangle(getWidth() - mScrollbarWidth,
                         getUpButtonDimension().height,
                         mScrollbarWidth,
                         getHeight()
                         - getUpButtonDimension().height
                         - getDownButtonDimension().height);
    }

    /**
      * Gets the horizontal scrollbar dimension.
      *
      * @return the dimension of the horizontal scrollbar.
      */
    Rectangle getHorizontalBarDimension()
    {
        if (!mHBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        if (mVBarVisible)
        {
            return Rectangle(getLeftButtonDimension().width,
                             getHeight() - mScrollbarWidth,
                             getWidth()
                             - getLeftButtonDimension().width
                             - getRightButtonDimension().width
                             - mScrollbarWidth,
                             mScrollbarWidth);
        }

        return Rectangle(getLeftButtonDimension().width,
                         getHeight() - mScrollbarWidth,
                         getWidth()
                         - getLeftButtonDimension().width
                         - getRightButtonDimension().width,
                         mScrollbarWidth);
    }

    /**
      * Gets the vertical marker dimension.
      *
      * @return the dimension of the vertical marker.
      */
    Rectangle getVerticalMarkerDimension()
    {
        if (!mVBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        int length, pos;
        Rectangle barDim = getVerticalBarDimension();

        if (getContent() && getContent().getHeight() != 0)
        {
            length = (barDim.height * getChildrenArea().height)
                / getContent().getHeight();
        }
        else
        {
            length = barDim.height;
        }

        if (length < mScrollbarWidth)
        {
            length = mScrollbarWidth;
        }

        if (length > barDim.height)
        {
            length = barDim.height;
        }

        if (getVerticalMaxScroll() != 0)
        {
            pos = ((barDim.height - length) * getVerticalScrollAmount())
                / getVerticalMaxScroll();
        }
        else
        {
            pos = 0;
        }

        return Rectangle(barDim.x, barDim.y + pos, mScrollbarWidth, length);
    }


    /**
      * Gets the horizontal marker dimension.
      *
      * @return the dimension of the horizontal marker.
      */
    Rectangle getHorizontalMarkerDimension()
    {
        if (!mHBarVisible)
        {
            return Rectangle(0, 0, 0, 0);
        }

        int length, pos;
        Rectangle barDim = getHorizontalBarDimension();

        if (getContent() && getContent().getWidth() != 0)
        {
            length = (barDim.width * getChildrenArea().width)
                / getContent().getWidth();
        }
        else
        {
            length = barDim.width;
        }

        if (length < mScrollbarWidth)
        {
            length = mScrollbarWidth;
        }

        if (length > barDim.width)
        {
            length = barDim.width;
        }

        if (getHorizontalMaxScroll() != 0)
        {
            pos = ((barDim.width - length) * getHorizontalScrollAmount())
                / getHorizontalMaxScroll();
        }
        else
        {
            pos = 0;
        }

        return Rectangle(barDim.x + pos, barDim.y, length, mScrollbarWidth);
    }
    
    /**
      * Holds the vertical scroll amount.
      */
    int mVScroll;

    /**
      * Holds the horizontal scroll amount.
      */
    int mHScroll;

    /**
      * Holds the width of the scroll bars.
      */
    int mScrollbarWidth;

    /**
      * Holds the horizontal scroll bar policy.
      */
    ScrollPolicy mHPolicy;

    /**
      * Holds the vertical scroll bar policy.
      */
    ScrollPolicy mVPolicy;

    /**
      * True if the vertical scroll bar is visible, false otherwise.
      */
    bool mVBarVisible;

    /**
      * True if the horizontal scroll bar is visible, false otherwise.
      */
    bool mHBarVisible;

    /**
      * True if the up button is pressed, false otherwise.
      */
    bool mUpButtonPressed;

    /**
      * True if the down button is pressed, false otherwise.
      */
    bool mDownButtonPressed;

    /**
      * True if the left button is pressed, false otherwise.
      */
    bool mLeftButtonPressed;

    /**
      * True if the right button is pressed, false otherwise.
      */
    bool mRightButtonPressed;

    /**
      * Holds the up button scroll amount.
      */
    int mUpButtonScrollAmount;

    /**
      * Holds the down button scroll amount.
      */
    int mDownButtonScrollAmount;

    /**
      * Holds the left button scroll amount.
      */
    int mLeftButtonScrollAmount;

    /**
      * Holds the right button scroll amount.
      */
    int mRightButtonScrollAmount;

    /**
      * True if the vertical marked is dragged.
      */
    bool mIsVerticalMarkerDragged;

    /**
      * True if the horizontal marked is dragged.
      */
    bool mIsHorizontalMarkerDragged;

    /**
      * Holds the horizontal markers drag offset.
      */
    int mHorizontalMarkerDragOffset;

    /**
      * Holds the vertical markers drag offset.
      */
    int mVerticalMarkerDragOffset;
}
