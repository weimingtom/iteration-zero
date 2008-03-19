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

module guichan.widgets.window;

private import
  guichan.all,
  guichan.widgets.container;

class Window : Container, MouseListener
{
    this()
    {
        mMoved = false;
        setFrameSize(1);
        setPadding(2);
        setTitleBarHeight(26);
        setAlignment(Alignment.CENTER);
        addMouseListener(this);
        setMovable(true);
        setOpaque(true);
    }

    this(dstring caption)
    {
        mMoved = false;
        setCaption(caption);
        setFrameSize(1);
        setPadding(2);
        setTitleBarHeight(26);
        setAlignment(Alignment.CENTER);
        addMouseListener(this);
        setMovable(true);
        setOpaque(true);
    }

    void setPadding(uint padding)
    {
        mPadding = padding;
    }

    uint getPadding()
    {
        return mPadding;
    }

    void setTitleBarHeight(uint height)
    {
        mTitleBarHeight = height;
    }

    uint getTitleBarHeight()
    {
        return mTitleBarHeight;
    }

    void setCaption(dstring caption)
    {
        mCaption = caption;
    }

    dstring getCaption()
    {
        return mCaption;
    }

    void setAlignment(Alignment alignment)
    {
        mAlignment = alignment;
    }

    Alignment getAlignment()
    {
        return mAlignment;
    }

    void draw(Graphics graphics)
    {
        int width = getWidth() + getFrameSize() * 2 - 1;
        int height = getHeight() + getFrameSize() * 2 - 1;

        Rectangle d = Rectangle(getChildrenArea());

        // Fill the background around the content
        graphics.setColor(getFrameColor);
        // Fill top
        graphics.fillRectangle(Rectangle(0,0,getWidth(),d.y - 1));
        // Fill left
        graphics.fillRectangle(Rectangle(0,d.y - 1, d.x - 1, getHeight() - d.y + 1));
        // Fill right
        graphics.fillRectangle(Rectangle(d.x + d.width + 1,
                                          d.y - 1,
                                          getWidth() - d.x - d.width - 1,
                                          getHeight() - d.y + 1));
        // Fill bottom
        graphics.fillRectangle(Rectangle(d.x - 1,
                                          d.y + d.height + 1,
                                          d.width + 2,
                                          getHeight() - d.height - d.y - 1));

        if (isOpaque())
        {
            graphics.setColor(getBackgroundColor);
            graphics.fillRectangle(d);
        }

        //ruct a rectangle one pixel bigger than the content
        d.x -= 1;
        d.y -= 1;
        d.width += 2;
        d.height += 2;

//         // Draw a border around the content
//         graphics.setColor(shadowColor);
//         // Top line
//         graphics.drawLine(d.x,
//                            d.y,
//                            d.x + d.width - 2,
//                            d.y);
// 
//         // Left line
//         graphics.drawLine(d.x,
//                            d.y + 1,
//                            d.x,
//                            d.y + d.height - 1);
// 
//         graphics.setColor(highlightColor);
//         // Right line
//         graphics.drawLine(d.x + d.width - 1,
//                            d.y,
//                            d.x + d.width - 1,
//                            d.y + d.height - 2);
//         // Bottom line
//         graphics.drawLine(d.x + 1,
//                            d.y + d.height - 1,
//                            d.x + d.width - 1,
//                            d.y + d.height - 1);

        drawChildren(graphics);

        int textX;
        int textY;

        textY = (cast(int)getTitleBarHeight() - getFont().getHeight()) / 2;

        switch (getAlignment())
        {
          case Alignment.LEFT:
              textX = 4;
              break;
          case Alignment.CENTER:
              textX = getWidth() / 2;
              break;
          case Alignment.RIGHT:
              textX = getWidth() - 4;
              break;
          default:
              throw new GCN_Exception("Unknown alignment.");
        }

        graphics.setColor(getForegroundColor());
        graphics.setFont(getFont());
        graphics.pushClipArea(Rectangle(0, 0, getWidth(), getTitleBarHeight() - 1));
        graphics.drawText(getCaption(), textX, textY, getAlignment());
        graphics.popClipArea();
    }

    void mousePressed(MouseEvent mouseEvent)
    {
        if (mouseEvent.getSource() !is this)
        {
            return;
        }
        
        if (getParent() !is null)
        {
            getParent().moveToTop(this);
        }

        mDragOffsetX = mouseEvent.getX();
        mDragOffsetY = mouseEvent.getY();
        
        mMoved = mouseEvent.getY() <= cast(int)mTitleBarHeight;
    }

    void mouseReleased(MouseEvent mouseEvent)
    {
        mMoved = false;
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        if (mouseEvent.isConsumed() || mouseEvent.getSource() !is this)
        {
            return;
        }
        
        if (isMovable() && mMoved)
        {
            setPosition(mouseEvent.getX() - mDragOffsetX + getX(),
                        mouseEvent.getY() - mDragOffsetY + getY());
        }

        mouseEvent.consume();
    }

    Rectangle getChildrenArea()
    {
        return Rectangle(getPadding(),
                         getTitleBarHeight(),
                         getWidth() - getPadding() * 2,
                         getHeight() - getPadding() - getTitleBarHeight());
    }

    void setMovable(bool movable)
    {
        mMovable = movable;
    }

    bool isMovable()
    {
        return mMovable;
    }

    void setOpaque(bool opaque)
    {
        mOpaque = opaque;
    }

    bool isOpaque()
    {
        return mOpaque;
    }

    void resizeToContent()
    {
        int w = 0, h = 0;
        foreach (Widget widget; mWidgets)
        {
            if (widget.getX() + widget.getWidth() > w)
            {
                w = widget.getX() + widget.getWidth();
            }

            if (widget.getY() + widget.getHeight() > h)
            {
                h = widget.getY() + widget.getHeight();
            }
        }

        setSize(w + 2* getPadding(), h + getPadding() + getTitleBarHeight());
    }

    void mouseEntered(MouseEvent mouseEvent){}
    void mouseExited(MouseEvent mouseEvent){}
    void mouseClicked(MouseEvent mouseEvent){}
    void mouseWheelMovedUp(MouseEvent mouseEvent){}
    void mouseWheelMovedDown(MouseEvent mouseEvent){}
    void mouseMoved(MouseEvent mouseEvent){}


protected:
    /**
      * Holds the caption of the window.
      */
    dstring mCaption;

    /**
      * Holds the alignment of the caption.
      */
    Alignment mAlignment;

    /**
      * Holds the padding of the window.
      */ 
    uint mPadding;

    /**
      * Holds the title bar height of the window.
      */
    uint mTitleBarHeight;

    /**
      * True if the window is movable, false otherwise.
      */
    bool mMovable;

    /**
      * True if the window is opaque, false otherwise.
      */
    bool mOpaque;

    /**
      * Holds a drag offset as an x coordinate where the drag of the window
      * started if the window is being dragged. It's used to move the window 
      * correctly when dragged.
      */
    int mDragOffsetX;

    /**
      * Holds a drag offset as an y coordinate where the drag of the window
      * started if the window is being dragged. It's used to move the window 
      * correctly when dragged.
      */
    int mDragOffsetY;

    /**
      * True if the window is being moved, false otherwise.
      */
    bool mMoved;
}
