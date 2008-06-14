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

module guichan.widgets.textfield;

import guichan.all;
import guichan.util;

/**
  * An implementation of a text field where a user can enter a line of text.
  */
class TextField : Widget
{
public:
    /**
      * Constructor.
      */
    this()
    {
        mCaretPosition = 0;
        mXScroll = 0;

        setFocusable(true);

        addMouseListener(this);
        addKeyListener(this);
    }

    /**
      * Constructor. The text field will be automatically resized
      * to fit the text.
      *
      * @param text The default text of the text field.
      */
    this(dstring text)
    {
        mCaretPosition = 0;
        mXScroll = 0;

        mText = text;
        adjustSize();

        setFocusable(true);

        addMouseListener(this);
        addKeyListener(this);
    }

    /**
      * Sets the text of the text field.
      *
      * @param text The text of the text field.
      * @see getText
      */
    void setText(dstring text)
    {
        if(text.length < mCaretPosition )
        {
            mCaretPosition = text.length;
        }

        mText = text;
    }

    /**
      * Gets the text of the text field.
      *
      * @return The text of the text field.
      * @see setText
      */
    dstring getText()
    {
        return mText;
    }

    /**
      * Adjusts the size of the text field to fit the text.
      */
    void adjustSize()
    {
        setWidth(getFont().getWidth(mText) + 6);
        adjustHeight();

        fixScroll();
    }


    /**
      * Adjusts the height of the text field to fit caption.
      */
    void adjustHeight()
    {
        setHeight(getFont().getHeight() + 4);
    }


    /**
      * Sets the caret position. As there is only one line of text
      * in a text field the position is the caret's x coordinate.
      *
      * @param position The caret position.
      * @see getCaretPosition
      */
    void setCaretPosition(uint position)
    {
        if (position > mText.length)
        {
            mCaretPosition = mText.length;
        }
        else
        {
            mCaretPosition = position;
        }

        fixScroll();
    }


    /**
      * Gets the caret position. As there is only one line of text
      * in a text field the position is the caret's x coordinate.
      *
      * @return The caret position.
      * @see setCaretPosition
      */
    uint getCaretPosition()
    {
        return mCaretPosition;
    }

    // Inherited from Widget

    void fontChanged()
    {
        fixScroll();
    }

    void draw(Graphics graphics)
    {
        // Draw a border.
        graphics.setColor(getFrameColor());
        graphics.drawLine(0, 0, getWidth() - 1, 0);
        graphics.drawLine(0, 1, 0, getHeight() - 2);
        graphics.drawLine(getWidth() - 1, 1, getWidth() - 1, getHeight() - 1);
        graphics.drawLine(0, getHeight() - 1, getWidth() - 1, getHeight() - 1);

        // Push a clip area so the other drawings don't need to worry
        // about the border.
        graphics.pushClipArea(Rectangle(1, 1, getWidth() - 2, getHeight() - 2));

        graphics.setColor(getBackgroundColor());
        graphics.fillRectangle(Rectangle(0, 0, getWidth(), getHeight()));

        if (isFocused())
        {
            drawCaret(graphics, getFont().getWidth(mText[0 .. mCaretPosition]) - mXScroll);
        }

        graphics.setColor(getForegroundColor());
        graphics.setFont(getFont());
        graphics.drawText(mText, 1 - mXScroll, 1,Alignment.LEFT);

        graphics.popClipArea();
    }


    // Inherited from MouseListener

    void mousePressed(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            mCaretPosition = getFont().getStringIndexAt(mText, mouseEvent.getX() + mXScroll,1);
            fixScroll();
        }
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        mouseEvent.consume();
    }

    // Inherited from KeyListener

    void keyPressed(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if (key.getValue() == Key.LEFT && mCaretPosition > 0)
        {
            --mCaretPosition;
        }

        else if (key.getValue() == Key.RIGHT && mCaretPosition < mText.length)
        {
            ++mCaretPosition;
        }

        else if (key.getValue() == Key.DELETE && mCaretPosition < mText.length)
        {
            mText.remove_index(mCaretPosition);
        }

        else if (key.getValue() == Key.BACKSPACE && mCaretPosition > 0)
        {
            mText.remove_index(mCaretPosition - 1);
            --mCaretPosition;
        }

        else if (key.getValue() == Key.ENTER)
        {
//             distributeActionEvent();
        }

        else if (key.getValue() == Key.HOME)
        {
            mCaretPosition = 0;
        }

        else if (key.getValue() == Key.END)
        {
            mCaretPosition = mText.length;
        }

        else if (key.isCharacter()
                 && key.getValue() != Key.TAB)
        {
            mText = mText[ 0 .. mCaretPosition ] ~ cast(dchar)key.getValue() ~  mText[mCaretPosition .. $ ];
            ++mCaretPosition;
        }

        if (key.getValue() != Key.TAB)
        {
            keyEvent.consume();
        }
        fixScroll();
    }

protected:
    /**
      * Draws the caret. Overloaded this method if you want to
      * change the style of the caret.
      *
      * @param graphics the Graphics object to draw with.
      * @param x the caret's x-position.
      */
    void drawCaret(Graphics graphics, int x)
    {
        // Check the current clip area as a clip area with a different
        // size than the widget might have been pushed (which is the
        // case in the draw method when we push a clip area after we have
        // drawn a border).
        ClipRectangle clipArea = graphics.getCurrentClipArea();

        graphics.setColor(getForegroundColor());
        graphics.drawLine(x, clipArea.height - 2, x, 1);
    }

    /**
      * Scrolls the text horizontally so that the caret shows if needed.
      * The method is used any time a user types in the text field so the
      * caret always will be shown.
      */
    void fixScroll()
    {
        if (isFocused())
        {
            int caretX = getFont().getWidth(mText[0 .. mCaretPosition]);

            if (caretX - mXScroll >= getWidth() - 4)
            {
                mXScroll = caretX - getWidth() + 4;
            }
            else if (caretX - mXScroll <= 0)
            {
                mXScroll = caretX - getWidth() / 2;

                if (mXScroll < 0)
                {
                    mXScroll = 0;
                }
            }
        }
    }


    /**
      * Holds the text of the text box.
      */
    dstring mText;

    /**
      * Holds the caret position.
      */
    uint mCaretPosition;

    /**
      * Holds the amount scrolled in x. If a user types more characters than
      * the text field can display, due to the text field being to small, the
      * text needs to scroll in order to show the last type character.
      */
    int mXScroll;
}
