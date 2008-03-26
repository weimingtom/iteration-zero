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

module guichan.widgets.textbox;

import guichan.all;
import guichan.util;

/**
  * An implementation of a text box where a user can enter text that contains of many lines.
  */
class TextBox: Widget
{
public:
    /**
      * Constructor.
      */
    this()
    {
        mCaretColumn = 0;
        mCaretRow = 0;
        mEditable = true;
        mOpaque = true;

        setFocusable(true);

        addMouseListener(this);
        addKeyListener(this);
        adjustSize();
        setText("");
    }

    /**
      * Constructor.
      *
      * @param text The default text of the text box.
      */
    this(dstring text)
    {
        mCaretColumn = 0;
        mCaretRow = 0;
        mEditable = true;
        mOpaque = true;

        setText(text);

        setFocusable(true);

        addMouseListener(this);
        addKeyListener(this);
        adjustSize();
    }


    /**
      * Sets the text of the text box.
      *
      * @param text The text of the text box.
      * @see getText
      */
    void setText(dstring text)
    {
        mCaretColumn = 0;
        mCaretRow = 0;

        mTextRows = splitlines(text);
        adjustSize();
    }

    /**
      * Gets the text of the text box.
      *
      * @return The text of the text box.
      * @see setText
      */
    dstring getText()
    {
        if (mTextRows.length == 0)
        {
            return ""d;
        }

        int i;
        dstring text;

        for (i = 0; i < mTextRows.length - 1; ++i)
        {
            text = text ~ mTextRows[i] ~ "\n"d;
        }

        text = text ~ mTextRows[i];

        return text;
    }

    /**
      * Gets a certain row from the text.
      *
      * @param row The number of the row to get from the text.
      * @return A row from the text of the text box.
      * @see setTextRow
      */
    dstring getTextRow(int row)
    {
        return mTextRows[row];
    }


    /**
      * Sets the text of a certain row of the text.
      *
      * @param row The number of the row to set in the text.
      * @param text The text to set in the given row number.
      * @see getTextRow
      */
    void setTextRow(int row, dstring text)
    {
        mTextRows[row] = text;

        if (mCaretRow == row)
        {
            setCaretColumn(mCaretColumn);
        }

        adjustSize();
    }


    /**
      * Gets the number of rows in the text.
      *
      * @return The number of rows in the text.
      */
    uint getNumberOfRows()
    {
        return mTextRows.length;
    }


    /**
      * Gets the caret position in the text.
      *
      * @return The caret position in the text.
      * @see setCaretPosition
      */
    uint getCaretPosition()
    {
        int pos = 0, row;

        for (row = 0; row < mCaretRow; row++)
        {
            pos += mTextRows[row].length;
        }

        return pos + mCaretColumn;
    }

    /**
      * Sets the position of the caret in the text.
      *
      * @param position the positon of the caret.
      * @see getCaretPosition
      */
    void setCaretPosition(int position)
    {
        int row;

        for (row = 0; row < mTextRows.length; row++)
        {
            if (position <= mTextRows[row].length)
            {
                mCaretRow = row;
                mCaretColumn = position;
                return; // we are done
            }
            else
            {
                position--;
            }
        }

        // position beyond end of text
        mCaretRow = mTextRows.length - 1;
        mCaretColumn = mTextRows[mCaretRow].length;
    }


    /**
      * Gets the row number where the caret is currently located.
      *
      * @return The row number where the caret is currently located.
      * @see setCaretRow
      */
    uint getCaretRow()
    {
        return mCaretRow;
    }


    /**
      * Sets the row where the caret should be currently located.
      *
      * @param The row where the caret should be currently located.
      * @see getCaretRow
      */
    void setCaretRow(int row)
    {
        mCaretRow = row;

        if (mCaretRow >= mTextRows.length)
        {
            mCaretRow = mTextRows.length - 1;
        }

        if (mCaretRow < 0)
        {
            mCaretRow = 0;
        }

        setCaretColumn(mCaretColumn);
    }


    /**
      * Gets the column where the caret is currently located.
      *
      * @return The column where the caret is currently located.
      * @see setCaretColumn
      */
    uint getCaretColumn()
    {
        return mCaretColumn;
    }


    /**
      * Sets the column where the caret should be currently located.
      *
      * @param The column where the caret should be currently located.
      * @see getCaretColumn
      */
    void setCaretColumn(int column)
    {
        mCaretColumn = column;

        if (mCaretColumn > mTextRows[mCaretRow].length)
        {
            mCaretColumn = mTextRows[mCaretRow].length;
        }

        if (mCaretColumn < 0)
        {
            mCaretColumn = 0;
        }
    }


    /**
      * Sets the row and the column where the caret should be curretly
      * located.
      *
      * @param row The row where the caret should be currently located.
      * @param column The column where the caret should be currently located.
      * @see getCaretRow, getCaretColumn
      */
    void setCaretRowColumn(int row, int column)
    {
        setCaretRow(row);
        setCaretColumn(column);
    }


    /**
      * Scrolls the text to the caret if the text box is in a scroll area.
      * 
      * @see ScrollArea
      */
    void scrollToCaret()
    {
        Rectangle scroll;
        scroll.x = getFont().getWidth(mTextRows[mCaretRow][0 .. mCaretColumn]);
        scroll.y = getFont().getHeight() * mCaretRow;
        scroll.width = getFont().getWidth(" "d);
        scroll.height = getFont().getHeight() + 2; // add 2 for some extra space

        showPart(scroll);
    }

 
    /**
      * Checks if the text box is editable.
      *
      * @return True it the text box is editable, false otherwise.
      * @see setEditable
      */
    bool isEditable()
    {
        return mEditable;
    }


    /**
      * Sets the text box to be editable or not.
      *
      * @param editable True if the text box should be editable, false otherwise.
      */
    void setEditable(bool editable)
    {
        mEditable = editable;
    }


    /**
      * Adds a row of text to the end of the text.
      *
      * @param row The row to add.
      */
    void addRow(dstring row)
    {
        mTextRows~=row;
        adjustSize();
    }


    /**
      * Checks if the text box is opaque. An opaque text box will draw
      * it's background and it's text. A non opaque text box only draw it's
      * text making it transparent.
      *
      * @return True if the text box is opaque, false otherwise.
      * @see setOpaque
      */
    bool isOpaque()
    {
        return mOpaque;
    }


    /**
      * Sets the text box to be opaque or not. An opaque text box will draw
      * it's background and it's text. A non opaque text box only draw it's
      * text making it transparent.
      *
      * @param opaque True if the text box should be opaque, false otherwise.
      * @see isOpaque
      */
    void setOpaque(bool opaque)
    {
        mOpaque = opaque;
    }

    // Inherited from Widget

    void draw(Graphics graphics)
    {
        /*
        int width = getWidth() + getBorderSize() * 2 - 1;
        int height = getHeight() + getBorderSize() * 2 - 1;

        graphics.setColor(getBackgroundColor());

        unsigned int i;
        for (i = 0; i < getBorderSize(); ++i)
        {
            graphics.drawLine(i,i, width - i, i);
            graphics.drawLine(i,i + 1, i, height - i - 1);
            graphics.drawLine(width - i,i + 1, width - i, height - i);
            graphics.drawLine(i,height - i, width - i - 1, height - i);
        }
        */

        uint i;

        if (mOpaque)
        {
            graphics.setColor(getBackgroundColor());
            graphics.fillRectangle(Rectangle(0, 0, getWidth(), getHeight()));
        }

        if (isFocused() && isEditable())
        {
            drawCaret(graphics, getFont().getWidth(mTextRows[mCaretRow][0 .. mCaretColumn]), mCaretRow * getFont().getHeight());
        }

        graphics.setColor(getForegroundColor());
        graphics.setFont(getFont());

        for (i = 0; i < mTextRows.length; i++)
        {
            // Move the text one pixel so we can have a caret before a letter.
            graphics.drawText(mTextRows[i], 1, i * getFont().getHeight(),Alignment.LEFT);
        }
    }

    void fontChanged()
    {
        adjustSize();
    }

    // Inherited from KeyListener

    void keyPressed(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if (key.getValue() == Key.LEFT)
        {
            --mCaretColumn;
            if (mCaretColumn < 0)
            {
                --mCaretRow;

                if (mCaretRow < 0)
                {
                    mCaretRow = 0;
                    mCaretColumn = 0;
                }
                else
                {
                    mCaretColumn = mTextRows[mCaretRow].length;
                }
            }
        }

        else if (key.getValue() == Key.RIGHT)
        {
            ++mCaretColumn;
            if (mCaretColumn > mTextRows[mCaretRow].length)
            {
                ++mCaretRow;

                if (mCaretRow >= mTextRows.length)
                {
                    mCaretRow = mTextRows.length - 1;
                    if (mCaretRow < 0)
                    {
                        mCaretRow = 0;
                    }

                    mCaretColumn = mTextRows[mCaretRow].length;
                }
                else
                {
                    mCaretColumn = 0;
                }
            }
        }

        else if (key.getValue() == Key.DOWN)
        {
            setCaretRow(mCaretRow + 1);
        }

        else if (key.getValue() == Key.UP)
        {
            setCaretRow(mCaretRow - 1);
        }

        else if (key.getValue() == Key.HOME)
        {
            mCaretColumn = 0;
        }

        else if (key.getValue() == Key.END)
        {
            mCaretColumn = mTextRows[mCaretRow].length;
        }

        else if (key.getValue() == Key.ENTER && mEditable)
        {
//             mTextRows.insert(mTextRows.begin() + mCaretRow + 1,
//                              mTextRows[mCaretRow].substr(mCaretColumn, mTextRows[mCaretRow].length - mCaretColumn));
            mTextRows[mCaretRow].length = mCaretColumn;
            ++mCaretRow;
            mCaretColumn = 0;
        }

        else if (key.getValue() == Key.BACKSPACE
                 && mCaretColumn != 0
                 && mEditable)
        {
            mTextRows[mCaretRow].remove_index(mCaretColumn - 1);
            --mCaretColumn;
        }

        else if (key.getValue() == Key.BACKSPACE
                 && mCaretColumn == 0
                 && mCaretRow != 0
                 && mEditable)
        {
            mCaretColumn = mTextRows[mCaretRow - 1].length;
            mTextRows[mCaretRow - 1] ~= mTextRows[mCaretRow];
            mTextRows.remove_index(mCaretRow);
            --mCaretRow;
        }

        else if (key.getValue() == Key.DELETE
                 && mCaretColumn < mTextRows[mCaretRow].length
                 && mEditable)
        {
            mTextRows[mCaretRow].remove_index(mCaretColumn);
        }

        else if (key.getValue() == Key.DELETE
                 && mCaretColumn == mTextRows[mCaretRow].length
                 && mCaretRow < (mTextRows.length - 1)
                 && mEditable)
        {
            mTextRows[mCaretRow] ~= mTextRows[mCaretRow + 1];
            mTextRows.remove_index(mCaretRow + 1);
        }

        else if(key.getValue() == Key.PAGE_UP)
        {
            Widget par = getParent();

            if (par !is null)
            {
                int rowsPerPage = par.getChildrenArea().height / getFont().getHeight();
                mCaretRow -= rowsPerPage;

                if (mCaretRow < 0)
                {
                    mCaretRow = 0;
                }
            }
        }

        else if(key.getValue() == Key.PAGE_DOWN)
        {
            Widget par = getParent();

            if (par !is null)
            {
                int rowsPerPage = par.getChildrenArea().height / getFont().getHeight();
                mCaretRow += rowsPerPage;

                if (mCaretRow >= mTextRows.length)
                {
                    mCaretRow = mTextRows.length - 1;
                }
            }
        }

        else if(key.getValue() == Key.TAB
                && mEditable)
        {
            mTextRows[mCaretRow] = mTextRows[mCaretRow][0..mCaretColumn] ~ "    "d ~ mTextRows[mCaretRow][mCaretColumn .. $];
            mCaretColumn += 4;
        }

        else if (key.isCharacter()
                 && mEditable)
        {
            mTextRows[mCaretRow] = mTextRows[mCaretRow][0..mCaretColumn] ~ cast(dchar)key.getValue() ~ mTextRows[mCaretRow][mCaretColumn .. $];
            ++mCaretColumn;
        }

        adjustSize();
        scrollToCaret();

        keyEvent.consume();
    }



    // Inherited from MouseListener

    void mousePressed(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            mCaretRow = mouseEvent.getY() / getFont().getHeight();

            if (mCaretRow >= mTextRows.length)
            {
                mCaretRow = mTextRows.length - 1;
            }

            mCaretColumn = getFont().getStringIndexAt(mTextRows[mCaretRow], mouseEvent.getX(),1);
        }
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        mouseEvent.consume();
    }

protected:
    /**
      * Draws the caret. Overloaded this method if you want to
      * change the style of the caret.
      *
      * @param graphics a Graphics object to draw with.
      * @param x the x position.
      * @param y the y position.
      */
    void drawCaret(Graphics graphics, int x, int y)
    {
        graphics.setColor(getForegroundColor());
        graphics.drawLine(x, getFont().getHeight() + y, x, y);
    }

    /**
      * Adjusts the text box's size to fit the text.
      */
    void adjustSize()
    {
        uint i;
        int width = 0;
        for (i = 0; i < mTextRows.length; ++i)
        {
            int w = getFont().getWidth(mTextRows[i]);
            if (width < w)
            {
                width = w;
            }
        }

        setWidth(width + 1);
        setHeight(getFont().getHeight() * mTextRows.length);
    }


    /**
      * Holds all the rows of the text.
      */
    dstring[] mTextRows;

    /**
      * Holds the current column of the caret.
      */
    int mCaretColumn;

    /**
      * Holds the current row of the caret.
      */
    int mCaretRow;

    /**
      * True if the text box is editable, false otherwise.
      */
    bool mEditable;

    /**
      * True if the text box is editable, false otherwise.
      */
    bool mOpaque;
}
