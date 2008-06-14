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

module guichan.widgets.listbox;

import
  std.utf, std.string;

interface IListModel
{
    int getNumberOfElements();
    dstring getElementAt(int i);
}

class ListModel(T) : IListModel
{
    T data;
    this(T t) {
      data = t;
    }
    int getNumberOfElements()
    {
      return data.length;
    }

    dstring getElementAt(int i)
    {
      assert( i >= 0 && i < data.length, "ListBox requested out-of bounds element." );

      static if( is( typeof(T[0]) == string) )
        return toUTF32(data[i]);
      else
        return toUTF32(toString(data[i]));
    }
}

alias ListModel!(string[]) StringListModel;

import guichan.all;

/**
  * An implementation of a list box where an item can be selected.
  *
  * To be able display a list the list box uses a user provided list model. 
  * A list model can be any class that implements the ListModel interface.
  *     
  * If an item is selected in the list box a select event will be sent to all selection 
  * listeners of the list box. If an item is selected by using a mouse click or by using 
  * the enter or space key an action event will be sent to all action listeners of the 
  * list box.
  */
class ListBox : Widget
{
public:
    /**
      * Constructor.
      */
    this()
    {
        mSelected = (-1);
        mWrappingEnabled = (false);
        setListModel(null);

        setWidth(100);
        setFocusable(true);

        addMouseListener(this);
        addKeyListener(this);
    }

    /**
      * Constructor.
      *
      * @param listModel the list model to use.
      */
    this(IListModel listModel)
    {
        mSelected = (-1);
        mWrappingEnabled = (false);

        setWidth(100);
        setListModel(listModel);
        setFocusable(true);

        addMouseListener(this);
        addKeyListener(this);
    }
    /**
      * Gets the selected item as an index in the list model.
      *
      * @return the selected item as an index in the list model.
      * @see setSelected
      */
    int getSelected()
    {
        return mSelected;
    }

    /**
    * Sets the selected item. The selected item is represented by
    * an index from the list model.
    *
    * @param selected the selected item as an index from the list model.
    * @see getSelected
    */
    void setSelected(int selected)
    {
        if (mListModel is null)
        {
            mSelected = -1;
        }
        else
        {
            if (selected < 0)
            {
                mSelected = -1;
            }
            else if (selected >= mListModel.getNumberOfElements())
            {
                mSelected = mListModel.getNumberOfElements() - 1;
            }
            else
            {
                mSelected = selected;
            }

            Rectangle scroll;

            if (mSelected < 0)
            {
                scroll.y = 0;
            }
            else
            {
                scroll.y = getRowHeight() * mSelected;
            }

            scroll.height = getRowHeight();
            showPart(scroll);
        }

//         distributeValueChangedEvent();
    }

    /**
      * Sets the list model to use.
      *
      * @param listModel the list model to use.
      * @see getListModel
      */
    void setListModel(IListModel listModel)
    {
        mSelected = -1;
        mListModel = listModel;
        adjustSize();
    }
    /**
      * Gets the list model used.
      *
      * @return the list model used.
      * @see setListModel
      */
    IListModel getListModel()
    {
        return mListModel;
    }

    /**
      * Adjusts the size of the list box to fit it's list model.
      */
    void adjustSize()
    {
        if (mListModel !is null)
        {
            setHeight(getRowHeight() * mListModel.getNumberOfElements());
        }
    }

    /**
      * Checks whether the list box wraps when selecting items with a keyboard.
      *
      * Wrapping means that the selection of items will be wrapped. That is, if 
      * the first item is selected and up is pressed, the last item will get 
      * selected. If the last item is selected and down is pressed, the first item 
      * will get selected.
      *
      * @return true if wrapping is enabled, fasle otherwise.
      * @see setWrappingEnabled
      */
    bool isWrappingEnabled()
    {
        return mWrappingEnabled;
    }

    /**
      * Sets the list box to wrap or not when selecting items with a keyboard.
      *
      * Wrapping means that the selection of items will be wrapped. That is, if 
      * the first item is selected and up is pressed, the last item will get 
      * selected. If the last item is selected and down is pressed, the first item 
      * will get selected.
      * 
      * @see isWrappingEnabled
      */
    void setWrappingEnabled(bool wrappingEnabled)
    {
        mWrappingEnabled = wrappingEnabled;
    }

    /**
      * Adds a selection listener to the list box. When the selection
      * changes an event will be sent to all selection listeners of the
      * list box.
      *
      * @param selectionListener The selection listener to add.
      * @since 0.8.0
      */
//     void addSelectionListener(SelectionListener* selectionListener);

    /**
      * Removes a selection listener from the list box.
      *
      * @param selectionListener The selection listener to remove.
      * @since 0.8.0
      */
//     void removeSelectionListener(SelectionListener* selectionListener);

    /**
      * Gets the height of a row. Should be overridden if another row
      * height than the font height is preferred.
      *
      * @return The height of a row.
      * @since 0.8.0
      */
    int getRowHeight()
    {
        return getFont().getHeight();
    }

    // Inherited from Widget

    void draw(Graphics graphics)
    {
        graphics.setColor(getBackgroundColor());
        graphics.fillRectangle(Rectangle(0, 0, getWidth(), getHeight()));

        if (mListModel is null)
        {
            return;
        }

        graphics.setColor(getForegroundColor());
        graphics.setFont(getFont());

        // Check the current clip area so we don't draw unnecessary items
        // that are not visible.
        ClipRectangle currentClipArea = graphics.getCurrentClipArea();
        int rowHeight = getRowHeight();

        // Calculate the number of rows to draw by checking the clip area.
        // The addition of two makes covers a partial visible row at the top
        // and a partial visible row at the bottom.
        int numberOfRows = currentClipArea.height / rowHeight + 2;

        if (numberOfRows > mListModel.getNumberOfElements())
        {
            numberOfRows = mListModel.getNumberOfElements();
        }

        // Calculate which row to start drawing. If the list box 
        // has a negative y coordinate value we should check if
        // we should drop rows in the begining of the list as
        // they might not be visible. A negative y value is very
        // common if the list box for instance resides in a scroll
        // area and the user has scrolled the list box downwards.
        int startRow;
        if (getY() < 0)
        {
                startRow = -1 * (getY() / rowHeight);
        }
        else
        {
                startRow = 0;
        }

        int i;
        // The y coordinate where we start to draw the text is
        // simply the y coordinate multiplied with the font height.
        int y = rowHeight * startRow;
        for (i = startRow; i < /+startRow + +/numberOfRows; ++i) // Commented out startRow ... KB
        {
            if (i == mSelected)
            {
                graphics.setColor(getSelectionColor());
                graphics.fillRectangle(Rectangle(0, y, getWidth(), rowHeight));
                graphics.setColor(getForegroundColor());
            }

            // If the row height is greater than the font height we
            // draw the text with a center vertical alignment.
            if (rowHeight > getFont().getHeight())
            {
                    graphics.drawText(mListModel.getElementAt(i), 1, y + rowHeight / 2 - getFont().getHeight() / 2, Alignment.LEFT);
            }
            else
            {
                    graphics.drawText(mListModel.getElementAt(i), 1, y,Alignment.LEFT);
            }

            y += rowHeight;
        }
    }

    void logic()
    {
        adjustSize();
    }


    // Inherited from KeyListener

    void keyPressed(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if (key.getValue() == Key.ENTER || key.getValue() == Key.SPACE)
        {
//             distributeActionEvent();
            keyEvent.consume();
        }
        else if (key.getValue() == Key.UP)
        {
            setSelected(mSelected - 1);

            if (mSelected == -1)
            {
                if (mWrappingEnabled)
                {
                    setSelected(getListModel().getNumberOfElements() - 1);
                }
                else
                {
                    setSelected(0);
                }
            }

            keyEvent.consume();
        }
        else if (key.getValue() == Key.DOWN)
        {
            if (mWrappingEnabled
                && getSelected() == getListModel().getNumberOfElements() - 1)
            {
                setSelected(0);
            }
            else
            {
                setSelected(getSelected() + 1);
            }

            keyEvent.consume();
        }
        else if (key.getValue() == Key.HOME)
        {
            setSelected(0);
            keyEvent.consume();
        }
        else if (key.getValue() == Key.END)
        {
            setSelected(getListModel().getNumberOfElements() - 1);
            keyEvent.consume();
        }
    }

    // Inherited from MouseListener

    void mousePressed(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            setSelected(mouseEvent.getY() / getRowHeight());
//             distributeActionEvent();
        }
    }

    void mouseWheelMovedUp(MouseEvent mouseEvent)
    {
        if (isFocused())
        {
            if (getSelected() > 0 )
            {
                setSelected(getSelected() - 1);
            }

            mouseEvent.consume();
        }
    }

    void mouseWheelMovedDown(MouseEvent mouseEvent)
    {
        if (isFocused())
        {
            setSelected(getSelected() + 1);

            mouseEvent.consume();
        }
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        mouseEvent.consume();
    }

protected:

    /**
      * The selected item as an index in the list model.
      */
    int mSelected;

    /**
      * The list model to use.
      */
    IListModel mListModel;

    /**
      * True if wrapping is enabled, false otherwise.
      */
    bool mWrappingEnabled;


}
