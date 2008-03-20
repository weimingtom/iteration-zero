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

module guichan.widgets.checkbox;

import guichan.all;


/**
  * An implementation of a check box where a user can select or deselect
  * the check box and where the status of the check box is displayed to the user.
  * A check box is capable of displaying a caption. 
  * 
  * If a check box's state changes an action event will be sent to all action 
  * listeners of the check box.
  */
class CheckBox : Widget
{
public:

    /**
      * Contructor.
      */
    this()
    {
        setSelected(false);

        setFocusable(true);
        addMouseListener(this);
        addKeyListener(this);
    }

    /**
      * Constructor. The check box will be automatically resized
      * to fit the caption.
      *
      * @param caption The caption of the check box.
      * @param marked True if the check box is selected, false otherwise.
      */
    this(dstring caption, bool selected = false)
    {
        setCaption(caption);
        setSelected(selected);

        setFocusable(true);
        addMouseListener(this);
        addKeyListener(this);

        adjustSize();
    }

    /**
      * Checks if the check box is selected.
      *
      * @return True if the check box is selected, false otherwise.
      * @see setSelected
      */
    bool isSelected()
    {
      return mSelected;
    }

    /**
      * Sets the check box to be selected or not.
      *
      * @param selected True if the check box should be set as selected.
      * @see isSelected
      */
    void setSelected(bool selected)
    {
      mSelected = selected;
    }

    /**
      * Gets the caption of the check box.
      *
      * @return The caption of the check box.
      * @see setCaption
      */
    dstring getCaption()
    {
      return mCaption;
    }

    /**
      * Sets the caption of the check box. It's advisable to call
      * adjustSize after setting of the caption to adjust the
      * check box's size to fit the caption.
      *
      * @param caption The caption of the check box.
      * @see getCaption, adjustSize
      */
    void setCaption(dstring caption)
    {
      mCaption = caption;
    }

    /**
      * Adjusts the check box's size to fit the caption.
      */
    void adjustSize()
    {
        int height = getFont().getHeight();

        setHeight(height + 2);
        setWidth(getFont().getWidth(mCaption) + height + height / 2);
    }


    // Inherited from Widget

    void draw(Graphics graphics)
    {
        drawBox(graphics);

        graphics.setFont(getFont());
        graphics.setColor(getForegroundColor());

        int h = getHeight() + getHeight() / 2;

        graphics.drawText(getCaption(), h - 2, 0,Alignment.LEFT);
    }


    // Inherited from KeyListener

    void keyPressed(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if (key.getValue() == Key.ENTER ||
            key.getValue() == Key.SPACE)
        {
            toggleSelected();
            keyEvent.consume();
        }
    }


    // Inherited from MouseListener

    void mouseClicked(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            toggleSelected();
        }
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        mouseEvent.consume();
    }


protected:
    /**
      * Draws the box of the check box. 
      *
      * @param graphics A Graphics object to draw with.
      */
    void drawBox(Graphics graphics)
    {
        int h = getHeight() - 2;

        if (isFocused())
        {
            graphics.fillRectangle(Rectangle(0, 0, h+1, h+1));
        }
        graphics.setColor(getFrameColor());
        if (isSelected())
        {
            graphics.setColor(getForegroundColor());
        }
        graphics.fillRectangle(Rectangle(1, 1, h-1, h-1));

    }

    /**
      * Toggles the check box between being selected and
      * not being selected.
      */
    void toggleSelected()
    {
        mSelected = !mSelected;
//         distributeActionEvent();
    }

    /**
      * True if the check box is selected, false otherwise.
      */
    bool mSelected;

    /**
      * Holds the caption of the check box.
      */
    dstring mCaption;
}
