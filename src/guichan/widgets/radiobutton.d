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

module guichan.widgets.radiobutton;

import guichan.all;
import guichan.util;

/**
  * An implementation of a radio button where a user can select or deselect
  * the radio button and where the status of the radio button is displayed to the user.
  * A radio button can belong to a group and when a radio button belongs to a
  * group only one radio button can be selected in the group. A radio button is
  * capable of displaying a caption.
  * 
  * If a radio button's state changes an action event will be sent to all action 
  * listeners of the check box.
  */
class RadioButton : Widget
{
public:

    /**
      * Constructor.
      */
    this()
    {
        setSelected(false);

        setFocusable(true);
        addMouseListener(this);
        addKeyListener(this);
    }

    /**
      * Constructor. The radio button will be automatically resized
      * to fit the caption.
      *
      * @param caption The caption of the radio button.
      * @param group The group the radio button should belong to.
      * @param selected True if the radio button should be selected.
      */
    this(dstring caption, string group, bool selected = false)
    {
        setCaption(caption);
        setGroup(group);
        setSelected(selected);

        setFocusable(true);
        addMouseListener(this);
        addKeyListener(this);

        adjustSize();
    }

    /**
      * Checks if the radio button is selected.
      *
      * @return True if the radio button is selecte, false otherwise.
      * @see setSelected
      */
    bool isSelected()
    {
      return mSelected;
    }

    /**
      * Sets the radio button to selected or not.
      *
      * @param selected True if the radio button should be selected,
      *                 false otherwise.
      * @see isSelected
      */
    void setSelected(bool selected)
    {
        if (selected && mGroup != "")
        {
            RadioButton[] buttons = mGroupMap[mGroup];
            foreach(RadioButton button; buttons)
            {
                if (button.isSelected())
                {
                    button.setSelected(false);
                }
            }
        }

        mSelected = selected;
    }

    /**
      * Gets the caption of the radio button.
      *
      * @return The caption of the radio button.
      * @see setCaption
      */
    dstring getCaption()
    {
      return mCaption;
    }

    /**
      * Sets the caption of the radio button. It's advisable to call
      * adjustSize after setting of the caption to adjust the
      * radio button's size to fit the caption.
      *
      * @param caption The caption of the radio button.
      * @see getCaption, adjustSize
      */
    void setCaption(dstring caption)
    {
      mCaption = caption;
    }

    /**
      * Sets the group the radio button should belong to. Note that
      * a radio button group is unique per application, not per Gui object
      * as the group is stored in a static map.
      *
      * @param group The name of the group.
      * @see getGroup
      */
    void setGroup(string group)
    {
        if (mGroup != "")
        {
            mGroupMap[mGroup].remove_all(this);
        }

        if (group != "")
        {
            mGroupMap[group] ~= this;
        }

        mGroup = group;
    }

    /**
      * Gets the group the radio button belongs to.
      *
      * @return The group the radio button belongs to.
      * @see setGroup
      */
    string getGroup()
    {
        return mGroup;
    }

    /**
      * Adjusts the radio button's size to fit the caption.
      */
    void adjustSize()
    {
        int height = getFont().getHeight();

        setHeight(height);
        setWidth(getFont().getWidth(getCaption()) + height + height/2);
    }


    // Inherited from Widget

    void draw(Graphics graphics)
    {
        graphics.pushClipArea(Rectangle(1,
                                         1,
                                         getWidth() - 1,
                                         getHeight() - 1));
        drawBox(graphics);
        graphics.popClipArea();

        
        graphics.setFont(getFont());
        graphics.setColor(getForegroundColor());

        if (isFocused())
        {
            int fh;
            
            if (getHeight()%2 == 0)
            {
                fh = getHeight() - 4;
            }
            else
            {
                fh = getHeight() - 3;
            }

            int hh = (fh + 1) / 2;
        
            graphics.drawLine(0, hh + 1, hh + 1, 0);
            graphics.drawLine(hh + 2, 1, fh + 2, hh + 1);
            graphics.drawLine(fh + 1, hh + 2, hh + 1, fh + 2);
            graphics.drawLine(hh + 1, fh + 2, 1, hh + 2);            
        }
        
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
            setSelected(true);
//             distributeActionEvent();
            keyEvent.consume();
        }
    }


    // Inherited from MouseListener

    void mouseClicked(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            setSelected(true);
//             distributeActionEvent();
        }
    }

    void mouseDragged(MouseEvent mouseEvent)
    {
        mouseEvent.consume();
    }

protected:
    /**
      * Draws the box.
      *
      * @param graphics a Graphics object to draw with.
      */
    void drawBox(Graphics graphics)
    {
        int h;

        if (getHeight()%2 == 0)
        {
            h = getHeight() - 4;
        }
        else
        {
            h = getHeight() - 3;
        }

        graphics.setColor(getBackgroundColor());

        int i;
        int hh = (h + 1) / 2;

        for (i = 1; i <= hh; ++i)
        {
            graphics.drawLine(hh - i + 1,
                               i,
                               hh + i - 1,
                               i);
        }

        for (i = 1; i < hh; ++i)
        {
            graphics.drawLine(hh - i + 1,
                               h - i,
                               hh + i - 1,
                               h - i);
        }

        graphics.setColor(getFrameColor());
        graphics.drawLine(hh, 0, 0, hh);
        graphics.drawLine(hh + 1, 1, h - 1, hh - 1);

        graphics.drawLine(1, hh + 1, hh, h);
        graphics.drawLine(hh + 1, h - 1, h, hh);

        graphics.setColor(getForegroundColor());

        int hhh = hh - 3;
        if (mSelected)
        {
            for (i = 0; i < hhh; ++i)
            {
                graphics.drawLine(hh - i, 4 + i, hh + i, 4 + i);
            }
            for (i = 0; i < hhh; ++i)
            {
                graphics.drawLine(hh - i, h - 4 - i, hh + i, h - 4 -  i);
            }

        }
    }

    /**
      * True if the radio button is selected, false otherwise.
      */
    bool mSelected;

    /**
      * Holds the caption of the radio button.
      */ 
    dstring mCaption;

    /**
      * Holds the group of the radio button.
      */
    string mGroup;

    /**
      * Holds all available radio button groups.
      */
    static RadioButton[][string] mGroupMap;
}
