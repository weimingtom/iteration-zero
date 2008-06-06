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

module guichan.widgets.button;

import std.stdio : writefln;

private import guichan.all;
private import dlisp.bind;

/**
  * An implementation of a regular clickable button. A button is capable of
  * displaying a caption. 
  *
  * If a button is clicked an action event will be sent to all action listener's 
  * of the button.
  *
  * @see ImageButton
  */
class Button : Widget
{
  public:
    mixin BindClass!("Button");

    /**
      *Constructor.
      */
    this()
    {
      super();

      mHasMouse = false;
      mKeyPressed = false;
      mMousePressed = false;
      mAlignment = Alignment.CENTER;
      mSpacing = 4;

      setFocusable(true);
      adjustSize();
      setFrameSize(2);

      addMouseListener(this);
      addKeyListener(this);
      addFocusListener(this);
    }

    /**
      *Constructor. The button will be automatically resized
      * to fit the caption.
      *
      * @param caption The caption of the button.
      */
    this(dstring caption)
    {
      super();

      mCaption = caption;
      mHasMouse = false;
      mKeyPressed = false;
      mMousePressed = false;
      mAlignment = Alignment.CENTER;
      mSpacing = 4;

      setFocusable(true);
      adjustSize();
      setFrameSize(2);

      addMouseListener(this);
      addKeyListener(this);
      addFocusListener(this);
    }


    /**
      * Sets the caption of the button. It's advisable to call
      * adjustSize after setting of the caption to adjust the
      * button's size to fit the caption.
      *
      * @param caption The caption of the button.
      * @see getCaption, adjustSize
      */
    void setCaption(dstring caption)
    {
        mCaption = caption;
    }

    /**
      * Gets the caption of the button.
      *
      * @return The caption of the button.
      */
    dstring getCaption()
    {
        return mCaption;
    }

    /**
      * Sets the alignment of the caption. The alignment is relative
      * to the center of the button.
      *
      * @param alignment The alignment of the caption.
      * @see getAlignment, Graphics
      */
    void setAlignment(Alignment alignment)
    {
        mAlignment = alignment;
    }

    /**
      * Gets the alignment of the caption.
      *
      * @return The alignment of the caption.
      * @see setAlignment, Graphics
      */
    Alignment getAlignment()
    {
        return mAlignment;
    }

    /**
      * Sets the spacing between the border of the button and its caption.
      *
      * @param spacing The default value for spacing is 4 and can be changed 
      *                using this method.
      * @see getSpacing
      */
    void setSpacing(uint spacing)
    {
        mSpacing = spacing;
    }


    /**
      * Gets the spacing between the border of the button and its caption.
      *
      * @return spacing.
      * @see setSpacing
      */
    uint getSpacing()
   {
      return mSpacing;
    }

 
    /**
      * Adjusts the button's size to fit the caption.
      */
    void adjustSize()
    {
        setWidth(getFont().getWidth(mCaption) + 4*mSpacing + 2*mFrameSize);
        setHeight(getFont().getHeight() + 2*mSpacing + 2*mFrameSize);
    }


        //Inherited from Widget

   void draw(Graphics graphics)
   {
        graphics.setColor(getFrameColor());
        graphics.fillRectangle(Rectangle(0, 0, getWidth(), getHeight()));

        graphics.setColor(getBackgroundColor());
        graphics.fillRectangle(Rectangle(getFrameSize, getFrameSize, getWidth()-getFrameSize*2, getHeight()-getFrameSize*2));

        int textX;
        int textY = (getHeight() - getFont().getHeight())/2 - mFrameSize;

        switch (getAlignment())
        {
          case Alignment.LEFT:
              textX = mSpacing;
              break;
          case Alignment.CENTER:
              textX = getWidth() / 2;
              break;
          case Alignment.RIGHT:
              textX = getWidth() - mSpacing;
              break;
          default:
              throw new GCN_Exception("Unknown alignment.");
        }

        graphics.setFont(getFont());
        graphics.setColor(getForegroundColor());
        if( isFocused() )
        {
          graphics.setColor(getSelectionColor());
        }

        if (isPressed())
        {
            graphics.drawText(getCaption(), textX + 1, textY + 1, getAlignment());
        }
        else
        {
            graphics.drawText(getCaption(), textX, textY, getAlignment());
        }
    }


    // Inherited from FocusListener

    void focusLost(Event event)
    {
        mMousePressed = false;
        mKeyPressed = false;
    }

    // Inherited from MouseListener

    void mousePressed(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            mMousePressed = true;
            mouseEvent.consume();
        }
        super.mousePressed(mouseEvent);
    }


    void mouseReleased(MouseEvent mouseEvent)
    {
        if (mouseEvent.getButton() == MouseEvent.LEFT
            && mMousePressed
            && mHasMouse)
        {
            mMousePressed = false;
//             distributeActionEvent();
            mouseEvent.consume();
        }
        else if (mouseEvent.getButton() == MouseEvent.LEFT)
        {
            mMousePressed = false;
            mouseEvent.consume();
        }
        super.mouseReleased(mouseEvent);
    }


    void mouseEntered(MouseEvent mouseEvent)
    {
//         writefln("mouseEntered");
        mHasMouse = true;
        super.mouseEntered(mouseEvent);
    }

    void mouseExited(MouseEvent mouseEvent)
    {
//         writefln("mouseExited");
        mHasMouse = false;
        super.mouseExited(mouseEvent);
    }


    void mouseDragged(MouseEvent mouseEvent)
    {
//         writefln("mouseDragged");
        mouseEvent.consume();
        super.mouseDragged(mouseEvent);
    }

    // Inherited from KeyListener

    void keyPressed(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if (key.getValue() == Key.ENTER
            || key.getValue() == Key.SPACE)
        {
            mKeyPressed = true;
            keyEvent.consume();
        }
        super.keyPressed(keyEvent);
    }

    void keyReleased(KeyEvent keyEvent)
    {
        Key key = keyEvent.getKey();

        if ((key.getValue() == Key.ENTER
             || key.getValue() == Key.SPACE)
             && mKeyPressed)
        {
            mKeyPressed = false;
            distributeActionEvent();
            keyEvent.consume();
        }
        super.keyReleased(keyEvent);
    }

    mixin BindMethods!(setCaption, getCaption);

  protected:
    /**
      * Checks if the button is pressed. Convenient method to use
      * when overloading the draw method of the button.
      *
      * @return True if the button is pressed, false otherwise.
      */
    bool isPressed()
    {
        if (mMousePressed)
        {
            return mHasMouse;
        }
        else
        {
            return mKeyPressed;
        }
    }

      /**
        * Holds the caption of the button.
        */
      dstring mCaption;

      /**
        * True if the mouse is ontop of the button, false otherwise.
        */
      bool mHasMouse;

      /**
        * True if a key has been pressed, false otherwise.
        */
      bool mKeyPressed;

      /**
        * True if a mouse has been pressed, false otherwise.
        */
      bool mMousePressed;

      /**
        * Holds the alignment of the caption.
        */
      Alignment mAlignment;

      /**
        * Holds the spacing between the border and the caption.
        */
      uint mSpacing;
}
