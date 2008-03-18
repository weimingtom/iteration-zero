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

module guichan.listener;
import guichan.event;

/**
  * Interface for listening for events from widgets. When a widget's size,
  * location or visibility changes, the relevant method of the listener is
  * invoked.
  *
  * @see Widget::addWidgetListener, Widget::removeWidgetListener
  * @author Olof Naess�n
  * @since 0.8.0
  */
interface WidgetListener
{
    /**
      * Invoked when a widget changes its size.
      *
      * @param event Describes the event.
      * @since 0.8.0
      */
    void widgetResized(Event event);

    /**
      * Invoked when a widget is moved.
      *
      * @param event Describes the event.
      * @since 0.8.0
      */
    void widgetMoved(Event event);

    /**
      * Invoked when a widget is hidden, i.e it's set to be
      * not visible.
      *
      * @param event Describes the event.
      * @since 0.8.0
      */
    void widgetHidden(Event event);

    /**
      * Invoked when a widget is shown, i.e it's set to be
      * visible.
      *
      * @param event Describes the event.
      * @since 0.8.0
      */
    void widgetShown(Event event);
}

/**
  * Interface for listening for focus events from widgets.
  *
  * @see Widget::addFocusListener, Widget::removeFocusListener
  * @author Olof Naess�n
  * @since 0.7.0
  */
interface FocusListener
{
    /**
      * Called when a widget gains focus. 
      *
      * @param event Discribes the event.
      */
    void focusGained(Event event);

    /**
      * Called when a widget loses focus. 
      *
      * @param event Discribes the event.
      */
    void focusLost(Event event);
}

interface KeyListener
{
    /**
      * Called if a key is pressed when the widget has keyboard focus.
      * If a key is held down the widget will generate multiple key
      * presses.
      *
      * @param keyEvent Discribes the event.
      */
    void keyPressed(KeyEvent keyEvent);

    /**
      * Called if a key is released when the widget has keyboard focus.
      *
      * @param keyEvent Discribes the event.
      */
    void keyReleased(KeyEvent keyEvent);
}

/**
  * Interface for listening for mouse events from widgets.
  *
  * @see Widget::addMouseListener, Widget::removeMouseListener
  * @since 0.1.0
  */
interface MouseListener
{
    /**
      * Called when the mouse has entered into the widget area.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseEntered(MouseEvent mouseEvent);

    /**
      * Called when the mouse has exited the widget area.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseExited(MouseEvent mouseEvent);

    /**
      * Called when a mouse button has been pressed on the widget area.
      *
      * NOTE: A mouse press is NOT equal to a mouse click.
      *       Use mouseClickMessage to check for mouse clicks.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mousePressed(MouseEvent mouseEvent);

    /**
      * Called when a mouse button has been released on the widget area.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseReleased(MouseEvent mouseEvent);

    /**
      * Called when a mouse button is pressed and released (clicked) on
      * the widget area.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseClicked(MouseEvent mouseEvent);

    /**
      * Called when the mouse wheel has moved up on the widget area.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseWheelMovedUp(MouseEvent mouseEvent);

    /**
      * Called when the mouse wheel has moved down on the widget area.
      *
      * @param mousEvent Describes the event.
      * @since 0.6.0
      */
    void mouseWheelMovedDown(MouseEvent mouseEvent);

    /**
      * Called when the mouse has moved in the widget area and no mouse button
      * has been pressed (i.e no widget is being dragged).
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseMoved(MouseEvent mouseEvent);

    /**
      * Called when the mouse has moved and the mouse has previously been
      * pressed on the widget.
      *
      * @param mouseEvent Describes the event.
      * @since 0.6.0
      */
    void mouseDragged(MouseEvent mouseEvent);
}

/**
  * Interface for listening for selection events from widgets.
  *
  * @see ListBox::addSelectionListener,
  *      ListBox::removeSelectionListener,
  *      DropDown::addSelectionListener,
  *      DropDown::removeSelectionListener
  * @author Olof Naess�n
  * @since 0.8.0
  */
// interface SelectionListener
// {
//     /**
//       * Called when the value of a selection has been changed in a Widget.
//       * It is used to be able to recieve a notification that a value has
//       * been changed.
//       *
//       * @param event The event of the value change.
//       * @since 0.8.0
//       */
//     void valueChanged( SelectionEvent event);
// }
