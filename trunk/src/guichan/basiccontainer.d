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

module guichan.basiccontainer;

import guichan.widget;
import guichan.event;
import guichan.rectangle;
import guichan.exception;
import guichan.focushandler;
import guichan.graphics;
import guichan.mouseinput;
import guichan.util;

class BasicContainer : Widget 
{
    this()
    {
    }

    ~this()
    {
        clear();
    }

    void moveToTop(Widget widget)
    {
        foreach (int i, Widget w; mWidgets)
        {
            if (w is widget)
            {
                mWidgets[i] = mWidgets[$-1];
                mWidgets[$-1] = w;
                return;
            }
        }

        throw new GCN_Exception("There is no such widget in this container.");
    }

    override void moveToBottom(Widget widget)
    {
        foreach (int i, Widget w; mWidgets)
        {
            if (w is widget)
            {
                mWidgets[i] = mWidgets[0];
                mWidgets[0] = w;
                return;
            }
        }
        throw new GCN_Exception("There is no such widget in this container.");
    }

    void death(Event event)
    {
        assert(0);
/+
        WidgetListIterator iter;
        iter = find(mWidgets.begin(), mWidgets.end(), event.getSource());

        if (iter == mWidgets.end())
        {
            throw GCN_EXCEPTION("There is no such widget in this container.");
        }

        mWidgets.erase(iter);+/
    }

    Rectangle getChildrenArea()
    {
        return new Rectangle(0, 0, getWidth, getHeight);
    }

    void focusNext()
    {
        assert(0);
/+
        WidgetListIterator it;

        for (it = mWidgets.begin(); it != mWidgets.end(); it++)
        {
            if ((*it)->isFocused())
            {
                break;
            }
        }

        WidgetListIterator end = it;

        if (it == mWidgets.end())
        {
            it = mWidgets.begin();
        }

        it++;

        for ( ; it != end; it++)
        {
            if (it == mWidgets.end())
            {
                it = mWidgets.begin();
            }

            if ((*it)->isFocusable())
            {
                (*it)->requestFocus();
                return;
            }
        }+/
    }

    void focusPrevious()
    {
        assert(0);
/+        WidgetListReverseIterator it;

        for (it = mWidgets.rbegin(); it != mWidgets.rend(); it++)
        {
            if ((*it)->isFocused())
            {
                break;
            }
        }

        WidgetListReverseIterator end = it;

        it++;

        if (it == mWidgets.rend())
        {
            it = mWidgets.rbegin();
        }

        for ( ; it != end; it++)
        {
            if (it == mWidgets.rend())
            {
                it = mWidgets.rbegin();
            }

            if ((*it)->isFocusable())
            {
                (*it)->requestFocus();
                return;
            }
        }+/
    }

    Widget getWidgetAt(int x, int y)
    {
        Rectangle r = getChildrenArea();

        if (!r.isPointInRect(x, y))
        {
            return null;
        }

        x -= r.x;
        y -= r.y;
        assert(0);

//         WidgetListReverseIterator it;
//         for (it = mWidgets.rbegin(); it != mWidgets.rend(); it++)
//         {
//             if ((*it)->isVisible() && (*it)->getDimension().isPointInRect(x, y))
//             {
//                 return (*it);
//             }
//         }

        return null;
    }

    void logic()
    {
        logicChildren();
    }

    void _setFocusHandler(FocusHandler focusHandler)
    {
        super._setFocusHandler(focusHandler);

        if (mInternalFocusHandler !is null)
        {
            return;
        }

        foreach(Widget widget; mWidgets)
        {
            widget._setFocusHandler(focusHandler);
        }
    }

    void add(Widget widget)
    {
        mWidgets ~= widget;
        assert(0);

        if (mInternalFocusHandler is null)
        {
            widget._setFocusHandler(_getFocusHandler);
        }
        else
        {
            widget._setFocusHandler(mInternalFocusHandler);
        }

        widget._setParent(this);
//         widget->addDeathListener(this);
    }

    void remove(Widget widget)
    {
        assert(0);
/+        WidgetListIterator iter;
        for (iter = mWidgets.begin(); iter != mWidgets.end(); iter++)
        {
            if (*iter == widget)
            {
                mWidgets.erase(iter);
                widget->_setFocusHandler(NULL);
                widget->_setParent(NULL);
                widget->removeDeathListener(this);
                return;
            }
        }

        throw GCN_EXCEPTION("There is no such widget in this container.");+/
    }

    void clear()
    {
        foreach(Widget widget; mWidgets)
        {
            widget._setFocusHandler(null);
            widget._setParent(null);
//             widget.removeDeathListener(this);
        }
        mWidgets.length = 0;
    }

    void drawChildren(Graphics graphics)
    {
        graphics.pushClipArea(getChildrenArea);

        foreach (Widget widget; mWidgets)
        {
            if (widget.isVisible())
            {
                // If the widget has a frame,
                // draw it before drawing the widget
                if (widget.getFrameSize() > 0)
                {
                    Rectangle rec = new Rectangle(widget.getDimension());
                    rec.x -= widget.getFrameSize();
                    rec.y -= widget.getFrameSize();
                    rec.width += 2 * widget.getFrameSize();
                    rec.height += 2 * widget.getFrameSize();
                    graphics.pushClipArea(rec);
                    widget.drawFrame(graphics);
                    graphics.popClipArea();
                }

                graphics.pushClipArea(new Rectangle(widget.getDimension()));
                widget.draw(graphics);
                graphics.popClipArea();
            }
        }
        graphics.popClipArea;
    }

    void logicChildren()
    {
        foreach(Widget widget; mWidgets)
          widget.logic;
    }

    void showWidgetPart(Widget widget, Rectangle area)
    {
        Rectangle widgetArea = getChildrenArea;

        area.x += widget.getX;
        area.y += widget.getY;
        
        if (area.x + area.width > widgetArea.width)
        {
            widget.setX(widget.getX - area.x - area.width + widgetArea.width);
        }

        if (area.y + area.height > widgetArea.height)
        {
            widget.setY(widget.getY - area.y - area.height + widgetArea.height);
        }

        if (area.x < 0)
        {
            widget.setX(widget.getX - area.x);
        }

        if (area.y < 0)
        {
            widget.setY(widget.getY - area.y);
        }
    }


    void setInternalFocusHandler(FocusHandler focusHandler)
    {
        Widget.setInternalFocusHandler(focusHandler);

        foreach(Widget widget; mWidgets)
        {
            if (mInternalFocusHandler == null)
            {
                widget._setFocusHandler(_getFocusHandler);
            }
            else
            {
                widget._setFocusHandler(mInternalFocusHandler);
            }
        }
    }

    Widget findWidgetById(string id)
    {
        foreach(Widget widget; mWidgets)
        {
            if (widget.getId == id)
            {
                return widget;
            }
            
            BasicContainer basic = cast(BasicContainer)(widget);
            
            if (basic !is null)
            {
                Widget w = basic.findWidgetById(id);
                if (w !is null)
                {
                    return w;
                }
            }
        }
        return null;
    }
  protected:
    Widget[] mWidgets;
}
