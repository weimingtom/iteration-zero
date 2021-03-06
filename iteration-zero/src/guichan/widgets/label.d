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

module guichan.widgets.label;

private import guichan.all;
private import dlisp.bind;

class Label : Widget
{
    this()
    {
        mAlignment = Alignment.LEFT;
    }

    this(dstring caption)
    {
        mCaption = caption;
        mAlignment = Alignment.LEFT;

        setWidth(getFont().getWidth(caption));
        setHeight(getFont().getHeight());
    }

    dstring getCaption()
    {
        return mCaption;
    }

    void setCaption(dstring caption)
    {
        mCaption = caption;
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
        int textX;
        int textY = getHeight() / 2 - getFont().getHeight() / 2;

        switch (getAlignment())
        {
          case Alignment.LEFT:
              textX = 0;
              break;
          case Alignment.CENTER:
              textX = getWidth() / 2;
              break;
          case Alignment.RIGHT:
              textX = getWidth();
              break;
          default:
              throw new GCN_Exception("Unknown alignment.");
        }

        graphics.setFont(getFont());
        graphics.setColor(getForegroundColor());
        graphics.drawText(getCaption(), textX, textY, getAlignment());
    }

    void adjustSize()
    {
        setWidth(getFont().getWidth(getCaption()));
        setHeight(getFont().getHeight());
    }

    mixin BindClass!("C/LABEL");
    mixin BindMethods!(setCaption,getCaption);

  protected:
  /**
    * Holds the caption of the label.
    */
  dstring mCaption;

  /**
    * Holds the alignment of the caption.
    */
  Alignment mAlignment;

}
