module guichan.opengl.font;

import std.stdio : writefln;

/// Use in Guichan
import guichan.font;
import guichan.graphics;
import guichan.iwidget;

import RealFont = font;

private import dlisp.bind;

class OpenGLFont : guichan.font.Font
{
  this(string filename, int size)
  {
    _realFont = new RealFont.Font(filename,size);
  }

  int getWidth(dstring text)
  {
    return cast(int)_realFont.getWidth(text);
  }

  int getHeight()
  {
    return _realFont.getHeight();
  }

  void drawString(IGraphics graphics, dstring text, int x, int y)
  {
      // writefln("x,y =",x,",",y);
      x += (cast(Graphics)graphics).getCurrentClipArea.xOffset;
      y += (cast(Graphics)graphics).getCurrentClipArea.yOffset;
      float[4] color = (cast(Graphics)graphics).getColor.toFloatVector;
      float[2] posi = [cast(float)x,cast(float)y];
      _realFont.draw([text],posi,color);
  }

  mixin BindClass!("C/GL-FONT");
  mixin BindConstructor!(OpenGLFont function(string,int));
  mixin BindMethods!(getWidth,getHeight);

  private:
    RealFont.Font _realFont;
}
