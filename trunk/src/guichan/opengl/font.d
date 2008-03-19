module guichan.opengl.font;

import std.stdio : writefln;

/// Use in Guichan
import guichan.font;
import guichan.graphics;

import RealFont = font;

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

  void drawString(Graphics graphics, dstring text, int x, int y)
  {
      // writefln("x,y =",x,",",y);
      x += graphics.getCurrentClipArea.xOffset;
      y += graphics.getCurrentClipArea.yOffset;
      float[4] color = graphics.getColor.toFloatVector;
      float[2] posi = [cast(float)x,cast(float)y];
      _realFont.draw([text],posi,color);
  }

  private:
    RealFont.Font _realFont;
}
