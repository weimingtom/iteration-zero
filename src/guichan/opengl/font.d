module guichan.opengl.font;

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
      float[4] color = graphics.getColor.toFloatVector;
      float[2] posi = [cast(float)x,cast(float)y];
      _realFont.draw([text],posi,color);
  }

  private:
    RealFont.Font _realFont;
}
