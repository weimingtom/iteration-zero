module gamestate;

import guichan.event;
import guichan.key;

import dlisp.bind;

class GameState
{
    mixin BindClass!("C/Game-State");
    
    this(string name)
    {
        _name = name;
    }

    string name() { return _name; }
    

    void start() {}
    void stop() {}
    void logic() {}

    void keyPressed(KeyEvent event) {}
    void keyReleased(KeyEvent event) {}

    void mouseEntered(MouseEvent mouseEvent) { }
    void mouseExited(MouseEvent mouseEvent) { }
    void mousePressed(MouseEvent mouseEvent) { }
    void mouseReleased(MouseEvent mouseEvent) { }
    void mouseClicked(MouseEvent mouseEvent) { }

    void mouseWheelMovedUp(MouseEvent mouseEvent) { }
    void mouseWheelMovedDown(MouseEvent mouseEvent) { }
    void mouseMoved(MouseEvent mouseEvent) { }
    void mouseDragged(MouseEvent mouseEvent) { }

    mixin BindConstructor!(GameState function(string));

    private:
        string _name;
}
