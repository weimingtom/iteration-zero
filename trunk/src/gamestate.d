module gamestate;

import guichan.event;
import guichan.key;

import dlisp.bind;

abstract class GameState
{
    mixin BindClass!("GameState");
    
    string name();
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
}
