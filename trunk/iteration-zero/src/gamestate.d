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
    

    void start() { startCallback(); }
    void stop() { stopCallback(); }
    void logic() {logicCallback();}

    void keyPressed(KeyEvent event) {keyPressedCallback(event);}
    void keyReleased(KeyEvent event) {keyReleasedCallback(event);}

    void mouseEntered(MouseEvent mouseEvent) {mouseEnteredCallback(mouseEvent); }
    void mouseExited(MouseEvent mouseEvent) {mouseExitedCallback(mouseEvent); }
    void mousePressed(MouseEvent mouseEvent) {mousePressedCallback(mouseEvent); }
    void mouseReleased(MouseEvent mouseEvent) {mouseReleasedCallback(mouseEvent); }
    void mouseClicked(MouseEvent mouseEvent) {mouseClickedCallback(mouseEvent); }

    void mouseWheelMovedUp(MouseEvent mouseEvent) {mouseWheelMovedUpCallback(mouseEvent); }
    void mouseWheelMovedDown(MouseEvent mouseEvent) {mouseWheelMovedDownCallback(mouseEvent); }
    void mouseMoved(MouseEvent mouseEvent) {mouseMovedCallback(mouseEvent); }
    void mouseDragged(MouseEvent mouseEvent) {mouseDraggedCallback(mouseEvent); }

    mixin BindConstructor!(GameState function(string));
    mixin BindHandlers!(start,stop,logic);
    mixin BindHandlers!(keyPressed,keyReleased);
    mixin BindHandlers!(mouseEntered,mouseExited,mousePressed,mouseReleased,mouseClicked);
    mixin BindHandlers!(mouseWheelMovedUp,mouseWheelMovedDown,mouseMoved,mouseDragged);
    private:
        string _name;
}
