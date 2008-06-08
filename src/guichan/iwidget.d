module guichan.iwidget;

import dlisp.bind;

class IWidget {
    mixin BindClass!("C/WIDGET-BASE");

    abstract bool isFocusable();
    abstract bool isModalFocused();
    abstract bool isTabOutEnabled();
    abstract bool isTabInEnabled();
};

interface IGraphics { };