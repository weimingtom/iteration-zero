module iwidget;

import dlisp.bind;

class IWidget {
    mixin BindClass!("C/WIDGET-BASE");
};