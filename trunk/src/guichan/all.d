module guichan.all;

public import
  guichan.gui,
  guichan.widget,
  guichan.input,
  guichan.graphics,
  guichan.font,
  guichan.listener,
  guichan.event,
  guichan.exception,
  guichan.rectangle,
  guichan.color,
  guichan.mouseinput,
  guichan.keyinput,
  guichan.key;

private import
    guichan.widgets.all,
    guichan.basiccontainer,
    dlisp.bind
    ;

void bindGuichan(Environment env)
{
    Widget.bindClass(env);
    BasicContainer.bindClass(env);
    Container.bindClass(env);
    Gui.bindClass(env);

    Button.bindClass(env);
}