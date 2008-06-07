module guichan.bind;

private import
    guichan.widgets.all,
    guichan.basiccontainer,
    guichan.all,
    dlisp.bind;

void bindGuichan(Environment env)
{
    Widget.bindClass(env);
    BasicContainer.bindClass(env);
    Container.bindClass(env);
    Gui.bindClass(env);
    Event.bindClass(env);

    Button.bindClass(env);
}