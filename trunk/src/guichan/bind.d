module guichan.bind;

private import
    guichan.widgets.all,
    guichan.basiccontainer,
    guichan.all,
    guichan.opengl.font,
    dlisp.bind;

void bindGuichan(Environment env)
{
    Widget.bindClass(env);
    Window.bindClass(env);
    BasicContainer.bindClass(env);
    Container.bindClass(env);
    Button.bindClass(env);
    Label.bindClass(env);

    Gui.bindClass(env);
    Event.bindClass(env);
    OpenGLFont.bindClass(env);

}