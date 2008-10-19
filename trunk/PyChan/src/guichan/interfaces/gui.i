%module guichan

%{
#include <list>
#include <set>

#include <guichan/keyevent.hpp>
#include <guichan/mouseevent.hpp>
#include <guichan/mouseinput.hpp>
%}

namespace gcn
{
    class FocusHandler;
    class Graphics;
    class Input;
    class KeyListener;
    class Widget;

    class Gui
    {
    public:

        /**
         * Constructor.
         */
        Gui();

        /**
         * Destructor.
         */
        virtual ~Gui();

        /**
         * Sets the top widget. The top widget is the root widget
         * of the GUI. If you want a GUI to be able to contain more
         * than one widget the top widget should be a container.
         *
         * @param top The top widget.
         * @see Container
         * @since 0.1.0
         */
        virtual void setTop(Widget* top);

        /**
         * Gets the top widget. The top widget is the root widget
         * of the GUI.
         *
         * @return The top widget. NULL if no top widget has been set.
         * @since 0.1.0
         */
        virtual Widget* getTop() const;

        /**
         * Sets the graphics object to use for drawing.
         *
         * @param graphics The graphics object to use for drawing.
         * @see getGraphics, AllegroGraphics, HGEGraphics, 
         *      OpenLayerGraphics, OpenGLGraphics, SDLGraphics
         * @since 0.1.0
         */
        virtual void setGraphics(Graphics* graphics);

        /**
         * Gets the graphics object used for drawing.
         *
         *  @return The graphics object used for drawing. NULL if no
         *          graphics object has been set.
         * @see setGraphics, AllegroGraphics, HGEGraphics, 
         *      OpenLayerGraphics, OpenGLGraphics, SDLGraphics
         * @since 0.1.0
         */
        virtual Graphics* getGraphics() const;

        /**
         * Sets the input object to use for input handling.
         *
         * @param input The input object to use for input handling.
         * @see getInput, AllegroInput, HGEInput, OpenLayerInput,
         *      SDLInput
         * @since 0.1.0
         */
        virtual void setInput(Input* input);

        /**
         * Gets the input object being used for input handling.
         *
         *  @return The input object used for handling input. NULL if no
         *          input object has been set.
         * @see setInput, AllegroInput, HGEInput, OpenLayerInput,
         *      SDLInput
         * @since 0.1.0
         */
        virtual Input* getInput() const;

        /**
         * Performs logic of the GUI. By calling this function all logic
         * functions down in the GUI heirarchy will be called. When logic
         * is called for Gui, user input will be handled.
         *
         * @see Widget::logic
         * @since 0.1.0
         */
        virtual void logic();

        /**
         * Draws the GUI. By calling this funcion all draw functions
         * down in the GUI hierarchy will be called. When draw is called
         * the used Graphics object will be initialised and drawing of
         * the top widget will commence.
         *
         * @see Widget::draw
         * @since 0.1.0
         */
        virtual void draw();

        /**
         * Focuses none of the widgets in the Gui.
         *
         * @since 0.1.0
         */
        virtual void focusNone();

        /**
         * Sets tabbing enabled, or not. Tabbing is the usage of
         * changing focus by utilising the tab key.
         *
         * @param tabbing True if tabbing should be enabled, false
         *                otherwise.
         * @see isTabbingEnabled
         * @since 0.1.0
         */
        virtual void setTabbingEnabled(bool tabbing);

        /**
         * Checks if tabbing is enabled.
         *
         * @return True if tabbing is enabled, false otherwise.
         * @see setTabbingEnabled
         * @since 0.1.0
         */
        virtual bool isTabbingEnabled();

        /**
         * Adds a global key listener to the Gui. A global key listener
         * will receive all key events generated from the GUI and global
         * key listeners will receive the events before key listeners
         * of widgets.
         *
         * @param keyListener The key listener to add.
         * @see removeGlobalKeyListener
         * @since 0.5.0
         */
        virtual void addGlobalKeyListener(KeyListener* keyListener);

        /**
         * Removes global key listener from the Gui.
         *
         * @param keyListener The key listener to remove.
         * @throws Exception if the key listener hasn't been added.
         * @see addGlobalKeyListener
         * @since 0.5.0
         */
        virtual void removeGlobalKeyListener(KeyListener* keyListener);
    };
}

