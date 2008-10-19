%{
%}

namespace gcn
{
    class KeyInput;
    class MouseInput;

    /**
     * Abstract class for providing functions for user input. 
     *
     * Guichan contains implementations of Input for common 
     * libraries like the Allegro library, the HGE library, 
     * and the SDL library. 
     * To make Guichan usable with other libraries, an Input 
     * class must be implemented.
     *
     * @see AllegroInput, HGEInput, OpenLayerInput, 
     *      SDLInput
     */
    class  Input
    {
    public:

        /**
         * Destructor.
         */
        virtual ~Input(){ }

        /**
         * Checks if the key queue is empty, or not.
         *
         * @return True if the key queue is empty, 
         *         false otherwise.
         */
        virtual bool isKeyQueueEmpty() = 0;

        /**
         * Dequeues the key input queue.
         *
         * @return The first key input in the key input queue.
         */
        virtual KeyInput dequeueKeyInput() = 0;

        /**
         * Checks if the mouse queue is empyt, or not.
         *
         * @return True if the mouse queue is empty,
         *         false otherwise.
         */
        virtual bool isMouseQueueEmpty() = 0;

        /**
         * Dequeues the mouse input queue.
         *
         * @return The first mouse input in the mouse input queue.
         */
        virtual MouseInput dequeueMouseInput() = 0;

        /**
         * Polls all exsisting input. Called when input should
         * be polled. The function exists for compatibility reason
         * where some libraries need to poll input at a certain
         * logic rate.
         */
        virtual void _pollInput() = 0;
    };
}

