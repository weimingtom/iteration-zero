%{
%}

namespace gcn
{
    /**
     * Represents a key or a character.
     */
    class Key
    {
    public:

        /**
         * Constructor.
         *
         * @param value The ascii or enum value for the key.
         */
        Key(int value = 0);

        /**
         * Checks if a key is a character.
         *
         * @return True if the key is a letter, number or whitespace,
         *         false otherwise.
         */
        bool isCharacter() const;

        /**
         * Checks if a key is a number.
         *
         * @return True if the key is a number (0-9),
         *         false otherwise.
         */
        bool isNumber() const;

        /**
         * Checks if a key is a letter.
         *
         * @return True if the key is a letter (a-z,A-Z),
         *         false otherwise.
         */
        bool isLetter() const;

        /**
         * Gets the value of the key. If an ascii value exists it 
         * will be returned. Otherwise an enum value will be returned.
         *
         * @return the value of the key.
         */
        int getValue() const;

        /**
         * Compares two keys.
         *
         * @param key The key to compare this key with.
         * @return True if the keys are equal, false otherwise.
         */
        bool operator==(const Key& key) const;

        /**
         * Compares two keys.
         *
         * @param key The key to compare this key with.
         * @return True if the keys are not equal, false otherwise.
         */
        bool operator!=(const Key& key) const;

        /**
         * An enum with key values.
         */
        enum
        {
            Space              = ' ',
            Tab                = '\t',
            Enter              = '\n',
            LeftAlt            = 1000,
            RightAlt,
            LeftShift,
            RightShift,
            LeftControl,
            RightControl,
            LeftMeta,
            RightMeta,
            LeftSuper,
            RightSuper,
            Insert,
            Home,
            PageUp,
            Delete,
            End,
            PageDown,
            Escape,
            CapsLock,
            Backspace,
            F1,
            F2,
            F3,
            F4,
            F5,
            F6,
            F7,
            F8,
            F9,
            F10,
            F11,
            F12,
            F13,
            F14,
            F15,
            PrintScreen,
            ScrollLock,
            Pause,
            NumLock,
            AltGr,
            Left,
            Right,
            Up,
            Down
        };

    };
}

