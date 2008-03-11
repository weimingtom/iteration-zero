module Sofu.Exception;

class SofuException : Exception
{
    this(char[] msg)
    {
        super(msg);
    }
}
