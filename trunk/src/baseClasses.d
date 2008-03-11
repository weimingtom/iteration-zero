module baseClasses;

private
{
    import std.signals;
}

class Signal_GObjectArrivedAt { mixin Signal!(GObjectBase,int,int); }

class GObjectBase
{
    Signal_GObjectArrivedAt arrivedAt;

    this()
    {
        arrivedAt = new Signal_GObjectArrivedAt();
    }
}

interface IGObject
{
}

interface ILevel
{
    bool isBlocked(int x, int y);
}