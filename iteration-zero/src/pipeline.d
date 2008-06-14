module pipeline;

private {
    import derelict.opengl.gl;
    import derelict.opengl.glu;
}


class Renderer
{
    public:
        this(char[] _name)
        {
            name = _name;
        }

        void render()
        {
        }

        char[] name;
        bool enabled = true;
}


class RenderPipeline
{
    public:
        this()
        {
        }

        void render()
        {
            foreach(Renderer r; _pipeline)
            {
                if( r.enabled )
                    r.render();
            }
        }

        void add(Renderer r)
        {
            _byName[r.name] = r;
        }

        Renderer opIndex(char[] name)
        {
            return _byName[name];
        }

        void setPipeline(char[][] names)
        {
          _pipeline.length = 0;
          foreach(char[] name; names)
            _pipeline ~= _byName[name];
        }

    private:
        Renderer[] _pipeline;
        Renderer[char[]] _byName;
}