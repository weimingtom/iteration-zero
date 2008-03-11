/*
  ---------------------------------------------------------------------
  luigi/example1.d -- An example program using the Luigi user
                      interface library.

  An example/test program for Luigi.
  Uses the GLFW windowing toolkit via luigi.adapter.glfw.
  ---------------------------------------------------------------------
  Copyright (C) 2006 William V. Baxter III

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  William Baxter wbaxter@gmail.com

  ---------------------------------------------------------------------
  Written in the D programming language (http://www.digitalmars.com/d)
  ---------------------------------------------------------------------
*/
import luigui = luigi.gui;
import luigi.adapters.glfw;
static import luigi.themes.std;
static import luigi.themes.dxut;

import derelict.glfw.glfw;
import derelict.opengl.gl;
import derelict.opengl.glu;
import std.string;
import std.stdio : writefln;

static this()
{
	DerelictGL.load();
	DerelictGLU.load();
	DerelictGLFW.load();
    
    // Initialise GLFW
    glfwInit();
}

static ~this()
{
    // Close OpenGL window and terminate GLFW
    glfwTerminate();
}

void usage(char[] name)
{
    writefln("Usage: %s [-nosync]", name);
}

luigui.Overlay create_gui(char[] theme)
{
    luigui.Overlay gui;
    with(luigui) {
        Luigi().adapter = GLFWAdapter();
        if (theme == "dxut" || theme == "dx") {
            Luigi().add_resource_location("..");
            Luigi().theme = new luigi.themes.dxut.DXUTTheme();
        }
        gui = new Overlay;
    
        gui.arranger = new BorderArranger(Gaps(5));

        alias BorderArranger.Region Region;
        alias Alignment A;

        auto eastGroup = gui.add_arranged(new GridPanel(1,0, Gaps(2)), Region.East);
        auto westGroup = gui.add_arranged(new GridPanel(2,0, Gaps(2)), Region.West);
        auto northGroup = gui.add_arranged(new FlowPanel, Region.North);
        auto southGroup = gui.add_arranged(new FlowPanel, Region.South);

        auto rgrp = new RadioGroup;

        with (westGroup) {
            auto b20 = add_arranged(new Button("Tool0"));
            auto b21 = add_arranged(new Button("Tool1"));
            auto b22 = add_arranged(new Button("Tool2"));
            auto b23 = add_arranged(new Button("Tool3"));
            auto b24 = add_arranged(new Button("LongNameTool4"));
            auto b25 = add_arranged(new Button("Tool5"));
            auto b26 = add_arranged(new Button("Tool6"));
            auto b27 = add_arranged(new Button("Tool7"));
            auto b28 = add_arranged(new Button("Tool8"));
            auto b29 = add_arranged(new Button("Tool9"));

            rgrp.multiadd(b20,b21,b22,b23,b24,b25,b26,b27,b28,b29);
            rgrp.value = 0;
            rgrp.selection_changed ~= (int t) {
                writefln("Tool %s now selected", t);
            };

            auto s01 = add_arranged(new Slider);
            auto s02 = add_arranged(new Slider);
            s02.vertical = true;
            s01.precision = 0.1;
            s02.precision = 0.1;

            s01.value_changed.connect2nd( (double d){ writefln("Slider now %s",d); } );
            s02.value_changed.connect2nd( (double d){ writefln("Slider now %s",d); } );
            s02.value_changed.connect2nd(&s01.value);
            s01.value_changed.connect2nd(&s02.value);

/*
            auto sb01 = add_arranged(new ScrollBar);
            auto sb02 = add_arranged(new ScrollBar);
            sb02.vertical = true;
            sb01.precision = 1;
            sb02.precision = 1;
*/
        }

        with(eastGroup) {
            auto b0 = add_widget(new Button("Luigi"));  b0.disable();
            auto b1 = add_widget(new Button("Is")); rgrp.add(b1,5);
            auto b2 = add_widget(new Button("My")); rgrp.add(b2,7);
            auto b3 = add_widget(new Button("Pal"));rgrp.add(b3,9);
            auto b4 = add_widget(new Button("Hide West")); 
            b4.is_toggle = true;
            b4.clicked.connect( &westGroup.toggle_shown );
            b4.value_changed ~= (Widget w, bool v){
                Button b = cast(Button)w; assert(b);
                if (v) b.label = "Show West";
                else   b.label = "Hide West";
                b.get_root.arrange();
            };

            auto onoff = new RadioGroup;
            auto r0 = add_widget(new RadioButton("On"));
            auto r1 = add_widget(new RadioButton("Off"));
            onoff.multiadd(r0,1, r1,0);
            onoff.value = 1;
        }

        with(southGroup) {
            auto l0 = add_widget(new Label( "First Name:"));
            auto t0 = add_widget(new TextField("Hello"));
            auto l1 = add_widget(new Label( "Last Name:"));
            auto t1 = add_widget(new TextField("Rumpelstiltskin"));
            auto b4 = add_widget(new Button("Click me!"));
            auto l2 = add_widget(new Label("Disabled label"));
            l2.disable;
            b4.clicked ~= (Widget w){ writefln("Ouch! Not so hard!"); };
        }

        with (northGroup) {
            auto b10 =   add_widget(new Button("File"));
            auto b11 =   add_widget(new Button("Edit"));
            auto b12 =   add_widget(new Button("View"));
            auto b13 =   add_widget(new Button("Favorites"));
            auto b14 =   add_widget(new Button("Tools"));
            auto b15 =   add_widget(new Button("Options"));
            auto c15 =   add_widget(new Checkbox("Check it out!"));
            auto b16 =   add_widget(new Button("Help"));

            c15.value_changed ~= (Widget w, bool onoff) {
                writefln(onoff?"checked!":"unchecked!"); 
            };
        }
    }
    return gui;
}

void main(char[][] argv)
{
    bool dosync = true;
    char[] theme = "dxut";
    
    //---------------- PARSE ARGS -----------------------
    for( int i=1; i<argv.length; i++ ) {
        auto a = argv[i];
        if (a == "-nosync") dosync = false;
        else if (a == "-help") { usage(argv[0]); return; }
        else if (a == "-theme") {
            theme = argv[++i];
        }
    }

    //---------------- SETUP -----------------------
    // Open OpenGL window
    if (!glfwOpenWindow( 640, 480, 0,0,0,0, 0,0, GLFW_WINDOW ))
        return;

    // Enable sticky keys
    glfwEnable( GLFW_STICKY_KEYS );
    glfwEnable( GLFW_KEY_REPEAT );

    // Disable vertical sync if requested (on cards that support it)
    if (!dosync) glfwSwapInterval( 0 );

    // Set the background color
    glClearColor( 66/255., 75/255., 121/255., 0 );


    //---------------- CREATE GUI ----------------------
    luigui.Overlay gui = create_gui(theme);

    //---------------- HELPERS -------------------------
    int frames = 0;
    real t0 = glfwGetTime();

    //----------------
    // Calculate and display FPS (frames per second), returns current time
    real show_fps() 
    {
        real t = glfwGetTime();
        if( (t-t0) > 1.0 || frames == 0 )
        {
            real fps = frames / (t-t0);
            char[] titlestr = format("Spinning Triangle (%0.1f FPS)", fps);
            if (dosync) { titlestr ~= " [vsync]"; }
            glfwSetWindowTitle( toStringz(titlestr) );
            t0 = t;
            frames = 0;
        }
        frames ++;
        return t;
    }

    //----------------
    // Setup up open gl matrices based on window size
    void setup_view_matrices() 
    {
        // Get window size (may be different than the requested size)
        int width, height;
        glfwGetWindowSize( &width, &height );
        height = height > 0 ? height : 1;

        //writefln("Size = (%d, %d)", width,height);

        // Set viewport
        glViewport( 0, height, width, -height );

        // Select and setup the projection matrix
        glMatrixMode( GL_PROJECTION );
        glLoadIdentity();
        gluPerspective( 65.0f, cast(GLfloat)width/height, 1.0f,100.0f );

        // Select and setup the modelview matrix
        glMatrixMode( GL_MODELVIEW );
        glLoadIdentity();
        gluLookAt( 0.0f, 1.0f,  0.0f,    // Eye-position
                   0.0f, 20.0f, 0.0f,    // View-point
                   0.0f, 0.0f,  1.0f );  // Up-vector
    }

    //---------------- MAIN LOOP -------------------------
    bool running=true;
    while (running)
    {
        int x,y;
        glfwGetMousePos( &x, &y );

        real t = show_fps();

        setup_view_matrices();

        glClear( GL_COLOR_BUFFER_BIT );

        glTranslatef(-4,14,4 );
        glBegin( GL_TRIANGLES );
          glColor3f( 1, 0, 0 );  glVertex3f( -5, 0, -4 );
          glColor3f( 0, 1, 0 );  glVertex3f(  5, 0, -4 );
          glColor3f( 0, 0, 1 );  glVertex3f(  0, 0,  6);
        glEnd();
        glTranslatef( 4,-14,-4 );

        // Draw a rotating colorful triangle
        glTranslatef( 0, 14, 0 );
        glRotatef( t*100, 0,1,0 );
        glRotatef( 0.3*x, 1,0,0);
        glRotatef( 0.3*y, 0,0,1);
        glBegin( GL_TRIANGLES );
          glColor3f( 1, 0, 0 );  glVertex3f( -5, 0, -4 );
          glColor3f( 0, 1, 0 );  glVertex3f(  5, 0, -4 );
          glColor3f( 0, 0, 1 );  glVertex3f(  0, 0,  6 );
        glEnd();

        gui.draw();

        glfwSwapBuffers();

        running = 
            !glfwGetKey( GLFW_KEY_ESC ) &&
            glfwGetWindowParam( GLFW_OPENED );
    }

}
