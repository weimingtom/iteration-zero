/* $Id: gears.c,v 1.2 1999/10/21 16:39:06 brianp Exp $ */

/*
 * 3-D gear wheels.  This program is in the public domain.
 *
 * Command line options:
 *    -info      print GL implementation information
 *
 *
 * Brian Paul
 */

/* Conversion to GLUT by Mark J. Kilgard */

/*
 * $Log: gears.c,v $
 * Revision 1.2  1999/10/21 16:39:06  brianp
 * added -info command line option
 *
 * Revision 1.1.1.1  1999/08/19 00:55:40  jtg
 * Imported sources
 *
 * Revision 3.2  1999/06/03 17:07:36  brianp
 * an extra quad was being drawn in front and back faces
 *
 * Revision 3.1  1998/11/03 02:49:10  brianp
 * added fps output
 *
 * Revision 3.0  1998/02/14 18:42:29  brianp
 * initial rev
 *
 */


module sdl_gears;

import luigui = luigi.gui;
import luigi.adapters.sdl;
static import luigi.themes.std;
static import luigi.themes.dxut;

import derelict.sdl.sdl;
import derelict.opengl.gl;
import std.math;
import std.stdio;
import std.string;

static GLint T0 = 0;
static GLint Frames = 0;

static this()
{
    // load the SDL shared library
    DerelictSDL.load();
    DerelictGL.load();

    SDL_Init(SDL_INIT_VIDEO);
}

static ~this()
{
    // Close OpenGL window and terminate GLFW
    SDL_Quit();
}


luigui.Overlay create_gui(SDL_Surface *win, char[][] argv)
{
    char[] theme = "dx";

    //---------------- PARSE ARGS -----------------------
    for( int i=1; i<argv.length; i++ ) {
        auto a = argv[i];
        if (a == "-theme") {
            theme = argv[++i];
        }
    }
    
    luigui.Overlay gui;
    with(luigui) {
        Luigi().adapter = SDLAdapter();
        if (theme == "dxut" || theme == "dx") {
            Luigi().add_resource_location("..");
            Luigi().theme = new luigi.themes.dxut.DXUTTheme();
        }
        gui = new Overlay(cast(WindowHandle)win);
    
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


/**

  Draw a gear wheel.  You'll probably want to call this function when
  building a display list since we do a lot of trig here.
 
  Input:  inner_radius - radius of hole at center
          outer_radius - radius at center of teeth
          width - width of gear
          teeth - number of teeth
          tooth_depth - depth of tooth

 **/

static void
gear(GLfloat inner_radius, GLfloat outer_radius, GLfloat width,
  GLint teeth, GLfloat tooth_depth)
{
    GLint i;
    GLfloat r0, r1, r2;
    GLfloat angle, da;
    GLfloat u, v, len;

    r0 = inner_radius;
    r1 = outer_radius - tooth_depth / 2.0;
    r2 = outer_radius + tooth_depth / 2.0;

    da = 2.0 * PI / teeth / 4.0;

    glShadeModel(GL_FLAT);

    glNormal3f(0.0, 0.0, 1.0);

    /* draw front face */
    glBegin(GL_QUAD_STRIP);
    for (i = 0; i <= teeth; i++) {
        angle = i * 2.0 * PI / teeth;
        glVertex3f(r0 * cos(angle), r0 * sin(angle), width * 0.5);
        glVertex3f(r1 * cos(angle), r1 * sin(angle), width * 0.5);
        if (i < teeth) {
            glVertex3f(r0 * cos(angle), r0 * sin(angle), width * 0.5);
            glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
        }
    }
    glEnd();

    /* draw front sides of teeth */
    glBegin(GL_QUADS);
    da = 2.0 * PI / teeth / 4.0;
    for (i = 0; i < teeth; i++) {
        angle = i * 2.0 * PI / teeth;

        glVertex3f(r1 * cos(angle), r1 * sin(angle), width * 0.5);
        glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), width * 0.5);
        glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), width * 0.5);
        glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
    }
    glEnd();

    glNormal3f(0.0, 0.0, -1.0);

    /* draw back face */
    glBegin(GL_QUAD_STRIP);
    for (i = 0; i <= teeth; i++) {
        angle = i * 2.0 * PI / teeth;
        glVertex3f(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
        glVertex3f(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
        if (i < teeth) {
            glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
            glVertex3f(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
        }
    }
    glEnd();

    /* draw back sides of teeth */
    glBegin(GL_QUADS);
    da = 2.0 * PI / teeth / 4.0;
    for (i = 0; i < teeth; i++) {
        angle = i * 2.0 * PI / teeth;

        glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
        glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), -width * 0.5);
        glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), -width * 0.5);
        glVertex3f(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
    }
    glEnd();

    /* draw outward faces of teeth */
    glBegin(GL_QUAD_STRIP);
    for (i = 0; i < teeth; i++) {
        angle = i * 2.0 * PI / teeth;

        glVertex3f(r1 * cos(angle), r1 * sin(angle), width * 0.5);
        glVertex3f(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
        u = r2 * cos(angle + da) - r1 * cos(angle);
        v = r2 * sin(angle + da) - r1 * sin(angle);
        len = sqrt(u * u + v * v);
        u /= len;
        v /= len;
        glNormal3f(v, -u, 0.0);
        glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), width * 0.5);
        glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), -width * 0.5);
        glNormal3f(cos(angle), sin(angle), 0.0);
        glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), width * 0.5);
        glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), -width * 0.5);
        u = r1 * cos(angle + 3 * da) - r2 * cos(angle + 2 * da);
        v = r1 * sin(angle + 3 * da) - r2 * sin(angle + 2 * da);
        glNormal3f(v, -u, 0.0);
        glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
        glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
        glNormal3f(cos(angle), sin(angle), 0.0);
    }

    glVertex3f(r1 * cos(0), r1 * sin(0), width * 0.5);
    glVertex3f(r1 * cos(0), r1 * sin(0), -width * 0.5);

    glEnd();

    glShadeModel(GL_SMOOTH);

    /* draw inside radius cylinder */
    glBegin(GL_QUAD_STRIP);
    for (i = 0; i <= teeth; i++) {
        angle = i * 2.0 * PI / teeth;
        glNormal3f(-cos(angle), -sin(angle), 0.0);
        glVertex3f(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
        glVertex3f(r0 * cos(angle), r0 * sin(angle), width * 0.5);
    }
    glEnd();

}

static GLfloat view_rotx = 20.0, view_roty = 30.0, view_rotz = 0.0;
static GLint gear1, gear2, gear3;
static GLfloat angle = 0.0;
static luigui.Overlay gui;

static void
draw()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glPushMatrix();
  glRotatef(view_rotx, 1.0, 0.0, 0.0);
  glRotatef(view_roty, 0.0, 1.0, 0.0);
  glRotatef(view_rotz, 0.0, 0.0, 1.0);

  glPushMatrix();
  glTranslatef(-3.0, -2.0, 0.0);
  glRotatef(angle, 0.0, 0.0, 1.0);
  glCallList(gear1);
  glPopMatrix();

  glPushMatrix();
  glTranslatef(3.1, -2.0, 0.0);
  glRotatef(-2.0 * angle - 9.0, 0.0, 0.0, 1.0);
  glCallList(gear2);
  glPopMatrix();

  glPushMatrix();
  glTranslatef(-3.1, 4.2, 0.0);
  glRotatef(-2.0 * angle - 25.0, 0.0, 0.0, 1.0);
  glCallList(gear3);
  glPopMatrix();

  glPopMatrix();

  gui.draw();

  SDL_GL_SwapBuffers();

  Frames++;
  {
     GLint t = SDL_GetTicks();
     if (t - T0 >= 5000) {
        GLfloat seconds = (t - T0) / 1000.0;
        GLfloat fps = Frames / seconds;
        printf("%d frames in %g seconds = %g FPS\n", Frames, seconds, fps);
        T0 = t;
        Frames = 0;
     }
  }
}


static void
idle()
{
  angle += 2.0;
}

/* new window size or exposure */
static void
reshape(int width, int height)
{
  GLfloat h = cast(GLfloat) height / cast(GLfloat) width;

  glViewport(0, 0, cast(GLint) width, cast(GLint) height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum(-1.0, 1.0, -h, h, 5.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(0.0, 0.0, -60.0);
}

static void
init(char[][] args)
{
  static GLfloat[4] pos =   [5.0, 5.0, 10.0, 0.0];
  static GLfloat[4] red =   [0.8, 0.1, 0.0, 1.0];
  static GLfloat[4] green = [0.0, 0.8, 0.2, 1.0];
  static GLfloat[4] blue =  [0.2, 0.2, 1.0, 1.0];

  glLightfv(GL_LIGHT0, GL_POSITION, pos.ptr);
  glEnable(GL_CULL_FACE);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);

  /* make the gears */
  gear1 = glGenLists(1);
  glNewList(gear1, GL_COMPILE);
  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, red.ptr);
  gear(1.0, 4.0, 1.0, 20, 0.7);
  glEndList();

  gear2 = glGenLists(1);
  glNewList(gear2, GL_COMPILE);
  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, green.ptr);
  gear(0.5, 2.0, 2.0, 10, 0.7);
  glEndList();

  gear3 = glGenLists(1);
  glNewList(gear3, GL_COMPILE);
  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, blue.ptr);
  gear(1.3, 2.0, 0.5, 10, 0.7);
  glEndList();

  glEnable(GL_NORMALIZE);

  glClearColor(0.13, 0.27, 0.22, 0);

  if (args.length > 1 && args[1]=="-info") {
      writefln("GL_RENDERER   = ", toString(glGetString(GL_RENDERER)));
      writefln("GL_VERSION    = ", toString(glGetString(GL_VERSION)));
      writefln("GL_VENDOR     = ", toString(glGetString(GL_VENDOR)));
      writefln("GL_EXTENSIONS = ", toString(glGetString(GL_EXTENSIONS)));
  }
}

int main(char[][] args)
{
    SDL_Surface *screen = SDL_SetVideoMode(640, 480, 32, SDL_OPENGL|SDL_RESIZABLE);

    if ( ! screen ) {
        fprintf(stderr, "Couldn't set 300x300 GL video mode: %s\n", SDL_GetError());
        SDL_Quit();
        return 2;
    }
    SDL_WM_SetCaption("Gears", "gears");
    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
    SDL_EnableUNICODE(1);

    init(args);
    gui = create_gui(screen, args);
    SDLAdapter adapter = cast(SDLAdapter)gui.adapter;

    reshape(screen.w, screen.h);
    int done = 0;
    while ( ! done ) {
        SDL_Event event;

        idle();
        while ( SDL_PollEvent(&event) ) {
            if (adapter.dispatchEvent(event)) {
                continue;
            }

            switch(event.type) {
            case SDL_VIDEORESIZE:
                screen.w = event.resize.w;
                screen.h = event.resize.h;
                reshape(screen.w,screen.h);
                break;

            case SDL_QUIT:
                done = 1;
                break;

            case SDL_KEYDOWN:
                with (event.key) {
                    if (keysym.sym == SDLK_ESCAPE ||
                        (keysym.sym == SDLK_q && (keysym.mod & KMOD_CTRL)))
                    {
                        done = 1;
                    }
                }
                break;

            case SDL_MOUSEBUTTONDOWN:
                // Click on the non-gui area -- steal focus from gui.
                gui.set_focus(null);
                break;

            default:
                // do nothing
                break;
            }
        }

        if (!gui.get_focus())
        {
            Uint8 *keys = SDL_GetKeyState(null);
            if ( keys[SDLK_UP] ) {
                view_rotx += 5.0;
            }
            if ( keys[SDLK_DOWN] ) {
                view_rotx -= 5.0;
            }
            if ( keys[SDLK_LEFT] ) {
                view_roty += 5.0;
            }
            if ( keys[SDLK_RIGHT] ) {
                view_roty -= 5.0;
            }
            if ( keys[SDLK_z] ) {
                if ( SDL_GetModState() & KMOD_SHIFT ) {
                    view_rotz -= 5.0;
                } else {
                    view_rotz += 5.0;
                }
            }
        }
        draw();
    }
    return 0;
}
