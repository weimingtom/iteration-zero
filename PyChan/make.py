#! /usr/bin/env python
import os,sys, glob
from os.path import abspath

LIB_INSTALL_PATH    = abspath("install/lib/")

GUICHAN_BUILD_DIR   = abspath("src/guichan")

GUICHAN_INCLUDE_DIR = abspath("install/include")
GUICHAN_LIB_DIR     = abspath("install/lib/")

GUICHAN_INSTALL_DIR = abspath("modules")

MICRON_BUILD_DIR    = abspath("src/micron")
MICRON_LIB_DIR      = LIB_INSTALL_PATH

MICRON_INSTALL_DIR  = abspath("modules")


SDL_CXXFLAGS = "-I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT"
SDL_LDFLAGS = "-L/usr/lib -lSDL -lSDL_image -lSDL_ttf "


PYTHON_INCLUDE_DIR = "/usr/include/python2.5"

def exec_in(path):
	def dec_f(f):
		def new_f(*args,**kwargs):
			os.chdir(path)
			try:
				return f(*args,**kwargs)
			finally:
				os.chdir("../..")
		return new_f
	return dec_f

def system(cmd):
	print cmd
	os.system(cmd)

@exec_in(GUICHAN_BUILD_DIR)
def make_guichan_interfaces():
	ifaces =  sorted(glob.glob("interfaces/*.i"))
	ifaces += sorted(glob.glob("interfaces/widgets/*.i"))
	in_templ = open("guichan_template.i").read()

	includes = ''.join(['%%include %s\n' % iface for iface in ifaces])
	out_file = open("guichan.i","w")
	out_file.write( in_templ )
	out_file.write( includes )
	out_file.close()
	print includes
		
@exec_in(GUICHAN_BUILD_DIR)
def make_guichan_cxx():
	cmd = "swig -w511 -c++ -python -I%s -outdir swig_wrapper guichan.i" % GUICHAN_INCLUDE_DIR
	system(cmd)

@exec_in(GUICHAN_BUILD_DIR)
def make_guichan_wrap():
	cmd = "c++ guichan_wrap.cxx -I%(GUICHAN_INCLUDE_DIR)s -I. -I%(PYTHON_INCLUDE_DIR)s -o guichan_wrap.o -c -fPIC -ggdb -O0" % globals()
	system(cmd)
	sofiles = GUICHAN_LIB_DIR + "/libguichan.so"
	cmd = "c++ -shared guichan_wrap.o %%s -o swig_wrapper/_guichan.so -Wl,-rpath=%(GUICHAN_LIB_DIR)s " % globals()
	system(cmd % sofiles)

@exec_in(MICRON_BUILD_DIR)
def make_micron_lib():
	sources = [(src,src[:-3]+"o") for src in sorted(glob.glob("src/*.cpp"))]
	for src, objfile in sources:
		cmd = "c++ -fPIC -c -g -O0 %%s -o %%s -I%(GUICHAN_INCLUDE_DIR)s %(SDL_CXXFLAGS)s" % globals()
		cmd = cmd % (src, objfile)
		system(cmd)
	objfiles = " ".join([objfile for src,objfile in sources])
	sofiles = " ".join([GUICHAN_LIB_DIR + "/libguichan%s.so" % variant for variant in ['','_opengl']])
	cmd = "c++ -fPIC -shared  %%s %%s -o libmicron.so  -Wl,-rpath=%(GUICHAN_LIB_DIR)s" % globals()
	cmd = cmd % (objfiles,sofiles)
	system(cmd)

@exec_in(MICRON_BUILD_DIR)
def make_micron_cxx():
	cmd = "swig -w511 -c++ -python -I./src -Wall -outdir swig_wrapper micron.i"
	system(cmd)

@exec_in(MICRON_BUILD_DIR)
def make_micron_wrap():
	cmd = "c++ micron_wrap.cxx -I. -I./src -I%(GUICHAN_INCLUDE_DIR)s  -I%(PYTHON_INCLUDE_DIR)s -o micron_wrap.o -c -fPIC -ggdb -O0 %(SDL_CXXFLAGS)s" % globals()
	system(cmd)
	LDFLAGS=" -lGL %(SDL_LDFLAGS)s -lguichan -lguichan_sdl " % globals()
	cmd = "c++ -shared micron_wrap.o libmicron.so -o swig_wrapper/_micron.so -L%(GUICHAN_LIB_DIR)s -Wl,-rpath=%(MICRON_LIB_DIR)s" % globals()
	system(cmd + LDFLAGS)

def make_micron_install():
	system("cp %(MICRON_BUILD_DIR)s/swig_wrapper/micron.py %(MICRON_INSTALL_DIR)s/"  % globals())
	system("cp %(MICRON_BUILD_DIR)s/swig_wrapper/_micron.so %(MICRON_INSTALL_DIR)s/"  % globals())
	system("cp %(MICRON_BUILD_DIR)s/libmicron.so %(MICRON_LIB_DIR)s/"  % globals())

def make_guichan_install():
	system("cp %(GUICHAN_BUILD_DIR)s/swig_wrapper/guichan.py %(GUICHAN_INSTALL_DIR)s/" % globals())
	system("cp %(GUICHAN_BUILD_DIR)s/swig_wrapper/_guichan.so %(GUICHAN_INSTALL_DIR)s/" % globals())
	
def make_guichan():
	make_guichan_interfaces()
	make_guichan_cxx()
	make_guichan_wrap()
	make_guichan_install()

def make_micron():
	make_micron_lib()
	make_micron_cxx()
	make_micron_wrap()
	make_micron_install()

make_guichan()
make_micron()
system('cd modules;python -c "import micron"; cd ..')
system('cd modules;python -c "import guichan"; cd ..')
