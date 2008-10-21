def xy_range(s):
	for x in range(s[0]):
		for y in range(s[1]):
			yield x,y

import Image,os,sys,os.path
import psyco; psyco.full()
from os.path import join as J

size = 46*2,24*2
mul = 2
prep_size =  size[0]*mul,size[0]*mul

def prep_image(fname):
	image = Image.open(fname)
	if image.size != prep_size:
		image = image.resize(prep_size)
	return image

import sys

_tmasks = {}
def make_tilemasks(s):
	if s in _tmasks: return _tmasks[s]
	tilemask = Image.new("1",s,0)
	itilemask = Image.new("1",s,1)
	T,I = tilemask.load(), itilemask.load()

	w,h = s
	for x in xrange(w):
		for y in xrange(h):
			dx,dy = x,y
			if x > w/2:
				dx = w - x
			if y > h/2:
				dy = h - y
			in_tile = dx/2 + dy >= h/2
			if in_tile:
				T[x,y] = 1
				I[x,y] = 0
	_tmasks[s] = tilemask,itilemask
	return tilemask,itilemask

#itilemask.show();input()

def make_tile(fname,out,rot=0):
	image = prep_image(fname)
	image = image.rotate(rot,expand=1)

	d = mul*4
	dx,dy = image.size
	image = image.crop((d,d,dx-d,dy-d)).copy()

	image.save("in.png")
	os.system("convert in.png -resample %dx%d  out.png" % (size[0]*mul,size[1]*mul))
	image = Image.open("out.png")

	w,h = image.size

	tmask,imask = make_tilemasks((w,h))
	tile = Image.new("RGBA",(w,h),(0,0,0,0))
	tile.paste(image,tmask)

	tile.save("in.png")
	os.system("convert in.png -resize %dx%d out.png" % (size))
	image = Image.open("out.png")
	image.save(out)

import ConfigParser
def make_tiles(tdef):
	in_dir  = "content/base/"
	out_dir = "content/tiles/"
	cfg = ConfigParser.ConfigParser({'quality':4})
	cfg.read(tdef)

	for index, name in enumerate(cfg.sections()):
		print name
		target = out_dir + "tile_%d.png" % index
		source = in_dir + cfg.get(name,'source')
		make_tile(source,target,rot=45)

if __name__=='__main__':
	make_tiles(sys.argv[1])
	
	#images = sorted(sys.argv[1:-1])
	#make_tiles_in_dir(images,sys.argv[-1])


