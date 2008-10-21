"""
Simplistic tile generator from textures.
"""

import Image,os,sys,os.path
from os.path import join as J
import ConfigParser

try:
	import psyco; psyco.full()
except:pass

size = 46*2,24*2
quality = 2
prep_size =  size[0]*quality, size[0]*quality

def prep_image(fname):
	image = Image.open(fname)
	if image.size != prep_size:
		image = image.resize(prep_size)
	return image

_tmasks = {}
def make_tilemasks(s):
	# create a tilemask and its inverse. cached.
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

	# chop added extra pixels by expanding
	d = quality*4
	dx,dy = image.size
	image = image.crop((d,d,dx-d,dy-d)).copy()

	# Scale Y-axis
	# convert -resample is much better than PIL.
	# ideally we'd use GIMP ...
	image.save("in.png")
	os.system("convert in.png -resample %dx%d  out.png" % (size[0]*quality,size[1]*quality))
	image = Image.open("out.png")

	# Apply tilemask
	w,h = image.size
	tmask,imask = make_tilemasks((w,h))
	tile = Image.new("RGBA",(w,h),(0,0,0,0))
	tile.paste(image,tmask)

	# Again convert -resize is better ...
	tile.save("in.png")
	os.system("convert in.png -resize %dx%d out.png" % (size))
	image = Image.open("out.png")
	image.save(out)

def make_tiles(tdef):
	# parse the tiles.def file.
	# create tiles from square images -- like nexuiz textures ^^
	in_dir  = "content/base/"
	out_dir = "content/tiles/"
	cfg = ConfigParser.ConfigParser({})
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


