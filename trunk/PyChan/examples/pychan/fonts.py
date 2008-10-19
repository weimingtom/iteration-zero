# coding: utf-8

# Font handling
from exceptions import *
import guichan as fife
import ConfigParser

def loadFontDefs(filename):
	from internal import get_manager
	fmanager = get_manager().guimanager.getFontManager()

	fontdef = ConfigParser.ConfigParser()
	fontdef.read(filename)

	sections = [section for section in fontdef.sections() if section.startswith("Font/")]

	fonts = []
	for section in sections:
		name = section[5:]
		def _load(variant,w,v):
			if fontdef.has_option(section,variant):
				fspec = fife.FontSpec(name,w,v)
				fmanager.addFont(fspec, fontdef.get(section,variant))

		_load("normal",     fife.FontSpec.NORMAL,fife.FontSpec.NORMAL)
		_load("bold",       fife.FontSpec.BOLD,  fife.FontSpec.NORMAL)
		_load("italic",     fife.FontSpec.NORMAL,fife.FontSpec.ITALIC)
		_load("bold-italic",fife.FontSpec.BOLD,  fife.FontSpec.ITALIC)

def _minicss(text,d):
	tokens = text.split(";")
	for tok in tokens:
		if ':' in tok:
			k,v = tok.split(":")
			d[k] = v
	return d

def parseFontSpec(text):
	from internal import get_manager
	fmanager = get_manager().guimanager.getFontManager()
	d = dict(family="default",weight=fife.FontSpec.NORMAL,variant=fife.FontSpec.NORMAL,size=12)
	d = _minicss(text,d)
	d['size'] = int(d['size'])
	print text,d
	font = fmanager.createFont(fife.FontSpec(d['family'],d['weight'],d['variant']), d['size'])
	return font


class Font(object):
	def __init__(self):
		pass

	def load(self,name,get):
		from internal import get_manager
		fmanager = get_manager().guimanager.getFontManager()
		self.font = None
		self.name = name
		self.typename = get("type")
		self.source = get("source")
		self.row_spacing = int(get("row_spacing",0))
		self.glyph_spacing = int(get("glyph_spacing",0))

		if self.typename == "truetype":
			self.size = int(get("size"))
			self.antialias = int(get("antialias",1))
			self.color = map(int,get("color","255,255,255").split(','))
			fmanager.addFont(fife.FontSpec(name),self.source)
			self.font = fmanager.createFont(fife.FontSpec(name),self.size)

			if self.font is None:
				raise InitializationError("Could not load font %s" % name)

			self.font.setAntiAlias(self.antialias)
			self.font.setColor(*self.color)
		else:
			raise InitializationError("Unsupported font type %s" % self.typename)

		self.font.setRowSpacing( self.row_spacing )
		self.font.setGlyphSpacing( self.glyph_spacing )
		return self

	@staticmethod
	def loadFromFile(filename):
		"""
		Static method to load font definitions out of a PyChan config file.
		"""
		import ConfigParser

		fontdef = ConfigParser.ConfigParser()
		fontdef.read(filename)

		sections = [section for section in fontdef.sections() if section.startswith("Font/")]

		fonts = []
		for section in sections:
			name = section[5:]
			def _get(name,default=None):
				if fontdef.has_option(section,name):
					return fontdef.get(section,name)
				return default
			fonts.append( Font().load(name,_get) )
		return fonts

	def __str__(self):
		return "Font(source='%s')" % self.source

	def __repr__(self):
		return "<Font(source='%s') at %x>" % (self.source,id(self))

def loadFonts(filename):
	"""
	Load fonts from a config file. These are then available via their name.
	"""
	from internal import get_manager

	for font in Font.loadFromFile(filename):
		get_manager().addFont(font)
