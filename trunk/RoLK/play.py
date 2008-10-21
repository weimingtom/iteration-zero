import game
#!/usr/bin/env python
# coding: utf-8


#import startup; startup.findGuichan()
from game import guichan, micron
from game import pychan, partycreation

STYLE = {
	'default' : {
		'border_size': 0,
		'margins': (0,0),
		'base_color' : guichan.Color(28,28,28),
		'foreground_color' : guichan.Color(255,255,255),
		'background_color' : guichan.Color(50,50,50),
		'focusable' : 0
	},
	'Button' : {
		'border_size': 2,
		'margins' : (5,2),
		'min_size' : (15,10),
		 
	},
	'CheckBox' : {
		'border_size': 0,
	},
	'RadioButton' : {
		'border_size': 0,
		'background_color' : guichan.Color(0,0,0),
	},
	'Label' : {
		'border_size': 0,
	},
	'ClickLabel' : {
		'border_size': 0,
	},
	'ListBox' : {
		'border_size': 0,
	},
	'Window' : {
		'border_size': 0,
		'margins': (5,5),
		'opaque' : 1,
		'titlebar_height' : 12,
		'vexpanding' : 1,
		#'background_image' : 'gui/backgrounds/background.png',
		#'font' : 'samanata_large'
	},
	'TextBox' : {
	},
	('Container','HBox','VBox') : {
		'border_size': 0,
		'margins': (0,0),
		'padding':2,
		#'background_image' : 'gui/backgrounds/background.png',
	}
}
class Application(object):
	def __init__(self, xmlfiles):
		self.engine = micron.Micron()
		self.screen_w,self.screen_h = 1000,800
		self.engine.init(self.screen_w,self.screen_h,1)
		self.loaded_images = {}

		self.gui = guichan.Gui()
		self.gui.setGraphics( self.engine.getGraphics() )
		self.gui.setInput( self.engine.getInput() )

		self.font = micron.TTFont("content/fonts/vera.ttf",12)

		guichan.Widget.setGlobalFont(self.font)

		self.top = guichan.Container()
		self.top.setOpaque(False)
		self.gui.setTop(self.top)
		self.top.setSize(self.screen_w,self.screen_h)

		pychan.init( self.create_hook(), debug = True )
		pychan.manager.addStyle("default",STYLE)

		self.mainmenu = pychan.loadXML("content/gui/mainmenu.xml")
		self.mainmenu.mapEvents({
			'new' :self.new_game,
			#'load' :self.load_game,
			#'options' :self.options,
			'quit':self.quit
		})
		self.show_mainmenu()
		self.party_creation = partycreation.PartyCreation(self)

	def new_game(self):
		self.mainmenu.hide()
		self.party_creation.start()


	def load_game(self):
		self.mainmenu.hide()

	def options(self):
		self.mainmenu.hide()

	def show_mainmenu(self):
		self.mainmenu.show()

	def main_loop(self):
		image = self.load_image("content/gui/desert_1024.jpg")
		self.running = 1
		while not self.engine.isQuitRequested() and self.running:
			self.engine.pumpEvents()
			self.gui.logic()
			self.engine.startFrame()
			g = self.engine.getGraphics()
			g._beginDraw()
			g.drawImage(image,0,self.screen_h-image.getHeight())
			g._endDraw()
			self.gui.draw()
			self.engine.endFrame()
		self.engine.shutdown()

	def quit(self):
		self.running = 0


	def create_hook(self):
		class hook:pass
		hook = hook()

		hook.add_widget    = self.top.add
		hook.remove_widget = self.top.remove
		hook.default_font  = self.font
		hook.screen_width  = self.screen_w
		hook.screen_height = self.screen_h

		hook.get_font      = lambda name: self.font
		hook.load_image    = guichan.Image.load
		return hook

	def load_image(self,filename):
		if filename in self.loaded_images:
			return self.loaded_images[filename]
		self.loaded_images[filename] = guichan.Image.load(filename)
		return self.loaded_images[filename]


if __name__ == '__main__':
	import sys
	app = Application(sys.argv[1:])
	app.main_loop()
