#!/usr/bin/env python
# coding: utf-8

import startup; startup.findGuichan()
import guichan, micron

import pychan

class Application(object):
	def __init__(self, xmlfile):
		self.engine = micron.Micron()

		self.engine.init(640,640,1)

		self.gui = guichan.Gui()
		self.gui.setGraphics( self.engine.getGraphics() )
		self.gui.setInput( self.engine.getInput() )

		self.font = micron.TTFont("vera.ttf",12)

		guichan.Widget.setGlobalFont(self.font)

		self.top = guichan.Container()
		self.gui.setTop(self.top)
		self.top.setSize(640,640)

		pychan.init( self.create_hook(), debug = True )
		self.widget = pychan.loadXML(xmlfile)
		self.widget.mapEvents({'okButton':self.quit})
		self.widget.show()

	def mainLoop(self):
		self.running = 1
		while not self.engine.isQuitRequested() and self.running:
			self.engine.pumpEvents()
			self.gui.logic()
			self.engine.startFrame()
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
		hook.screen_width  = 640
		hook.screen_height = 640

		hook.get_font      = lambda name: self.font
		hook.load_image    = None #_fife_load_image
		return hook


if __name__ == '__main__':
	import sys

	app = Application(sys.argv[1])
	app.mainLoop()
