import startup; startup.findGuichan()

import guichan
import micron
import pychan

class Engine(object):
	def __init__(self):
		self.engine = micron.Micron()
		self.engine.init(640,480,1)
		self.gui = guichan.Gui()
		self.gui.setGraphics( engine.getGraphics() )
		self.gui.setInput( engine.getInput() )
		self.font = micron.TTFont("vera.ttf",12)
		guichan.Widget.setGlobalFont(self.font)
		self.top_widget = guichan.Container()
		self.gui.setTop(self.top_widget)
		self.top_widget.setSize(640,480)

	def pump(self):
		self.engine.pumpEvents()
		self.gui.logic()

	def draw(self):
		self.engine.startFrame()
		self.gui.draw()
		self.engine.endFrame()

	def mainLoop(self):
		while not self.engine.isQuitRequested():
			self.pump()
			self.draw()

	def shutdown(self):
		self.engine.shutdown()

