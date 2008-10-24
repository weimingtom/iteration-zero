import startup; startup.findGuichan()

import guichan
import guichan.micron as micron

engine = micron.Micron()

engine.init(640,480,1)

gui = guichan.Gui()
gui.setGraphics( engine.getGraphics() )
gui.setInput( engine.getInput() )

font = micron.TTFont("content/fonts/vera.ttf",48)

guichan.Widget.setGlobalFont(font)

top = guichan.Container()
gui.setTop(top)
top.setSize(640,480)

running = 1
class Listener(guichan.ActionListener):
	def __init__(self):
		guichan.ActionListener.__init__(self)
	def action(self,event):
		global running
		running = 0

button = guichan.Button("Hello world!")
button.setSize(620,460)
button.setPosition(10,10)

listener = Listener()
button.addActionListener(listener)

top.add(button)


while not engine.isQuitRequested() and running:
	engine.pumpEvents()
	gui.logic()
	engine.startFrame()
	gui.draw()
	engine.endFrame()

engine.shutdown()

