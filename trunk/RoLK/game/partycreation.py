import pychan
import pychan.dialogs
import pychan.tools
curry = pychan.tools.callbackWithArguments
import widgetgroup

from races import RACES, CLASSES

class CharCreation(object):
	RACE, CLASS, NAME = range(3)
	def __init__(self):
		self.gui = widgetgroup.WidgetGroup()
		self.gui.loadXMLFiles([
			"content/gui/ng_char_select_race.xml",
			"content/gui/ng_char_select_class.xml",
		])
		self.gui.mapEvents({
			"next":self.next,
			"prev":self.prev,
		})

		self.race_buttons = widgetgroup.WidgetGroup()
		box = self.gui[self.RACE].findChild(name="race_box")
		for index,race in enumerate(RACES):
			button = pychan.Button(name="c%d" % index,text=race.name)
			button.min_size=(150,50)
			button.capture( curry(self.select_race, index) )
			self.race_buttons.append( button )
			box.addChild( button )
		box.adaptLayout()

		self.class_buttons = widgetgroup.WidgetGroup()
		box = self.gui[self.CLASS].findChild(name="class_box")
		for index,race in enumerate(CLASSES):
			button = pychan.Button(name="c%d" % index,text=race.name)
			button.min_size=(150,50)
			button.capture( curry(self.select_class, index) )
			self.class_buttons.append( button )
			box.addChild( button )
		box.adaptLayout()

		self.current = 0

		self.race_index = 0
		self.class_index = 0
		self.select_class(0)
		self.select_race(0)

	def get_choice_button(self,step,index):
		return self.gui[step].findChild(name="c%d" % index)

	def update_choice(self,step,old,new):
		button = self.get_choice_button(step,old)
		button.base_color = 28,28,28
		button = self.get_choice_button(step,new)
		button.base_color = 64,28,28

	def select_race(self,index):
		self.update_choice(self.RACE,self.race_index,index)
		name = self.get_choice_button(self.RACE,index).text
		self.gui[self.RACE].distributeInitialData({
			"current_label" : name,
			"description" : RACES[index].description,
			"allowed_classes" : ", ".join([c().name for c in RACES[index].allowed_classes]),
		})
		self.race_index = index
		if CLASSES[self.class_index].__class__ not in RACES[index].allowed_classes:
			self.select_class(0)
		for i,button in enumerate(self.class_buttons):
			active = CLASSES[i].__class__ in RACES[index].allowed_classes
			button.enabled = active
			if not active and i == self.class_index:
				self.class_index += 1
			if active:
				button.base_color = 28,28,28
			else:
				button.base_color = 0,0,0
		self.select_class(self.class_index)


	def select_class(self,index):
		self.update_choice(self.CLASS,self.class_index,index)
		print "sc",self.class_index,index
		name = self.get_choice_button(self.CLASS,index).text
		self.gui[self.CLASS].distributeInitialData({
			"current_label" : name,
			"description" : CLASSES[index].description,
		})
		self.class_index = index

	def next(self):
		self.gui[self.current].hide()
		self.current += 1
		self.gui[self.current].show()

	def prev(self):
		self.gui[self.current].hide()
		self.current -= 1
		self.gui[self.current].show()


	def start(self):
		self.gui[self.current].show()
		self.gui[self.current].adaptLayout()

	def stop(self):
		self.gui[self.current].hide()


class PartyCreation(object):
	def __init__(self,app):
		self.app = app
		self.control = widgetgroup.WidgetGroup()
		self.control.loadXMLFiles(["content/gui/ng_party.xml","content/gui/ng_control.xml"])
		self.control.mapEvents({
			'abort' : self.abort,
		})
		for i in range(8):
			widget = self.get_char_button(i)
			widget.capture(curry(self.switch_to_char,i))

	def get_char_button(self,index):
		return self.control[0].findChild(name = "char%d" % index)

	def start(self):
		self.control.show()
		self.current_char = 0
		self.chars = [CharCreation() for i in range(8)]
		self.select_char(0)

	def stop(self):
		self.control.hide()
		self.chars[ self.current_char ].stop()
		self.deselect_char(self.current_char)

	def abort(self):
		self.stop()
		self.app.show_mainmenu()

	def deselect_char(self,num):
		self.chars[ num ].stop()
		self.get_char_button(num).base_color = 28,28,28

	def select_char(self,num):
		self.chars[ num ].start()
		self.get_char_button(num).base_color = 64,28,28

	def switch_to_char(self,num):
		if num == self.current_char:
			return
		self.deselect_char(self.current_char)
		self.current_char = num
		self.select_char(self.current_char)
