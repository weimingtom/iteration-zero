# coding: utf-8
import pychan

class WidgetGroup(object):
	def __init__(self):
		self.widgets = []

	def __iter__(self):
		for widget in self.widgets:
			yield widget

	def __getitem__(self,i):
		return self.widgets[i]

	def show(self):
		for widget in self:
			if isinstance(widget,WidgetGroup):
				widget.show()
				continue
			if not widget.isVisible():
				widget.show()

	def hide(self):
		for widget in self:
			if isinstance(widget,WidgetGroup):
				widget.hide()
				continue
			if widget.isVisible():
				widget.hide()

	def append(self,widget):
		self.widgets.append(widget)

	def loadXMLFiles(self,xmlfiles):
		for xml in xmlfiles:
			self.loadXML(xml)

	def loadXML(self,xml):
		self.append(pychan.loadXML(xml))

	def mapEvents(self,event_map,ignoreMissing=True):
		for widget in self:
			widget.mapEvents(event_map,ignoreMissing=True)