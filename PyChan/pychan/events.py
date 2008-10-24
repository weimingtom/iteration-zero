#coding: utf-8

"""
PyChan event handling
=====================

Users shouldn't need to use this module directly.
L{widgets.Widget.capture} and L{widgets.Widget.mapEvents} provide
a convenient API to capture events.

Nevertheless to understand how its supposed to work
take a look at L{EventMapper} and L{EventListener}

Event callbacks
---------------

You can either write callbacks yourself or
use L{tools.callBackWithArguments} or L{tools.attrSetCallback}
to generate suitable callbacks.

Here's an example callback::
   def dumpEventInfo(event=0,widget=0):
      print widget, " received the event ", event

Note the signature - C{event} and C{widget} are keyword
arguments passed to the callback. If doesn't accept either
C{event} or C{widget} as argument, these are not passed.

This way a simple function which ignores C{event} or C{widget}
can be used, while they are available if needed.

Currently only one callback can be set per event. In case
you don't want to write your own callback that dispatches
to different callbacks you can use L{tools.chainCallbacks}.

Available Events
----------------

"""

from compat import guichan

import exceptions
from internal import get_manager
import tools
import traceback

EVENTS = [
	"mouseEntered",
	"mouseExited",
	"mousePressed",
	"mouseReleased",
	"mouseClicked",
	"mouseMoved",
	"mouseDragged",
	"action",
	"keyPressed",
	"keyReleased",
]

# Add the EVENTS to the docs.
__doc__ += "".join([" - %s\n" % event for event in EVENTS])

# The line before seems to leak the variable event into the global namespace ... remove that!
try: del event
except:pass

MOUSE_EVENT, KEY_EVENT, ACTION_EVENT = range(3)
def getEventType(name):
	if "mouse" in name:
		return MOUSE_EVENT
	if "key" in name:
		return KEY_EVENT
	return ACTION_EVENT

DEFAULT_CHANNEL = "default"
DEFAULT_EVENT = "action"

CALLBACK_NONE_MESSAGE = """\
You passed None as parameter to %s.capture, which would normally remove a mapped event.
But there was no event mapped. Did you accidently call a function instead of passing it?
"""
class EventListenerBase(object):
	"""
	Redirector for event callbacks.
	Use *only* from L{EventMapper}.

	This class uses the SWIG director feature - overriden
	virtual methods are called from C++ to - listen to
	Guichan events.

	"""
	def __init__(self):
		super(EventListenerBase,self).__init__()
		self.events = {}
		self.indent = 0
		self.debug = 1
		self.is_attached = False

	def attach(self,widget):
		"""
		Start receiving events.
		No need to call this manually.
		"""

		if self.is_attached:
			return
		if not self.events:
			return
		if self.debug: print "Attach:",self
		self.doAttach(widget.real_widget)
		self.is_attached = True

	def detach(self):
		"""
		Stop receiving events.
		No need to call this manually.
		"""
		if not self.is_attached:
			return
		if self.debug: print "Detach:",self
		self.doDetach(widget.real_widget)
		self.is_attached = False

	def _redirectEvent(self,name,event):
		self.indent += 4
		try:
			event = self.translateEvent(getEventType(name), event)
			if name in self.events:
				if self.debug: print "-"*self.indent, name
				for f in self.events[name].itervalues():
					f( event )

		except:
			print name, event
			traceback.print_exc()
			raise

		finally:
			self.indent -= 4

	def translateEvent(self,event_type,event):
		if event_type == MOUSE_EVENT:
			return get_manager().hook.translate_mouse_event(event)
		if event_type == KEY_EVENT:
			return get_manager().hook.translate_key_event(event)
		return event

class _ActionEventListener(EventListenerBase,guichan.ActionListener):
	def __init__(self):super(_ActionEventListener,self).__init__()
	def doAttach(self,real_widget):	real_widget.addActionListener(self)
	def doDetach(self,real_widget): real_widget.removeActionListener(self)

	def action(self,e): self._redirectEvent("action",e)

class _MouseEventListener(EventListenerBase,guichan.MouseListener):
	def __init__(self):super(_MouseEventListener,self).__init__()
	def doAttach(self,real_widget):	real_widget.addMouseListener(self)
	def doDetach(self,real_widget): real_widget.removeMouseListener(self)

	def mouseEntered(self,e): self._redirectEvent("mouseEntered",e)
	def mouseExited(self,e): self._redirectEvent("mouseExited",e)
	def mousePressed(self,e): self._redirectEvent("mousePressed",e)
	def mouseReleased(self,e): self._redirectEvent("mouseReleased",e)
	def mouseClicked(self,e): self._redirectEvent("mouseClicked",e)
	def mouseMoved(self,e): self._redirectEvent("mouseMoved",e)
	def mouseDragged(self,e): self._redirectEvent("mouseDragged",e)

class _KeyEventListener(EventListenerBase,guichan.KeyListener):
	def __init__(self):super(_KeyEventListener,self).__init__()
	def doAttach(self,real_widget):	real_widget.addKeyListener(self)
	def doDetach(self,real_widget): real_widget.removeKeyListener(self)

	def keyPressed(self,e): self._redirectEvent("keyPressed",e)
	def keyReleased(self,e): self._redirectEvent("keyReleased",e)

class EventMapper(object):
	"""
	Handles events and callbacks for L{widgets.Widget}
	and derived classes.

	Every PyChan widget has an L{EventMapper} instance
	as attribute *event_mapper*.

	This instance handles all necessary house-keeping.
	Such an event mapper can be either *attached* or
	*detached*. In its attached state an L{EventListener}
	is added to the Guichan widget and will redirect
	the events to the callbacks.

	In its detached state no events are received from the
	real Guichan widget.

	The event mapper starts in the detached state.
	When a new event is captured the mapper attaches itself
	automatically. The widget doesn't need to handle that.
	"""
	def __init__(self,widget):
		super(EventMapper,self).__init__()
		self.widget = widget
		self.listener = {
			KEY_EVENT    : _KeyEventListener(),
			ACTION_EVENT : _ActionEventListener(),
			MOUSE_EVENT  : _MouseEventListener(),
		}
		self.is_attached = False
		self.debug = get_manager().debug

	def __repr__(self):
		return "EventMapper(%s)" % repr(self.widget)


	def capture(self,event_name,callback,group_name):
		if event_name not in EVENTS:
			raise exceptions.RuntimeError("Unknown eventname: " + event_name)

		if callback is None:
			if self.isCaptured(event_name,group_name):
				self.removeEvent(event_name,group_name)
			elif self.debug:
					print CALLBACK_NONE_MESSAGE % str(self.widget)
			return
		self.addEvent(event_name,callback,group_name)

	def isCaptured(self,event_name,group_name="default"):
		return event_name in self.listener.events and group_name in self.listener.events[event_name]

	def getCapturedEvents(self):
		return self.listener.events.keys()

	def getListener(self,event_name):
		return self.listener[getEventType(event_name)]

	def removeEvent(self,event_name,group_name):
		listener = self.getListener(event_name)
		del listener.events[event_name][group_name]
		if not listener.events[event_name]:
			del listener.events[event_name]
		if not listener.events:
			self.listener.detach(self.widget)

	def addEvent(self,event_name,callback,group_name):
		if not callable(callback):
			raise RuntimeError("An event callback must be either a callable or None - not %s" % repr(callback))

		def captured_f(event):
			tools.applyOnlySuitable(callback,event=event,widget=self.widget)

		listener = self.getListener(event_name)

		if event_name not in listener.events:
			listener.events[event_name] = {group_name : captured_f}
		else:
			listener.events[event_name][group_name] = captured_f
		listener.attach(self.widget)

def splitEventDescriptor(name):
	"""
	Utility function to split "widgetName/eventName/channelName" descriptions into tuples.

	C{channelName} is optional and defaults to L{DEFAULT_CHANNEL}.

	That's the default channel used in normal application code.

	C{eventName} is optional and defaults to L{DEFAULT_EVENT}.

	That's the B{action} event distributed by buttons when they are activated.
	"""
	L = name.split("/")
	if len(L) not in (1,2,3):
		raise exceptions.RuntimeError("Invalid widgetname / eventname combination: " + name)
	if len(L) == 1: 
		L = L[0],DEFAULT_EVENT
	elif L[1] not in EVENTS:
		raise exceptions.RuntimeError("Unknown event name: " + name)
	if len(L) == 2:
		L = L[0],L[1],DEFAULT_CHANNEL
	return L
