# coding: utf-8

in_fife = None
guichan = None

def _import_guichan():
	global in_fife

	err_fife = ""
	try:
		import fife
		in_fife = True
		return fife
	except ImportError, e:
		err_fife = str(e)
	
	try:
		import guichan
		in_fife = False
		return guichan
	except ImportError, e:
		import traceback
		traceback.print_exc()
		raise ImportError("Couldn't import neither fife nor guichan: fife:'%s' guichan:'%s'" % (err_fife,str(e)))



def _munge_engine_hook(engine):
	engine.translate_mouse_event = getattr(engine,'translate_mouse_event',lambda x : x )
	engine.translate_key_event   = getattr(engine,'translate_key_event',lambda x : x )

	if not in_fife:
		return engine
	if not isinstance(engine,fife.Engine):
		return engine

	guimanager = engine.getGuiManager()

	def _fife_load_image(filename):
		index = engine.imagePool.addResourceFromFile(filename)
		return guichan.GuiImage(index,engine.getImagePool())

	hook = object()
	hook.add_widget    = guimanager.add
	hook.remove_widget = guimanager.remove
	hook.default_font  = engine.getDefaultFont()
	hook.load_image    = _fife_load_image
	hook.translate_mouse_event = guimanager.translateMouseEvent
	hook.translate_key_event   = guimanager.translateKeyEvent
	hook.engine        = engine
	return hook


guichan = _import_guichan()
if in_fife:
	fife = guichan
