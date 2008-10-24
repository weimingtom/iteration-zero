import sys
import os

def haveGuichan():
	try:
		import guichan.guichan as _dummy1_
		import guichan as _dummy2_
	except ImportError, e:
		print e
		return False
	return True

def getDefaultPaths():
	global guichan_path
	try:
		import config
		_paths = [config.guichan_path]
	except (ImportError, AttributeError):
		_paths = []
	_paths += [ '.', '..', '../..' ]
	return _paths

def findGuichan():
	if haveGuichan():
		return

	sys_path = sys.path[:]
	for path in getDefaultPaths():
		sys.path.append(path)
		print ">",path,
		if haveGuichan():
			return
		sys.path = sys_path[:]

	_paths = getDefaultPaths()
	guichan_path = None
	for p in _paths:
		if p in sys.path:
			continue

		# check if we are in a fife dir...
		test_paths = [os.path.abspath(p + '/' + a) for a in ('modules/guichan.py', 'modules/_guichan.so') ]
		#print p , map(os.path.exists,test_paths)
		if not all(map(os.path.exists,test_paths)):
			continue

		guichan_path = p
		print "Found GUICHAN in:", guichan_path + "/modules"
		sys.path.append(guichan_path + "/modules")

		os.environ['PYTHONPATH'] = os.path.pathsep.join(os.environ.get('PYTHONPATH', '').split(os.path.pathsep) + [
			os.path.abspath(guichan_path + "/modules")])

		#add windows paths (<fife>/.)
		##os.environ['PATH'] = os.path.pathsep.join(os.environ.get('PATH', '').split(os.path.pathsep) + [ os.path.abspath(guichan_path + '/' + a) for a in ('.') ])
		#os.path.defpath += os.path.pathsep + os.path.pathsep.join([ os.path.abspath(guichan_path + '/' + a) for a in ('.') ])
		break

	else:
		print 'Guichan was not found.'
		print "Please create a config.py file and add a line with: guichan_path = '<path to guichan>' eg. guichan_path = '../trunk'"
		exit()

	try:
		if not os.environ.get('LD_LIBRARY_PATH', '').startswith(os.path.abspath(guichan_path)):
			try:
				import guichan
			except ImportError, e:
				ld_path = [ os.path.abspath(guichan_path + '/install/lib')]
				ld_path += os.environ['LD_LIBRARY_PATH'].split(os.path.pathsep) if os.environ.has_key('LD_LIBRARY_PATH') else []
				
				os.environ['LD_LIBRARY_PATH'] = os.path.pathsep.join(ld_path)
				print "Restarting with proper LD_LIBRARY_PATH...",os.environ['LD_LIBRARY_PATH']
				args = [sys.executable] + sys.argv
				#we are already in openanno root, so just exec local executeable
				#args[1] = os.path.split(os.path.realpath(args[1]))[1]
				# support for python -O flag (disables __debug__)
				if not __debug__:
					args.insert(1, '-O')
				os.execvp(args[0], args)
		else:
			import guichan
	except ImportError, e:
		print 'Guichan was not found or failed to load.'
		print 'Reason: ' + e.message
		print "Please create a config.py file and add a line with: path = '<path to fife>' eg. path = '../../guichan/trunk/'"
		exit()

if __name__ ==  '__main__':
	findGuichan()