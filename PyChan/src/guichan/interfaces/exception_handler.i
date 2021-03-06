/**
 * Some materials to understand exception handling:
 *
 * Basics about python exceptions:
 * 	http://docs.python.org/tut/node10.html
 * Python exception handling in C APIs
 * 	http://docs.python.org/api/exceptions.html
 * 	http://docs.python.org/api/exceptionHandling.html
 * SWIG exception handling
 * 	http://www.swig.org/Doc1.3/Customization.html#exception 
 * 	http://www.swig.org/Doc1.3/SWIGPlus.html#SWIGPlus_exception_specifications
 * 	http://www.swig.org/Doc1.3/Python.html#Python_nn36
 */

%feature("autodoc", "1");  // 0 == no param types, 1 == show param types

namespace std {
	%template(StringVector) vector<std::string>;
	%template(UintVector) vector<unsigned int>;
	%template(IntVector) vector<int>;
	%template(FloatVector) vector<float>;
	%template(DoubleVector) vector<double>;
	%template(BoolVector) vector<bool>;

	%template(UintUintPair) pair<unsigned int, unsigned int>;
	%template(UintUintPairVector) vector<std::pair<unsigned int, unsigned int> >;
};

%{
#include <guichan/exception.hpp> 
static void handleDirectorException() {
	PyObject* exception = NULL;
	PyObject* value = NULL;
	PyObject* traceback = NULL;
	PyErr_Fetch(&exception, &value, &traceback);
	PyErr_NormalizeException(&exception, &value, &traceback);
	if (exception) {
		PySys_SetObject("last_type", exception);
		PySys_SetObject("last_value", value);
		PySys_SetObject("last_traceback", traceback);	
		
		PyObject* d = PyModule_GetDict (PyImport_AddModule ("__main__"));
		PyDict_SetItemString(d, "exc_type", exception);
		PyDict_SetItemString(d, "exc_value", value);
		PyDict_SetItemString(d, "exc_traceback", traceback ? traceback : Py_None);
		
		char buf[1024];
		sprintf (buf, "\n\
import traceback\n\
s = \"\"\n\
for filename, line, function, text in traceback.extract_tb(exc_traceback):\n\
	s = s + ' File \"%%s\", line %%d, in %%s\\n    %%s' %% (filename, line, function, text)\n\
	if s[-1] != '\\n': s = s + '\\n'\n\
for l in traceback.format_exception_only(exc_type, exc_value):\n\
	s = s + ' ' + l\n\
	if s[-1] != '\\n': s = s + '\\n'\n\
print s\n\
");
		PyObject* e = PyRun_String(buf, Py_file_input, d, d);
		if (!e) {
			PyErr_Print();
		}
		Py_XDECREF(e);
		Py_XDECREF(d);
		Py_XDECREF(exception);
		Py_XDECREF(value);
		Py_XDECREF(traceback);
	}
}

#define _GCN_EXC_HANDLER(_fife_exc_type, _converted_type) \
	catch (const gcn::_fife_exc_type& _e) { \
		PyErr_Clear(); \
		SWIG_exception(_converted_type, _e.getMessage().c_str()); \
	}

#define _FIFE_DIRECTOR_EXC_HANDLER() \
	catch (Swig::DirectorException &_e) { \
		PyErr_Clear(); \
		SWIG_exception(SWIG_RuntimeError, "Catched director exception"); \
	}
%}

%feature("director:except") {
	if ($error != NULL) {
		handleDirectorException();
		throw Swig::DirectorMethodException();
	}
}

%exception {
	try {
		$action
	}
 	_GCN_EXC_HANDLER(Exception, SWIG_RuntimeError) 
 	_FIFE_DIRECTOR_EXC_HANDLER() 
}
