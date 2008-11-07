%module(directors="1") guichan

%include "std_string.i"
%include "std_vector.i"
%include "std_pair.i"
%include "std_list.i"
%include "std_map.i"
%include "std_set.i"
%include "typemaps.i"
%include "exception.i"

%include "interfaces/exception_handler.i"

%{
#include <guichan.hpp>
#include <guichan/platform.hpp>
%}

%feature("director") ActionListener;
%feature("director") MouseListener;
%feature("director") WidgetListener;
%feature("director") KeyListener;
%feature("director") FocusListener;


