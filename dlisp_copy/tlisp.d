/*
  tlisp.d
  tLISP - Library version of dLISP, callable as standard dynamic library

  Author: Fredrik Olsson <peylow@treyst.se>
  Copyright (c) 2005 Treyst AB, <http://www.treyst.se>
  All rights reserved.

    This file is part of dLISP.
    dLISP is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    dLISP is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with dLISP; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

module tlisp;

private {
  import std.string;
  import dlisp.concell;
  import dlisp.parse;
  import dlisp.evaluate;
}

version(Windows) {
  extern(Windows):
} else {
  extern(C):
}

int tl_init() {
  // More magic needed
  return 1;
}

int tl_deinit() {
  // Here too
  return 1;
}

Cell* tl_parse(char* source) {
  // Need some C-ish way to report errors...
  return parse(toString(source));
}

Cell* tl_eval(Cell* cell) {
  return eval(cell);
}