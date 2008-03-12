/*
  evalpredefs.d
  dLISP

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

module dlisp.evalpredefs;

private {
  import dlisp.dlisp;
}

public {
  import dlisp.predefs.core;
  import dlisp.predefs.variables;
  import dlisp.predefs.lists;
  import dlisp.predefs.control;
  import dlisp.predefs.loopcontrol;
  import dlisp.predefs.boolean;
  import dlisp.predefs.numeric;
  import dlisp.predefs.compare;
  import dlisp.predefs.stream;
  import dlisp.predefs.strings;
}

public Environment addAllToEnvironment(Environment environment) {
  dlisp.predefs.core.addToEnvironment(environment);
  dlisp.predefs.variables.addToEnvironment(environment);
  dlisp.predefs.lists.addToEnvironment(environment);
  dlisp.predefs.control.addToEnvironment(environment);
  dlisp.predefs.loopcontrol.addToEnvironment(environment);
  dlisp.predefs.boolean.addToEnvironment(environment);
  dlisp.predefs.numeric.addToEnvironment(environment);
  dlisp.predefs.compare.addToEnvironment(environment);
  dlisp.predefs.stream.addToEnvironment(environment);
  dlisp.predefs.strings.addToEnvironment(environment);
  
  return environment;
}
