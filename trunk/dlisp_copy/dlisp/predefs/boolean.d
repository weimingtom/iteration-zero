/*
  boolean.d
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


module dlisp.predefs.boolean;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  // Boolean ops not, and, or
  Cell* evalNot(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".", cell.cdr);
    return newBool(!isTrue(args[0]));
  }
  
  Cell* evalAnd(DLisp dlisp, Cell* cell) {
    Cell* args = cell.cdr;
    if (args) {
      while (args.cdr) {
        if (!isTrue(dlisp.eval(args.car))) {
          return null;
        }
        args = args.cdr;
      }
      return dlisp.eval(args.car);
    } else {
      return newBool(true);
    }
  }
  
  Cell* evalOr(DLisp dlisp, Cell* cell) {
    Cell* args = cell.cdr;
    if (args) {
      while (args.cdr) {
        Cell* res = dlisp.eval(args.car);
        if (isTrue(res)) {
          return res;
        }
        args = args.cdr;
      }
      return dlisp.eval(args.car);
    } else {
      return null;
    }
  }
  
}

public Environment addToEnvironment(Environment environment) {
  
  environment.bindPredef("not", &evalNot, "(not <bool>); Returns the oposite of <bool>.");
  environment.bindPredef("and", &evalAnd, "(AND <form> ...); Evalutes form from left to right, stoping and returns nil at first form not evaluating to true, or the last form.");
  environment.bindPredef("or", &evalOr, "(OR <form> ...); Evalutes form from left to right, stoping and returns the result at first form not evaluating to false, or the last form.");
  
  environment.clonePredef("not", "null");
  
  return environment;
  
}