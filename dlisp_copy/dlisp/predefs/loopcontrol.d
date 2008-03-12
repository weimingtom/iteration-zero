/*
  loopcontrol.d
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


module dlisp.predefs.loopcontrol;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {

  Cell* evalWhile(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'.'.+", cell.cdr);
    cell = null;
    while (isTrue(dlisp.eval(args[0]))) {
      for (uint i = 1; i < args.length; i++) {
        cell = dlisp.eval(args[i]);
      }
    }
    return cell;
  }
  
  Cell* evalUntil(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'.'.+", cell.cdr);
    cell = null;
    while (!isTrue(dlisp.eval(args[0]))) {
      for (uint i = 1; i < args.length; i++) {
        cell = dlisp.eval(args[i]);
      }
    }
    return cell;
  }

  Cell* evalDo(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'.+", cell.cdr);
    while(1) {
      for (uint i = 0; i < args.length; i++) {
        cell = dlisp.eval(args[i]);
      }
    }
    return null;
  }

  Cell* evalDoTimes(DLisp dlisp, Cell* cell) {
    Cell*[] forms = evalArgs(dlisp, "'l'.+", cell.cdr);
    Cell*[] args = evalArgs(dlisp, "'yi'.?", forms[0]);
    dlisp.environment.pushScope();
    dlisp.environment.addLocal(args[0].name, null);
    cell = null;
    try {
      uint index = 0;
      while (index < args[1].intValue) {
        dlisp.environment()[args[0].name] = newInt(index);
        index++;
        for (uint i = 1; i < forms.length; i++) {
          cell = dlisp.eval(forms[i]);
        }
      }
      if (args.length == 3) {
        cell = dlisp.eval(args[2]);
      }
    }
    finally {
      dlisp.environment.popScope();
    }
    return cell;
  }

  Cell* evalDoRange(DLisp dlisp, Cell* cell) {
    Cell*[] forms = evalArgs(dlisp, "'l'.+", cell.cdr);
    Cell*[] args = evalArgs(dlisp, "'yii'.?", forms[0]);
    dlisp.environment.pushScope();
    dlisp.environment.addLocal(args[0].name, null);
    cell = null;
    try {
      int step = (args[1].intValue < args[2].intValue) ? 1 : -1;
      for (int index = args[1].intValue; index != (args[2].intValue + step); index += step) {
        dlisp.environment()[args[0].name] = newInt(index);
        for (uint i = 1; i < forms.length; i++) {
          cell = dlisp.eval(forms[i]);
        }
      }
      if (args.length == 4) {
        cell = dlisp.eval(args[3]);
      }
    }
    finally {
      dlisp.environment.popScope();
    }
    return cell;
  }

  Cell* evalDoList(DLisp dlisp, Cell* cell) {
    Cell*[] forms = evalArgs(dlisp, "'l'.+", cell.cdr);
    Cell*[] args = evalArgs(dlisp, "'yl'.?", forms[0]);
    dlisp.environment.pushScope();
    dlisp.environment.addLocal(args[0].name, null);
    cell = null;
    try {
      Cell* list = args[1];
      while (list) {
        dlisp.environment()[args[0].name] = list.car;
        list = list.cdr;
        for (uint i = 1; i < forms.length; i++) {
          cell = dlisp.eval(forms[i]);
        }
      }
      if (args.length == 3) {
        cell = dlisp.eval(args[2]);
      }
    }
    finally {
      dlisp.environment.popScope();
    }
    return cell;
  }

}

public Environment addToEnvironment(Environment environment) {

  environment.bindPredef("while", &evalWhile, "(WHEN <bool> <form> ...); Executes all <form>s in succession while <bool> holds true, returns result of last form.");
  environment.bindPredef("until", &evalUntil, "(UNTIL <bool> <form> ...); Executes all <form>s in succession until <bool> holds true, returns result of last form.");

  environment.bindPredef("do", &evalDo, "(DO <forms> ...); Infinitely execute <forms>s.");
  environment.bindPredef("dotimes", &evalDoTimes, "(DOTIMES (<sym> <count> [<result>]) <forms> ...); Execute <forms>s <count> times with <sym> set iteration count starting with 0. Returns last evaluated cell or optionally the valuated <result> form.");
  environment.bindPredef("dorange", &evalDoRange, "(DOTIMES (<sym> <start> <end> [<result>]) <forms> ...); Execute <forms>s with <sym> set to <start> and step 1 down or up for each iteration until <end>. Returns last evaluated cell or optionally the valuated <result> form.");
  environment.bindPredef("dolist", &evalDoList, "(DOLIST (<sym> <list> [<result>]) <forms> ...); Execute <forms>s <count> times with <sym> set each element of <list> in succession. Returns last evaluated cell or optionally the valuated <result> form.");

  return environment;
}