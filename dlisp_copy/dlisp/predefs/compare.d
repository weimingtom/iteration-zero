/*
  compare.d
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


module dlisp.predefs.compare;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  // Comparison ops =, /=, <, <=, >, >=
  Cell* evalEqn(DLisp dlisp, Cell* cell) {
    return evalCmpFunc(dlisp, cell, &eqnCells);
  }
  Cell* evalNeqn(DLisp dlisp, Cell* cell) {
    return evalCmpFunc(dlisp, cell, &neqnCells);
  }
  Cell* evalLtn(DLisp dlisp, Cell* cell) {
    return evalCmpFunc(dlisp, cell, &ltnCells);
  }
  Cell* evalLten(DLisp dlisp, Cell* cell) {
    return evalCmpFunc(dlisp, cell, &ltenCells);
  }
  Cell* evalGtn(DLisp dlisp, Cell* cell) {
    return evalCmpFunc(dlisp, cell, &gtnCells);
  }
  Cell* evalGten(DLisp dlisp, Cell* cell) {
    return evalCmpFunc(dlisp, cell, &gtenCells);
  }
  
  Cell* evalEq(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "..", cell.cdr);
    return newBool(eqCell(args[0], args[1]));
  }
  
  Cell* evalEqual(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "..", cell.cdr);
    return newBool(eqCell(args[0], args[1], true));
  }
  
  Cell* evalEndp(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "l", cell.cdr);
    return newBool(!args[0]);
  }
  
  // Cell type ops
  Cell* evalAtom(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isAtom);
  }
  Cell* evalIssym(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isSym);
  }
  Cell* evalIsnum(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isNumber);
  }
  Cell* evalIsint(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isInt);
  }
  Cell* evalIsfloat(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isFloat);
  }
  Cell* evalIsstr(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isString);
  }
  Cell* evalIscons(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isCons);
  }
  Cell* evalIslist(DLisp dlisp, Cell* cell) {
    return isCellType(dlisp, cell, &isList);
  }
  
}

private {
  
  typedef bool function(Cell*, Cell*) CmpNumb;
  typedef bool function(Cell*) IsType;
  
  
  bool eqnCells(Cell* cella, Cell* cellb) {
    return cmpCell(cella, cellb) == 0;
  }
  bool neqnCells(Cell* cella, Cell* cellb) {
    return cmpCell(cella, cellb) != 0;
  }
  bool ltnCells(Cell* cella, Cell* cellb) {
    return cmpCell(cella, cellb) < 0;
  }
  bool ltenCells(Cell* cella, Cell* cellb) {
    return cmpCell(cella, cellb) <= 0;
  }
  bool gtnCells(Cell* cella, Cell* cellb) {
    return cmpCell(cella, cellb) > 0;
  }
  bool gtenCells(Cell* cella, Cell* cellb) {
    return cmpCell(cella, cellb) >= 0;
  }
  
  Cell* evalCmpFunc(DLisp dlisp, Cell* cell, CmpNumb cmpNumb) {
    Cell*[] args = evalArgs(dlisp, "nn+", cell.cdr);
    for (uint i = 0; i < args.length - 1; i++) {
      if (!cmpNumb(args[i], args[i + 1])) {
        return null;
      }
    }
    return newSym("T");
  }
  
  Cell* isCellType(DLisp dlisp, Cell* cell, IsType isType) {
    cell = dlisp.eval(cell.cdr.car);
    if (isType(cell))
      return newSym("T");
    else
      return null;
  }
  
}

public Environment addToEnvironment(Environment environment) {

  environment.bindPredef("=", &evalEqn, "(= <num> <num> ...); Returns true if all <num> are equal.");
  environment.bindPredef("/=", &evalNeqn, "(= <num> <num> ...); Returns true if all <num> are not equal.");
  environment.bindPredef("<", &evalLtn, "(= <num> <num> ...); Returns true if all <num> are monotonically increasing.");
  environment.bindPredef("<=", &evalLten, "(= <num> <num> ...); Returns true if all <num> are monotonically nondecreasing.");
  environment.bindPredef(">", &evalGtn, "(= <num> <num> ...); Returns true if all <num> are monotonically decreasing.");
  environment.bindPredef(">=", &evalGten, "(= <num> <num> ...); Returns true if all <num> are monotonically nonincreasing.");
  
  environment.bindPredef("eq", &evalEq, "(EQ <obja> <obja>); Returns true if the same objects.");
  environment.bindPredef("equal", &evalEqual, "(EQUAL <obja> <obja>); Returns true if structurally the same objects.");
  environment.bindPredef("isempty", &evalEndp, "(ISEMPTY <list>); Returns true for the empty list '().");
  environment.bindPredef("atom", &evalAtom, "(ATOM <arg>); Return true if <arg> is an atom.");
  
  environment.bindPredef("issym", &evalIssym, "(ISSYM <arg>); Return true if <arg> is a symbol.");
  environment.bindPredef("isnum", &evalIsnum, "(ISNUM <arg>); Return true if <arg> is numeric.");
  environment.bindPredef("isint", &evalIsnum, "(ISINT <arg>); Return true if <arg> is an integer.");
  environment.bindPredef("isfloat", &evalIsnum, "(ISFLOAT <arg>); Return true if <arg> is a float.");
  environment.bindPredef("isstr", &evalIsstr, "(ISSTR <arg>); Return true if <arg> is a string.");
  environment.bindPredef("iscons", &evalIscons, "(ISCONS <arg>); Return true if <arg> is a cons-cell.");
  environment.bindPredef("islist", &evalIslist, "(ISLIST <arg>); Return true if <arg> is a proper list.");
  
  environment.clonePredef("isempty", "endp");
  environment.clonePredef("issym", "symbolp");
  environment.clonePredef("isnum", "numericp");
  environment.clonePredef("isint", "integerp");
  environment.clonePredef("isfloat", "floatp");
  environment.clonePredef("isstr", "stringp");
  environment.clonePredef("iscons", "consp");
  environment.clonePredef("iscons", "listp");
  
  return environment;
  
}