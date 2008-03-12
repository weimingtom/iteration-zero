/*
  variables.d
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

module dlisp.predefs.variables;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  Cell* evalIsSet(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "y+", cell.cdr);
    for (uint i = 0; i < args.length; i++) {
      if (!dlisp.environment.isBound(args[i].name)) {
        return null;
      }
    }
    return newBool(true);
  }
  
  Cell* evalSet(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "('y'.)+", cell.cdr);
    for(int i = 0; i < args.length; i += 2) {
      char[] name = args[i].name;
      cell = dlisp.eval(args[i + 1]);
      //dlisp.environment[name] = cell;
      Environment e = dlisp.environment;
      e[name] = cell;
    }
    return cell;
  }
  
  Cell* evalPut(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "('..)+", cell.cdr);
    Cell* place;
    for (int i = 0; i < args.length; i += 2) {
      place = dlisp.eval(args[i], true);
      if (!place) {
        throw new EvalState("Can not put into NIL", cell.pos);
      }
      cell = args[i + 1];
      if (isBoundValue(place)) {
        switch (place.cellType) {
          case CellType.ctBINT:
            if (cell.cellType != CellType.ctINT) {
              throw new TypeState("Not an integer", cell.pos);
            } else {
              *place.pintValue = cell.intValue;
            }
          case CellType.ctBFLOAT:
            if (cell.cellType != CellType.ctFLOAT) {
              throw new TypeState("Not a float", cell.pos);
            } else {
              *place.pfloatValue = cell.floatValue;
            }
          case CellType.ctBSTR:
            if (cell.cellType != CellType.ctSTR) {
              throw new TypeState("Not a string", cell.pos);
            } else {
              *place.pstrValue = cell.strValue;
            }
        }
      } else {
        if (cell) {
          *place = *cell;
        } else {
          *place = *nil;
        }
      }
    }
    return cell;
  }
  
  Cell* evalUnset(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "y+", cell.cdr);
    cell = null;
    for (uint i = 0; i < args.length; i++) {
      if (dlisp.environment.isBound(args[i].name)) {
        cell = dlisp.environment[args[i].name];
        if (dlisp.environment.isBound(args[i].name)) {
          dlisp.environment.unbind(args[i].name);
        } else {
          throw new UnboundSymbolState("Unbound symbol: " ~ args[i].name, args[i].pos); 
        }
      }
    }
    return cell;
  }
  
}

public Environment addToEnvironment(Environment environment) {
  
  environment.bindPredef("isset", &evalIsSet, "(ISSET <sym> .. ); Ruturns true if all <sym>s are set to avalue.");
  environment.bindPredef("set", &evalSet, "(SET <sym> <cons>); Sets global symbol <sym> to <cons>.", true);
  environment.bindPredef("put", &evalPut, "(PUT <pos> <cons>); Place sym at pos.", true);
  environment.bindPredef("unset", &evalUnset, "(UNSET <sym> ...); Unset symbol value, returns last symbols value or NIL if none is set.");
  
  environment.clonePredef("isset", "boundp");
  environment.clonePredef("set", "setq");
  environment.clonePredef("put", "setf");
  
  return environment;
  
}