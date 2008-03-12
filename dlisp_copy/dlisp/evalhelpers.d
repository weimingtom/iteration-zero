/*
 evalhelpers.d
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

module dlisp.evalhelpers;

private {
  import dlisp.dlisp;
}

public {
    
  Cell*[] evalArgs(DLisp dlisp, char[] fmt, Cell* args) {
    uint[] cnts;
    return evalArgs(dlisp, fmt, args, cnts);
  }
  
  Cell*[] evalArgs(DLisp dlisp, char[] fmt, Cell* args, out uint[] cnts) {
    // "yifscan" "'y+" 1 eller fler symboler, ingen eval."
    uint i, a, mincnt, maxcnt;
    Cell*[] ret, tret;
    void incI() {
      i++;
      if (i >= fmt.length) {
        throw new Error("Unexpected end of argument format string");
      }
    }
    void getCnt() {
      if ((i + 1) < fmt.length) {
        switch(fmt[i + 1]) {
          case '?':
            mincnt = 0; maxcnt = 1;
            incI();
            return;
          case '*':
            mincnt = 0; maxcnt = uint.max;
            incI();
            return;
          case '+':
            mincnt = 1; maxcnt = uint.max;
            incI();
            return;
          default:
            break;
        }
      }
      mincnt = maxcnt = 1;
    }
    bool tryArgs(char[] fmt) {
      Cell* nargs = args;
      while (fmt != "") {
        if (!isCons(nargs)) {
          return false;
        }
        bool quoted = fmt[0] == '\'';
        Cell* cell;
        if (quoted) {
          fmt = fmt[1..$];
          if (fmt == "") {
            throw new Error("Type expected after argument quote");
          }
          cell = nargs.car;
        } else {
          cell = dlisp.eval(nargs.car);
        }
        nargs = nargs.cdr;
        switch (fmt[0]) {
          case '.':
            break;
          case 'b':
            if (!isBool(cell)) return false;
            break;
          case 'i':
            if (!isInt(cell)) return false;
            break;
          case 'f':
            if (!isFloat(cell)) return false;
            break;
          case 'n':
            if (!isNumber(cell)) return false;
            break;
          case 's':
            if (!isString(cell)) return false;
            break;
          case 't':
            if (!isStream(cell)) return false;
            break;
          case 'y':
            if (!isSym(cell)) return false;
            break;
          case 'a':
            if (!isAtom(cell)) return false;
            break;
          case 'c':
            if (!isCons(cell)) return false;
            break;
          case 'l':
            if (!isList(cell)) return false;
            break;
          case 'o':
            if (!isFunc(cell)) return false;
            break;
          default:
            throw new Error("Unexpected char: " ~ fmt[0]);
            assert(0);
        }
        tret ~= cell;
        fmt = fmt[1..$];
      }
      args = nargs;
      return true;
    }
    for (i = 0; i < fmt.length; i++) {
      char[] tfmt;
      tfmt = "";
      tret.length = 0;
      if (fmt[i] == '(') {
        incI();
        while (fmt[i] != ')') {
          tfmt ~= fmt[i];
          incI();
        }
      } else {
        tfmt ~= fmt[i];
        if (tfmt == "'") {
          incI();
          tfmt ~= fmt[i];        
        }
      }
      getCnt();
      for (a = 0; a < maxcnt; a++) {
        if (!tryArgs(tfmt)) {
          break;
        } else {
          ret ~= tret;
          tret.length = 0;
        }
      }
      if (a < mincnt) {
        throw new TypeState("Could not evaluate argument as: " ~ tfmt, args.car.pos);
      } else {
        if (mincnt != maxcnt)
          cnts ~= a;
      }
    }
    return ret;
}

}