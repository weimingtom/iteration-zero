/*
 concell.d
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

module dlisp.concell;

private {
  import std.string;
  import std.stream;
  
  import dlisp.types;  
  import dlisp.dlisp;
}

public {
  
  Cell* nil = null;
  
  typedef Cell* function(DLisp, Cell*) PredefFunc;
  
  enum CellType {
    ctSYM, ctINT, ctFLOAT, ctSTR, 
    ctSTREAM, ctCONS, ctFUNC, ctPREDEF,
    ctBINT, ctBFLOAT, ctBSTR
  }

  struct Cell {
    CellType cellType;
    char[] name;
    Pos pos;
    union {
      int intValue;         // ctINT
      int* pintValue;       // ctBINT
      real floatValue;      // ctFLOAT
      real* pfloatValue;    // ctBFLOAT
      char[] strValue;      // ctSTR
      char[]* pstrValue;    // ctBSTR
      Stream streamValue;   // ctSTREAM
      struct {              // ctCONS and ctFUNC, ctPREDEF
        bool ismacro;       // ctFUNC, ctPREDEF
        union {
          char[] docs;      // ctFUNC, ctPREDEF
          bool visited;
        }
        union {
          struct {
            Cell* car, cdr; // ctCONS, ctFUNC
          }
          PredefFunc func;  // ctPREDEF
        }
      }
    }
  }
  
  Cell* newSym(char[] name, bool nilsym = false) {
    atomcount++;
    name = toupper(name);
    if (name != "NIL" || nilsym) {
      Cell* cell = new Cell;
      cell.cellType = CellType.ctSYM;
      cell.name = name;
      return cell;
    } else {
      return null;
    }
  }
  
  Cell* newSym(char[] name, Pos pos) {
    Cell* cell = newSym(name);
    if (cell) {
      cell.pos = pos;
    }
    return cell;
  }
  
  Cell* newBool(bool value) {
    static Cell _true = { cellType: CellType.ctSYM, name: "T" };
    if (value) {
      return &_true; // newSym("T");
    } else {
      return null;
    }
  }
  
  Cell* newInt(int value) {
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctINT;
    cell.intValue = value;
    return cell;
  }

  Cell* newBoundInt(int* value) {
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctBINT;
    cell.pintValue = value;
    return cell;
  }
  
  public Cell* newInt(int value, Pos pos) {
    Cell* cell = newInt(value);
    cell.pos = pos;
    return cell;
  }
  
  Cell* newFloat(real value) {
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctFLOAT;
    cell.floatValue = value;
    return cell;
  }

  Cell* newBoundFloat(real* value) {
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctBFLOAT;
    cell.pfloatValue = value;
    return cell;
  }
  
  Cell* newFloat(real value, Pos pos) {
    Cell* cell = newFloat(value);
    cell.pos = pos;
    return cell;
  }
  
  Cell* newStr(char[] value) {
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctSTR;
    cell.strValue = value;
    return cell;
  }

  Cell* newBoundStr(char[]* value) {
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctBSTR;
    cell.pstrValue = value;
    return cell;
  }
  
  Cell* newStr(char[] value, Pos pos) {
    Cell* cell = newStr(value);
    cell.pos = pos;
    return cell;
  }
  
  Cell* newStream(Stream value) {
    assert(value !is null);
    atomcount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctSTREAM;
    cell.streamValue = value;
    return cell;
  }
  
  Cell* newCons(Cell* car, Cell* cdr) {
    conscount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctCONS;
    cell.car = car;
    cell.cdr = cdr;
    return cell;
  }
  
  Cell* newFunc(Cell* car, Cell* cdr, char[] docs = "", char[] name = "") {
    conscount++;
    Cell* cell = new Cell;
    cell.cellType = CellType.ctFUNC;
    cell.name = name;
    cell.docs = docs;
    cell.car = car;
    cell.cdr = cdr;
    return cell;
  }
  
  Cell* newPredef(char[] name, PredefFunc func, char[] docs, bool ismacro = false) {
    name = toupper(name);
    Cell* cell = new Cell;
    cell.cellType = CellType.ctPREDEF;
    cell.name = name;
    cell.func = func;
    cell.docs = docs;
    cell.ismacro = ismacro;
    return cell;
  }
  
}

protected {
  
  uint conscount = 0;
  uint atomcount = 0;
  
}

static this() {
  nil = newSym("NIL", true);
}

