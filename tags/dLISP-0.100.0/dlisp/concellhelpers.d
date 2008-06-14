/*
 concellhelper.d
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

module dlisp.concellhelpers;

private {
  import std.conv;
  import std.math;
  import std.string;
  
  import dlisp.types;
  import dlisp.states;
  import dlisp.concell;
}

public {
  
  char[] cellToString(Cell* cell, bool list = false, bool quotestr = true) {
    if (!cell) {
      return "NIL";
    }
    switch (cell.cellType) {
      case CellType.ctSYM:
        return cell.name;
        break;
      case CellType.ctINT:
        return toString(cell.intValue);
        break;
      case CellType.ctFLOAT:
        return toString(cell.floatValue);
        break;
      case CellType.ctSTR:
        if (quotestr) {
          return "\"" ~ cell.strValue ~ "\"";//"
        } else {
          return cell.strValue;
        }
        break;
      case CellType.ctBINT:
        return "[" ~ toString(*cell.pintValue) ~ "]";
        break;
      case CellType.ctBFLOAT:
        return "[" ~ toString(*cell.pfloatValue) ~ "]";
        break;
      case CellType.ctBSTR:
        if (quotestr) {
          return "[\"" ~ *cell.pstrValue ~ "\"]";//"
        } else {
          return "[" ~ *cell.pstrValue ~ "]";
        }
        break;
      case CellType.ctSTREAM:
        return "<Stream>";
        break;
      case CellType.ctCONS:
        char[] tmp;
        if (isSym(cell.car) && cell.car.name == "BACK-QUOTE") {
          tmp = "`" ~ cellToString(cell.cdr.car, list, quotestr);
        } else if (isSym(cell.car) && cell.car.name == "COMMA-QUOTE") {
          tmp = "," ~ cellToString(cell.cdr.car, list, quotestr);
        } else if (isSym(cell.car) && cell.car.name == "QUOTE") {
          tmp = "'" ~ cellToString(cell.cdr.car, list, quotestr);
        } else {
          tmp = list ? " " : "(";
          if (cell.visited) {
            return tmp ~ "...";
          } else {
            cell.visited = true;
            tmp ~= cellToString(cell.car);
            if (cell.cdr) {
              tmp ~= (isCons(cell.cdr) ? "" : " . ") ~ cellToString(cell.cdr, true);
            }
            cell.visited = false;
            if (!list)
              tmp ~= ")";
          }
        }
          return tmp;
        break;
      case CellType.ctFUNC:
        if (cell.name == "") {
          return "<Interpreted anonymous function>";
        } else {
          return "<Interpreted " ~ (cell.ismacro ? "macro" : "function") ~ ": " ~ cell.name ~ ">";
        }
        break;
      case CellType.ctPREDEF:
        return "<Predefined " ~ (cell.ismacro ? "macro" : "function") ~ ": " ~ cell.name ~ ">";
        break;
      case CellType.ctOBJECT:
        return "<Object: " ~ cell.name ~ ">";
        break;
    }
  }
  
  int cellToInt(Cell* cell) {
    if (!cell) {
      return 0;
    }
    switch (cell.cellType) {
      case CellType.ctINT:
        return cell.intValue;
        break;
      case CellType.ctFLOAT:
        return cast(int)cell.floatValue;
        break;
      case CellType.ctSTR:
        return toInt(cell.strValue);
        break;
      default:
        throw new ConvError("Incompatible cell-type for converting to integer.");
        break;
    }
  }
  
  real cellToFloat(Cell* cell) {
    if (!cell) {
      return 0;
    }
    switch (cell.cellType) {
      case CellType.ctINT:
        return cast(real)cell.intValue;
        break;
      case CellType.ctFLOAT:
        return cell.floatValue;
        break;
      case CellType.ctSTR:
        return toReal(cell.strValue);
        break;
      default:
        throw new ConvError("Incompatible cell-type for converting to float.");
        break;
    }
  }
  
  // Be more restrictive?
  bool isBool(Cell* cell) {
    return true;
  }
  
  bool isTrue(Cell* cell) {
    if (cell) {
      switch (cell.cellType) {
        case CellType.ctINT:
          return cell.intValue != 0;
        case CellType.ctFLOAT:
          return cell.floatValue != 0.0 && !isnan(cell.floatValue);
        case CellType.ctSTR:
          return cell.strValue.length > 0;
        case CellType.ctSTREAM:
          return cell.streamValue.isOpen;
        case CellType.ctSYM:
          return cell.name != "NIL";
        default:
          return true;
      }
    }
    return false;
  }
  
  bool isSym(Cell* cell) {
    if (!cell) {
      return false;
    } else {
      return cell.cellType == CellType.ctSYM;
    }
  }

  bool isNil(Cell* cell) {
    return !cell;
  }

  bool isInt(Cell* cell) {
    if (!cell) {
      return false;
    }
    return cell.cellType == CellType.ctINT;
  }
  
  bool isFloat(Cell* cell) {
    if (!cell) {
      return false;
    }
    return cell.cellType == CellType.ctFLOAT;
  }
  
  bool isNumber(Cell* cell) {
    if (!cell) {
      return false;
    }
    return cell.cellType == CellType.ctINT || cell.cellType == CellType.ctFLOAT;
  }
  
  bool isString(Cell* cell) {
    if (!cell) {
      return false;
    } else {
      return cell.cellType == CellType.ctSTR;
    }
  }
  
  bool isStream(Cell* cell) {
    if (!cell) {
      return false;
    } else {
      return cell.cellType == CellType.ctSTREAM;
    }
  }
  
  bool isFunc(Cell* cell) {
    if (!cell) {
      return false;
    }
    return cell.cellType == CellType.ctFUNC || cell.cellType == CellType.ctPREDEF;
  }
  
  bool isAtom(Cell* cell) {
    if (!cell) {
      return true;
    }
    switch (cell.cellType) {
      case CellType.ctINT, CellType.ctFLOAT, CellType.ctSTR, 
           CellType.ctSTREAM, CellType.ctFUNC, CellType.ctPREDEF, CellType.ctOBJECT:
        return true;
        break;
      case CellType.ctSYM:
        return cell.name == "T";
        break;    
      default:
        return false;
        break;
    }
  }
  
  bool isBoundValue(Cell* cell) {
    if (!cell) {
      return false;
    } else {
      switch(cell.cellType) {
        case CellType.ctBINT, CellType.ctBFLOAT, CellType.ctBSTR:
          return true;
        default:
          return false;
      }
    }
  }
  
  bool isCons(Cell* cell) {
    if (!cell) {
      return false;
    }
    return cell.cellType == CellType.ctCONS;
  }
  
  bool isList(Cell* cell) {
    if (!cell) {
      return true;
    }
    return cell.cellType == CellType.ctCONS;
  }

  bool isObject(Cell* cell) {
    if (!cell) {
      return false;
    }
    return cell.cellType == CellType.ctOBJECT;
  }
  
  int cmpCell(Cell* cella, Cell* cellb) {
    real a, b;
    if (!isNumber(cella)) {
      throw new ArgumentState("Only numbers can be compared", cella.pos);
    }
    if (!isNumber(cellb)) {
      throw new ArgumentState("Only numbers can be compared", cellb.pos);
    }
    if (cella.cellType == CellType.ctINT) {
      a = cast(real)cella.intValue;
    } else {
      a = cella.floatValue;
    }
    if (cellb.cellType == CellType.ctINT) {
      b = cast(real)cellb.intValue;
    } else {
      b = cellb.floatValue;
    }
    if (a < b)
      return -1;
    if (a > b)
      return 1;
    return 0;
  }
  
  bool eqCell(Cell* cella, Cell* cellb, bool fulleq = false) {
    // Same Cell or two nils
    if (cella == cellb) {
      return true;
    }
    // One nil
    if (!cella || !cellb) {
      return false;
    }
    // Be more forgiving of types?
    if (cella.cellType != cellb.cellType) {
      return false;
    }
    switch (cella.cellType) {
      case CellType.ctINT:
        return cella.intValue == cellb.intValue;
        break;
      case CellType.ctFLOAT:
        return cella.floatValue == cellb.floatValue;
        break;
      case CellType.ctSTR:
        return cella.strValue == cellb.strValue;
        break;
      case CellType.ctSYM, CellType.ctFUNC, CellType.ctPREDEF:
        return cella.name == cellb.name;
        break;
      case CellType.ctCONS:
        if (fulleq) {
          return eqList(cella, cellb);
        } else {
          return false;
        }
        break;
      case CellType.ctOBJECT:
        return cella.instance is cellb.instance;

      default:
        return false;
    }
  }
  
  bool eqList(Cell* cella, Cell* cellb) {
    if (cella == cellb) {
      return true;
    }
    if (!cella || !cellb) {
      return false;
    }
    if (cella.cellType != CellType.ctCONS || cellb.cellType != CellType.ctCONS) {
      return false;
    }
    uint size = listLen(cella);
    if (size != listLen(cellb)) {
      return false;
    }
    for (uint i = 0; i < size; i++) {
      if (!eqCell(cella.car, cellb.car, true)) {
        return false;
      }
      cella = cella.cdr; 
      cellb = cellb.cdr;
    }
    return true;
  }
  
  
  uint listLen(Cell* cell) {
    uint cnt = 1;
    if (cell && cell.cellType == CellType.ctSYM) {
      return 1;
    }
    if (!cell) {
      return 0;
    }
    while (isCons(cell.cdr)) {
      cnt++;
      cell = cell.cdr;
    }
    return cnt;
  }
  
  Cell* listItem(Cell* list, uint item) {
    while (item) {
      list = list.cdr;
      item--;
    }
    return list.car;
  }
  
  Cell* copyList(Cell* cell, uint count = uint.max) {
    if (isCons(cell)) {
      if (count == 0) {
        return null;
      } else {
        Cell* ret = new Cell;
        *ret = *cell;
        ret.cdr = copyList(cell.cdr, count - 1);
        return ret;
      }
    } else {
      return cell;
    }
  }
  
  Cell* appendToList(Cell* list, Cell* item) {
    if (list) {
      Cell* ret = list;
      while (list.cdr) {
        list = list.cdr;
      }
      list.cdr = item;
      return ret;
    } else {
      return item;
    }
  }
  /*
  Cell* newListFromArray(Cell*[] cells) {
    if (cells.length == 0) {
      return null;
    } else {
      Cell* first, cur;
      first = cur = newCons(cells[0], null);
      foreach(Cell* cell; cells[1..$]) {
        cur.cdr = newCons(cell, null);
        cur = cur.cdr;
      }
      return first;
    }
  }
  */
  
  Cell* newList(Cell*[] cells ...) {
    if (cells.length == 0) {
      return null;
    } else {
      Cell* first, cur;
      first = cur = newCons(cells[0], null);
      foreach(Cell* cell; cells[1..$]) {
        cur.cdr = newCons(cell, null);
        cur = cur.cdr;
      }
      return first;
    }
  }

  Cell*[] listToArray(Cell* list) {
    Cell*[] ret;
    while (list) {
      ret ~= list.car;
      list = list.cdr;
    }
    return ret;
  }
  
  Cell* newSymList(char[][] names) {
    if (names.length == 0) {
      return null;
    } else {
      return newCons(newSym(names[0]), newSymList(names[1..$]));
    }
  }
  
  Cell* newStrList(char[][] strs) {
    if (strs.length == 0) {
      return null;
    } else {
      return newCons(newStr(strs[0]), newStrList(strs[1..$]));
    }
  }
  
}
