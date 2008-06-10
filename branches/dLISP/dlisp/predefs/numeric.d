/*
  numeric.d
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


module dlisp.predefs.numeric;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
  import std.conv;
  import std.math;
  import std.string;
}

// Aritmetic ops +, - , *, /
public {
  Cell* evalAdd(DLisp dlisp, Cell* cell) {
    return evalOpFunc(dlisp, cell, &addCells);
  }
  Cell* evalSub(DLisp dlisp, Cell* cell) {
    return evalOpFunc(dlisp, cell, &subCells);
  }
  Cell* evalMul(DLisp dlisp, Cell* cell) {
    return evalOpFunc(dlisp, cell, &mulCells);
  }
  Cell* evalDiv(DLisp dlisp, Cell* cell) {
    return evalOpFunc(dlisp, cell, &divCells);
  }
  Cell* evalMod(DLisp dlisp, Cell* cell) {
    return evalOpFunc(dlisp, cell, &modCells);
  }
}

private {

  typedef Cell* function(Cell*, Cell*) OpFunc;
  
  Cell* addCells(Cell* cella, Cell* cellb) {
    if (!isNumber(cella)) {
      throw new TypeState("Numeric operands expected", cella.pos);
    }
    if (!isNumber(cellb)) {
      throw new TypeState("Numeric operands expected", cellb.pos);
    }
    if (cella.cellType == CellType.ctINT) {
      if (cellb.cellType == CellType.ctINT)
        return newInt(cella.intValue + cellb.intValue);
      else
        return newFloat(cella.intValue + cellb.floatValue);      
    } else {
      if (cellb.cellType == CellType.ctINT)
        return newFloat(cella.floatValue + cellb.intValue);
      else
        return newFloat(cella.floatValue + cellb.floatValue);
    }
  }
  
  Cell* subCells(Cell* cella, Cell* cellb) {
    if (!isNumber(cella)) {
      throw new TypeState("Numeric operands expected", cella.pos);
    }
    if (!isNumber(cellb)) {
      throw new TypeState("Numeric operands expected", cellb.pos);
    }
    if (cella.cellType == CellType.ctINT) {
      if (cellb.cellType == CellType.ctINT)
        return newInt(cella.intValue - cellb.intValue);
      else
        return newFloat(cella.intValue - cellb.floatValue);      
    } else {
      if (cellb.cellType == CellType.ctINT)
        return newFloat(cella.floatValue - cellb.intValue);
      else
        return newFloat(cella.floatValue - cellb.floatValue);
    }
  }
  
  Cell* mulCells(Cell* cella, Cell* cellb) {
    if (!isNumber(cella)) {
      throw new TypeState("Numeric operands expected", cella.pos);
    }
    if (!isNumber(cellb)) {
      throw new TypeState("Numeric operands expected", cellb.pos);
    }
    if (cella.cellType == CellType.ctINT) {
      if (cellb.cellType == CellType.ctINT)
        return newInt(cella.intValue * cellb.intValue);
      else
        return newFloat(cella.intValue * cellb.floatValue);      
    } else {
      if (cellb.cellType == CellType.ctINT)
        return newFloat(cella.floatValue * cellb.intValue);
      else
        return newFloat(cella.floatValue * cellb.floatValue);
    }
  }
  
  Cell* divCells(Cell* cella, Cell* cellb) {
    if (!isNumber(cella)) {
      throw new TypeState("Numeric operands expected", cella.pos);
    }
    if (!isNumber(cellb)) {
      throw new TypeState("Numeric operands expected", cellb.pos);
    }
    if (cella.cellType == CellType.ctINT) {
      if (cellb.cellType == CellType.ctINT)
        return newInt(cella.intValue / cellb.intValue);
      else
        return newFloat(cella.intValue / cellb.floatValue);      
    } else {
      if (cellb.cellType == CellType.ctINT)
        return newFloat(cella.floatValue / cellb.intValue);
      else
        return newFloat(cella.floatValue / cellb.floatValue);
    }
  }

  Cell* modCells(Cell* cella, Cell* cellb) {
    if (!isNumber(cella)) {
      throw new TypeState("Numeric operands expected", cella.pos);
    }
    if (!isNumber(cellb)) {
      throw new TypeState("Numeric operands expected", cellb.pos);
    }
    if (cella.cellType == CellType.ctINT) {
      if (cellb.cellType == CellType.ctINT)
        return newInt(cella.intValue % cellb.intValue);
      else
        return newFloat(cella.intValue % cellb.floatValue);      
    } else {
      if (cellb.cellType == CellType.ctINT)
        return newFloat(cella.floatValue % cellb.intValue);
      else
        return newFloat(cella.floatValue % cellb.floatValue);
    }
  }
  
  Cell* evalOpFunc(DLisp dlisp, Cell* cell, OpFunc opFunc) {
    Cell* args = cell.cdr;
    uint cnt = listLen(args);
    if (cnt < 2) {
      if (opFunc == cast(OpFunc)&addCells) {
        cell = newInt(0);
      } else {
        if (cnt == 0) {
          throw new ArgumentState("-, * and / requeres at least 1 operand", cell.car.pos);
        }
        cell = newInt(opFunc == cast(OpFunc)&subCells ? 0 : 1);
      }
    } else {
      cell = dlisp.eval(args.car);
      args = args.cdr;
    }
    while (args) {
      cell = opFunc(cell, dlisp.eval(args.car));
      args = args.cdr;
    }
    return cell;
  }
  
  
  Cell* evalToType(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".", cell.cdr);
    char[] typename;
    switch (cell.car.name) {
      case "TOINT":
        typename = "integer";
        break;
      case "TOFLOAT":
        typename = "float";
        break;
    }
    switch (args[0].cellType) {
      case CellType.ctINT:
        if (typename == "integer") {
          return args[0];
        } else {
          return newFloat(cast(real)args[0].intValue);
        }
        break;
      case CellType.ctFLOAT:
        if (typename == "integer") {
          return newInt(cast(int)args[0].floatValue);
        } else {
          return args[0];
        }
        break;
      case CellType.ctSTR:
          try {
            if (find(args[0].strValue, ".") == -1) {
              return newInt(toInt(args[0].strValue));
            } else {
              return newFloat(toReal(args[0].strValue));
            }
          } catch (ConvError e) {
            throw new TypeState("Can not convert '" ~ args[0].strValue ~ "' to " ~ typename, args[0].pos);
          }
      default:
        throw new TypeState("Invalid type for converting to " ~ typename ~ " " ~ cellToString(args[0]), args[0].pos);
    }
  }

}

public Environment addToEnvironment(Environment environment) {

  environment["PI"] = newFloat(PI);

  environment.bindPredef("+", &evalAdd, "(+ <num> ...); Returns sum of <num>.");
  environment.bindPredef("-", &evalSub, "(- <num> ...); Returns \"sum\" of <num>.");
  environment.bindPredef("*", &evalMul, "(* <num> ...); Returns the product of <num>.");
  environment.bindPredef("/", &evalDiv, "(/ <num> ...); Returns the \"product\" of <num>");
  environment.bindPredef("mod", &evalMod, "(MOD <num> ...); Returns the modulus of <num>");

  environment.bindPredef("toint", &evalToType, "(TOINT <obj>); Convert <obj> to int or cast state conv-error.");
  environment.bindPredef("tofloat", &evalToType, "(TOFLOAT <obj>); Convert <obj> to int or cast state conv-error.");
  
  return environment;
  
}