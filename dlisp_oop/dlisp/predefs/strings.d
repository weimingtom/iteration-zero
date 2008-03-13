/*
  strings.d
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

module dlisp.predefs.strings;

private {
  import std.math;
  import std.string;
  
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  Cell* evalToString(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".+", cell.cdr);
    char[] ret = "";
    char[] sep;
    if (cell.car.name == "TOSTRING") {
      sep = dlisp.eval(newSym("*SEP*")).strValue;
    }
    for (uint i = 0; i < args.length; i++) {
      if (i != 0) {
        ret ~= sep;
      }
      ret ~= cellToString(args[i], false, false);
    }
    return newStr(ret);
  }

  Cell* evalToChars(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "s", cell.cdr);
    Cell* makeCell(char[] source) {
      if (source.length == 0) {
        return null;
      } else {
        return newCons(newStr(source[0..1]), makeCell(source[1..$]));
      }
    }
    return makeCell(args[0].strValue);
  }

  Cell* evalSplit(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ss?", cell.cdr);
    char[] sep = dlisp.eval(newSym("*SEP*")).strValue;
    char[] delim = (args.length == 2) ? args[1].strValue : sep;
    return newStrList(split(args[0].strValue, delim));
  }
  
  Cell* evalLeft(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "is", cell.cdr);
    return newStr(args[1].strValue[0..min(args[0].intValue, args[1].strValue.length)]);
  }

  Cell* evalRight(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "is", cell.cdr);
    return newStr(args[1].strValue[max($ - args[0].intValue, 0)..$]);
  }
  
  Cell* evalSubString(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ii?s", cell.cdr);
    char[] str;
    int index = max(args[0].intValue, 0), count = int.max;
    if (args.length == 3) {
      count = args[1].intValue;
      str = args[2].strValue;
    } else {
      str = args[1].strValue;
    }
    index = min(index, str.length);
    count = min(max(count, 0), str.length - index);
    return newStr(str[index..(index + count)]);
  }

  Cell* evalStringFuncs(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "s", cell.cdr);
    switch (cell.car.name) {
      case "TRIM":
        return newStr(strip(args[0].strValue));
        break;
      case "TRIM-LEFT":
        return newStr(stripl(args[0].strValue));
        break;
      case "TRIM-RIGHT":
        return newStr(stripr(args[0].strValue));
        break;
      case "UPPER":
        return newStr(toupper(args[0].strValue));
        break;
      case "LOWER":
        return newStr(tolower(args[0].strValue));
        break;
      case "LENGTH":
        return newInt(args[0].strValue.length);
        break;
    }
  }
  
}

private {
  int min(int a, int b) {
    return a < b ? a : b;
  }
  int max(int a, int b) {
    return a > b ? a : b;
  }
}

public Environment addToEnvironment(Environment environment) {

  environment["*SEP*"] = newStr(" ");
  environment["*LN*"] = newStr("\n");

  environment.bindPredef("length", &evalStringFuncs, "(LENGTH <string>); Return <string>s length");
  environment.bindPredef("tostring", &evalToString, "(TOSTRING <obj> ...); Convert one or more objects to strings, sepearted by space if needed.");
  environment.bindPredef("tochars", &evalToChars, "(TOCHARS <string>); Split the <string> into a list of strings one character each.");
  environment.bindPredef("split", &evalSplit, "(SPLIT <string> [<delim>]); Split the <string> into a list delimited by <delim> or space.");
  environment.bindPredef("join", &evalToString, "(JOIN <string> ...); Joins all <string>s into a single string without delimeters.");

  environment.bindPredef("left", &evalLeft, "(LEFT <count> <string>); Return <count> leftmost characters of <string>.");
  environment.bindPredef("right", &evalRight, "(RIGHT <count> <string>); Return <count> rightmost characters of <string>.");
  environment.bindPredef("substring", &evalSubString, "(SUBSTRING <index> [<count>] <string>); Return <count>, or reminding characters from <index> in <string>.");
  environment.bindPredef("trim", &evalStringFuncs, "(TRIM <string>); Trim whitespaces from start and end of <string>");
  environment.bindPredef("trim-left", &evalStringFuncs, "(TRIM-LEFT <string>); Trim whitespaces from start of <string>");
  environment.bindPredef("trim-right", &evalStringFuncs, "(TRIM-RIGHT <string>); Trim whitespaces from end of <string>");
  environment.bindPredef("upper", &evalStringFuncs, "(UPPER <string>); Return <string> all upper case.");
  environment.bindPredef("lower", &evalStringFuncs, "(LOWER <string>); Return <string> all lower case");

  return environment;
}