/*
  lists.d
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

module dlisp.predefs.lists;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  Cell* evalFirst(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "l", cell.cdr);
    if (args[0])
      return args[0].car;
    else
      return null;
  }
  
  Cell* evalRest(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "l", cell.cdr);
    if (args[0])
      return args[0].cdr;
    else
      return null;
  }

  Cell* evalElements(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "l", cell.cdr);
    return newInt(listLen(args[0]));
  }
  
  Cell* evalNth(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "il", cell.cdr);
    uint nth = args[0].intValue;
    Cell* list = args[1];
    for (int i = 0; i < nth; i++) {
      list = list.cdr;
      if (!list) {
        throw new EvalState("List index out of bounds", args[0].pos);
      }
    }
    return list.car;
  }

  Cell* evalFrom(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "il", cell.cdr);
    uint nth = args[0].intValue;
    Cell* list = args[1];
    for (int i = 0; i < nth; i++) {
      list = list.cdr;
      if (!list) {
        throw new EvalState("List index out of bounds", args[0].pos);
      }
    }
    return list;
  }
  
  Cell* evalLast(DLisp dlisp, Cell* cell) {
    uint cnt, len;
    Cell*[] args = evalArgs(dlisp, "li?", cell.cdr);
    cnt = args.length == 1 ? 1 : args[1].intValue;
    Cell* list = args[0];
    if (list) {
      len = listLen(list);
      if (cnt > len) {
        cnt = len;
      }
      while (cnt < len) {
        list = list.cdr;
        cnt++;
      }
    }
    return list;
  }
  
  Cell* evalCons(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "..", cell.cdr);
    return newCons(args[0], args[1]);
  }
  
  // List functions
  Cell* evalList(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".*", cell.cdr);
    Cell* ret = null;
    for (int i = args.length - 1; i >= 0; i--) {
      ret = newCons(args[i], ret);
    }
    return ret;
  }
  
  Cell* evalAssoc(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".l", cell.cdr);
    Cell* item = args[0];
    args = evalArgs(dlisp, "'l*", args[1]);
    foreach (Cell* arg; args) {
      if (eqCell(item, arg.car)) {
        return arg;
      }
    }
    return null;
  }

  Cell* evalMember(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".l", cell.cdr);
    Cell* item = args[0];
    cell = args[1];
    while (cell) {
      if (eqCell(item, cell.car)) {
        return cell;
      }
      cell = cell.cdr;
    }
    return null;
  }
  
  Cell* evalCopy(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "i?.", cell.cdr);
    if (args.length == 2) {
      return copyList(args[1], args[0].intValue);
    } else {
      return copyList(args[0], uint.max);
    }
  }
  
  Cell* evalAppend(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".*", cell.cdr);
    if (args.length == 0) {
      return null;
    } else {
      uint cnt = args.length - 1;
      Cell* ret = args[cnt];
      while (cnt) {
        cnt--;
        ret = appendToList(args[cnt], ret);
      } 
      return ret;
    }
  }
  
  Cell* evalDistinct(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "l", cell.cdr);
    Cell*[] res;
    Cell* first, cur, test;
    first = cur = args[0];
    for (cur = first; cur != null; cur = cur.cdr) {
      for (test = first; test != cur; test = test.cdr) {
        if (eqCell(test.car, cur.car, true)) {
          break;
        }
      }
      if (test == cur) {
        res ~=cur.car;
      }
    }
    return newList(res);
  }
  
  Cell* evalUnion(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ll", cell.cdr);
    Cell*[] ret = listToArray(args[0]);
    Cell* cur;
    for (cur = args[1]; cur != null; cur = cur.cdr) {
      bool exists = false;
      foreach (Cell* test; ret) {
        if (eqCell(test, cur.car, true)) {
          exists = true;
          break;
        }
      }
      if (!exists) {
        ret ~= cur.car;
      }
    }
    return newList(ret);
  }
  
  Cell* evalIntersect(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ll", cell.cdr);
    Cell*[] lista, listb, ret;
    lista = listToArray(args[0]);
    listb = listToArray(args[1]);
    foreach (Cell* cur; lista) {
      foreach (Cell* test; listb) {
        if (eqCell(cur, test, true)) {
          ret ~= cur;
          break;
        }
      }
    }
    return newList(ret);
  }

  Cell* evalDifference(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ll", cell.cdr);
    Cell*[] lista, listb, ret;
    lista = listToArray(args[0]);
    listb = listToArray(args[1]);
    for (uint i = 0; i < 2; i++) {
      foreach (Cell* cur; lista) {
        bool exists = false;
        foreach (Cell* test; listb) {
          if (eqCell(cur, test, true)) {
            exists = true;
            break;
          }
        }
        if (!exists) {
          ret ~= cur;
        }
      }
      Cell*[] tlist = lista;
      lista = listb;
      listb = tlist;
    }
    return newList(ret);
  }
  
  Cell* evalSort(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ol", cell.cdr);
    Cell*[] ret = listToArray(args[1]);
    if (ret.length > 1) {
      // Create a (foo nil nil) list to use for calls later
      Cell* call = newCons(args[0], newCons(null, newCons(null, null)));
      for (uint i = 0; i < ret.length - 1; i++) {
        call.cdr.car = ret[i + 1];
        call.cdr.cdr.car = ret[i];
        if (isTrue(dlisp.eval(call))) {
          Cell* tcell = ret[i];
          ret[i] = ret[i + 1];
          ret[i + 1] = tcell;
          i = -1;
        }
      }
      return newList(ret);
    } else {
      return args[0];
    }
  }
  
  Cell* evalMap(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ol", cell.cdr);
    Cell*[] ret = listToArray(args[1]);
    if (ret.length > 0) {
      // Create a (foo nil) list to use for calls later
      Cell* call = newCons(args[0], newCons(null, null));
      for (uint i = 0; i < ret.length; i++) {
        call.cdr.car = ret[i];
        ret[i] = dlisp.eval(call);
      }
      return newList(ret);
    } else {
      return null;
    }
  }
  
}

public Environment addToEnvironment(Environment environment) {
  
  environment.bindPredef("elements", &evalElements, "(ELEMENTS <list>); Return the length of a list.");

  environment.bindPredef("first", &evalFirst, "(CAR <cons>); Return the value of a cons-cell.");
  environment.bindPredef("rest", &evalRest, "(CDR <cons>); Return the rest of a cons-cell.");
  environment.bindPredef("nth", &evalNth, "(NTH <int> <list>); Retrieve nth item of list, list is zero based.");
  environment.bindPredef("from", &evalFrom, "(FROM <int> <list>); Retrieve reminder of <list> from nth element, list is zero based.");
  environment.bindPredef("last", &evalLast, "(LAST <list>); Retrive the last conses (not element) of list.");

  environment.bindPredef("cons", &evalCons, "(CONS <car> <cdr>); Create a new cons-cell with <car> as the value and <cdr> as the rest.");
  environment.bindPredef("list", &evalList, "(LIST <item> ...); Returns a list with all <item>s."); 
  environment.bindPredef("assoc", &evalAssoc, "(ASSOC <item> <alist>); Returns the first list in <alist> where the car matches <item>.");
  environment.bindPredef("member", &evalMember, "(MEMBER <item> <alist>); Returns the tail of first element in <alist> matching <item>.");
  environment.bindPredef("copy", &evalCopy, "(COPY <object> [<count>]); Returns a copy of <object>, if <object> is a list then copy for max <count> items, or until end. Only the structure, not the elements are copied.");
  environment.bindPredef("append", &evalAppend, "(APPEND <list> ...); Apends <list>s to a single list, does *not* copy original lists."); 
  
  environment.bindPredef("distinct", &evalDistinct, "(DISTINCT <list>); Returns a copy of <list> with duplicates removed");
  environment.bindPredef("union", &evalUnion, "(UNION <lista> <lista>); Returns the union of <lista> and <listb>"); 
  environment.bindPredef("intersect", &evalIntersect, "(INTERSECT <lista> <lista>); Returns the intersection of <lista> and <listb>");
  environment.bindPredef("difference", &evalDifference, "(DIFFERENCE <lista> <lista>); Returns the difference of <lista> and <listb>");

  environment.bindPredef("sort", &evalSort, "(SORT <func> <list> ); Sorts <list> using <func> as comparator. <func> must take two arguments and return true if and only if first argument is logically less than second.");
  environment.bindPredef("map", &evalMap, "(MAP <func> <list>); Maps <list> using <func> as comparator, and returns a new list. <func> must take at least one argument.");

  environment.clonePredef("first", "car");
  environment.clonePredef("rest", "cdr");
  
  return environment;
  
}