/*
  core.d
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

module dlisp.predefs.core;

private {
  import std.date;
  import std.stdio;
  import std.stream;
  import std.string;
  
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {

  Cell* evalLoad(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "sb?", cell.cdr);
    try {
      bool silent = args.length > 1 ? isTrue(args[1]) : false;
      dlisp.parseEvalPrint(new File(args[0].strValue), silent);
    } catch (Exception e) {
      throw new FileState("Could not load " ~ args[0].strValue ~ " : " ~ e.toString());
    } finally {
      dlisp.environment.refresh();
    }
    return newSym("t");
  }
  
  Cell* evalHelp(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "o?", cell.cdr);
    if (args.length == 1) {
      return newStr(args[0].docs);
    } else {
      cell = null;
      char[][] funcs = dlisp.environment.allFuncs();
      foreach(char[] func; funcs) {
        cell = appendToList(cell, newStr(func));
      }
      return cell;
    }
  }
  
  Cell* evalTrace(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y*", cell.cdr);
    if (args.length > 0) {
      foreach(Cell* arg; args) {
        dlisp.addTrace(arg.name);
      }
      return cell.cdr;
    } else {
      return newSymList(dlisp.tracefuncs.keys);
    }
  }
  
  Cell* evalUntrace(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y*", cell.cdr);
    if (args.length > 0) {
      foreach(Cell* arg; args) {
        dlisp.removeTrace(arg.name);
      }
      return cell.cdr;
    } else {
      Cell* ret = newSymList(dlisp.tracefuncs.keys);
      foreach(char[] key; dlisp.tracefuncs.keys) {
        dlisp.tracefuncs.remove(key);
      }
      return ret;
    }
  }
  
  Cell* evalTime(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'l", cell.cdr);
    d_time start = getUTCtime();
    dlisp.evalcount = 0;
    dlisp.conscount = 0;
    dlisp.atomcount = 0;
    cell = dlisp.eval(args[0]);
    writefln("; Evaluation took: %.3f seconds.", cast(real)(getUTCtime() - start) / TicksPerSecond);
    writefln(";   %d conses and %d atoms in %dkb allocated.", conscount, atomcount, ((conscount + atomcount) * 28) / 1024);
    return cell;
  }
  
  Cell* evalEval(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".", cell.cdr);
    return dlisp.eval(args[0]);
  }

  Cell* evalFunCall(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ol", cell.cdr);
    return dlisp.eval(newCons(args[0], args[1]));
  }
  
  Cell* evalParse(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".", cell.cdr);
    if (isString(args[0])) {
      return dlisp.parse(args[0].strValue);
    } else if (isStream(args[0])) {
      return dlisp.parse(args[0].streamValue);
    } else {
      throw new ArgumentState("Could not evaluate argument as: s or t", args[0].pos);
    }
  }
  
  Cell* evalQuote(DLisp dlisp, Cell* cell) {
    // Should we be allowed to quote nulls?
    return cell.cdr.car;
  }


  Cell* evalBack_Quote(DLisp dlisp, Cell* cell) {
    // Should we be allowed to quote nulls?
    Cell* backQuote(Cell* cell) {
      if (!cell) {
        return null;
      }
      if (isCons(cell)) {
        if (isSym(cell.car) && cell.car.name == "COMMA-QUOTE") {
          return dlisp.eval(cell.cdr.car);
        } else {
          return newCons(backQuote(cell.car), backQuote(cell.cdr));
        }
      }
      return cell;
    }
    return backQuote(cell.cdr.car);
  }

  Cell* evalComma_Quote(DLisp dlisp, Cell* cell) {
    throw new EvalState("Comma not inside back quotes.", cell.pos);
  }
  
  Cell* evalFunction(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'l'.+", cell.cdr);
    cell = newFunc(cell.cdr.cdr, args[0]);
    return cell;
  }
  
  Cell* evalDefun(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y'l's?'.+", cell.cdr);
    char[] docs = "";
    Cell* fbody;
    if (args.length > 3) {
      docs = args[2].strValue;
      fbody = cell.cdr.cdr.cdr.cdr;
    } else {
      docs = "";
      fbody = cell.cdr.cdr.cdr;
    }
    cell = newFunc(fbody, args[1], docs, args[0].name);
    dlisp.environment()[cell.name] = cell;
    return cell;
  }
  
  Cell* evalDefmacro(DLisp dlisp, Cell* cell) {
    cell = evalDefun(dlisp, cell);
    cell.ismacro = true;
    return cell;
  }
    
}

public Environment addToEnvironment(Environment environment) {

  environment["T"] = newSym("T");
  environment["NIL"] = null;
    
  environment.bindPredef("help", &evalHelp, "(HELP <sym>); Return help for documented symbol.");
  environment.bindPredef("load", &evalLoad, "(LOAD <path> [<silent>]); Load, parse and execute file at <path>, supress output if <silent> is true (false is default)."); 
  environment.bindPredef("trace", &evalTrace, "(TRACE [<sym> ...]); Add trace on functions, if no symbol is given a list of currently traced functions is returned.");
  environment.bindPredef("untrace", &evalUntrace, "(UNTRACE [<sym> ...]); Remove trace from functions, if no symbols given all traces are removed.");
  environment.bindPredef("time", &evalTime, "(TIME <form>); Evaluate and return <form>s result, and print timing information.");

  environment.bindPredef("eval", &evalEval,"(EVAL <arg>); Execute <arg>.");
  environment.bindPredef("funcall", &evalFunCall,"(EVALFUN <func> <list>); Execute function or macro <func> with arguments in <list>.");
  environment.bindPredef("parse", &evalParse,"(PARSE <str>|<stream>); Parse and return the string <str> or the string in stream <stream>, as lisp program.");
  environment.bindPredef("quote", &evalQuote, "(QUOTE <sym>); Quotes symbol <sym>, equilent to \"'<sym>\".", true);
  environment.bindPredef("back-quote", &evalBack_Quote); 
  environment.bindPredef("comma-quote", &evalComma_Quote); 
  environment.bindPredef("function", &evalFunction, "(FUNCTION (<arg> ...) <body>); Returns unnamed function with arguments <arg> and body <body>.", true);
  environment.clonePredef("function", "lambda");
  environment.bindPredef("defun", &evalDefun, "(DEFUN <sym> (<arg> ...) [<doc>] <body>); Defines function <sym> with arguments <arg> and body <body>, optionaly with documentation <doc>.", true);
  environment.bindPredef("defmacro", &evalDefmacro, "(DEFMACRO <sym> (<arg> ...) [<doc>] <body>); Defines macro <sym> with arguments <arg> and body <body>, optionaly with documentation <doc>.", true);
  
  return environment;
  
}
