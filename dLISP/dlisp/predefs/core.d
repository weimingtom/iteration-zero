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
  import std.conv;
  import std.stdio;
  import std.stream;
  import std.string;
  
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {

  Cell* evalLoad(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "sb?", cell.cdr);
    try {
      bool silent = args.length > 1 ? isTrue(args[1]) : false;
      auto file = new File(args[0].strValue);

      // Skip she bang
      char first_char = file.getc();
      if( first_char == '#' )
         file.readLine();
      else
         file.ungetc(first_char);
    
      // pep the stream.
      dlisp.parseEvalPrint(file, silent);
    } catch (Exception e) {
      throw new FileState("Could not load " ~ args[0].strValue ~ " : " ~ e.toString());
    } finally {
      dlisp.environment.refresh();
    }
    return newSym("t");
  }
  
  Cell* evalHelp(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "o?", cell.cdr);
    if (args.length == 1) {
      return newStr(args[0].docs);
    } else {
      cell = null;
      char[][] funcs;//FIXME: = dlisp.environment.allFuncs();
      foreach(char[] func; funcs) {
        cell = appendToList(cell, newStr(func));
      }
      return cell;
    }
  }
  
  Cell* evalTrace(IDLisp dlisp, Cell* cell) {
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
  
  Cell* evalUntrace(IDLisp dlisp, Cell* cell) {
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
  
  Cell* evalTraceback(IDLisp dlisp, Cell* cell) {
      foreach(int i, Cell* tracedCell; dlisp.traceback) {
        writefln("--FRAME #%d ------------------------------------\n%s",i,cellToString(tracedCell));
      }
      return nil;
  }

  Cell* evalTime(IDLisp dlisp, Cell* cell) {
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
  
  Cell* evalEval(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".", cell.cdr);
    return dlisp.eval(args[0]);
  }

  Cell* evalFunCall(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "ol", cell.cdr);
    return dlisp.eval(newCons(args[0], args[1]));
  }

  Cell* evalParse(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".", cell.cdr);
    if (isString(args[0])) {
      return dlisp.parse(args[0].strValue);
    } else if (isStream(args[0])) {
      return dlisp.parse(args[0].streamValue);
    } else {
      throw new ArgumentState("Could not evaluate argument as: string or stream", args[0].pos);
    }
  }
  
  Cell* evalQuote(IDLisp dlisp, Cell* cell) {
    // Should we be allowed to quote nulls?
    return cell.cdr.car;
  }


  Cell* evalBack_Quote(IDLisp dlisp, Cell* cell) {
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

  Cell* evalComma_Quote(IDLisp dlisp, Cell* cell) {
    throw new EvalState("Comma not inside back quotes.", cell.pos);
  }
  
  Cell* evalSymbol(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "s", cell.cdr);
    return newSym(args[0].strValue);
  }

  Cell* evalFunction(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'l'.+", cell.cdr);
    cell = newFunc(cell.cdr.cdr, args[0]);
    if( dlisp.environment.context.master !is null )
       cell.context = dlisp.environment.context.dup;
    else { cell.context = new Context; cell.context.master = dlisp.environment.globals; }
    return cell;
  }
  
  Cell* evalDefun(IDLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y'l's?'.+", cell.cdr);
    char[] docs = "";
    Cell* fbody;
    if (args.length > 3 && isString(args[2]) ) {
      docs = args[2].strValue;
      fbody = cell.cdr.cdr.cdr.cdr;
    } else {
      docs = "";
      fbody = cell.cdr.cdr.cdr;
    }
    if( args[0].name[0] == ':' )
      throw new TypeState("""Can't bind keyword symbol '""" ~ args[0].name ~ """'""",cell.pos);

    cell = newFunc(fbody, args[1], docs, args[0].name);
    if( dlisp.environment.context.master )
       cell.context = dlisp.environment.context.dup;
    else { cell.context = new Context; cell.context.master = dlisp.environment.globals; }
    dlisp.environment.globals.bind(cell.name,cell);
    return cell;
  }
  
  Cell* evalDefmacro(IDLisp dlisp, Cell* cell) {
    cell = evalDefun(dlisp, cell);
    cell.ismacro = true;
    return cell;
  }
    
}

public IEnvironment addToEnvironment(IEnvironment environment) {

  environment["T"] = newSym("T");
  environment["NIL"] = null;
    
  environment.bindPredef("help", &evalHelp, "(HELP <sym>); Return help for documented symbol.");
  environment.bindPredef("load", &evalLoad, "(LOAD <path> [<silent>]); Load, parse and execute file at <path>, supress output if <silent> is true (false is default)."); 
  environment.bindPredef("trace", &evalTrace, "(TRACE [<sym> ...]); Add trace on functions, if no symbol is given a list of currently traced functions is returned.");
  environment.bindPredef("untrace", &evalUntrace, "(UNTRACE [<sym> ...]); Remove trace from functions, if no symbols given all traces are removed.");
  environment.bindPredef("traceback", &evalTraceback, "(TRACEBACK); Print and clear current traceback.");
  environment.bindPredef("time", &evalTime, "(TIME <form>); Evaluate and return <form>s result, and print timing information.");

  environment.bindPredef("eval", &evalEval,"(EVAL <arg>); Execute <arg>.");
  environment.bindPredef("funcall", &evalFunCall,"(FUNCALL <func> <list>); Execute function or macro <func> with arguments in <list>.");
  environment.clonePredef("funcall","apply");
  environment.bindPredef("parse", &evalParse,"(PARSE <str>|<stream>); Parse and return the string <str> or the string in stream <stream>, as lisp program.");
  environment.bindPredef("quote", &evalQuote, "(QUOTE <sym>); Quotes symbol <sym>, equivalent to \"'<sym>\".", true);
  environment.bindPredef("back-quote", &evalBack_Quote); 
  environment.bindPredef("comma-quote", &evalComma_Quote); 
  environment.bindPredef("function", &evalFunction, "(FUNCTION (<arg> ...) <body>); Returns unnamed function with arguments <arg> and body <body>.", true);
  environment.clonePredef("function", "lambda");
  environment.bindPredef("defun", &evalDefun, "(DEFUN <sym> (<arg> ...) [<doc>] <body>); Defines function <sym> with arguments <arg> and body <body>, optionaly with documentation <doc>.", true);
  environment.bindPredef("defmacro", &evalDefmacro, "(DEFMACRO <sym> (<arg> ...) [<doc>] <body>); Defines macro <sym> with arguments <arg> and body <body>, optionaly with documentation <doc>.", true);
  environment.bindPredef("symbol", &evalSymbol, "(SYMBOL <string>); Generates a symbol from a string.", true);
  
  return environment;
  
}
