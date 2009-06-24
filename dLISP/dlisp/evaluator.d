/*
 evaluator.d
 dLISP
 
 Author: Fredrik Olsson <peylow@treyst.se>
 Author: Klaus Blindert <klaus.blindert@web.de>

 Copyright (c) 2005 Treyst AB, <http://www.treyst.se>
 Copyright (c) 2008 Klaus Blindert

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

module dlisp.evaluator;

private {
  import std.stdio;
  import std.string;
}

public template Evaluator() {
  
  public {
    Cell*[] traceback;

    
    bool[char[]] tracefuncs;
    uint evalcount = 0;
    uint tracetabs = 2;
    uint allowLists = 0;
    
    uint tracelevel = 0;
    
    void addTrace(char[] name) {
      if (!(name in tracefuncs)) {
        tracefuncs[name] = true;
      }
    }
    
    void removeTrace(char[] name) {
      if (name in tracefuncs) {
        tracefuncs.remove(name);
      } 
    }
    
    Cell* eval(Cell* cell, bool leavebound = false) {
      scope(failure)
        traceback ~= cell;

      evalcount++;
      if (isAtom(cell)) {
        return cell;
      }
      if (isBoundValue(cell)) {
        switch (cell.cellType) {
          case CellType.ctBINT:
            return newInt(*cell.pintValue);
          case CellType.ctBFLOAT:
            return newFloat(*cell.pfloatValue);
          case CellType.ctBSTR:
            return newStr(*cell.pstrValue);        
        }
      }

      if (cell.cellType == CellType.ctSYM) {
        if( environment.isGeneric(cell.name) ) {
            if( !(environment.isBound(cell.name) && !isFunc(environment[cell.name])) )
            {
                Cell* temp = environment["GENCALL"];
                Cell* rval = eval(newList(temp,cell));
                return rval;
            }
        }

        if (environment.isBound(cell.name)) {
          Cell* temp = environment[cell.name];
          if (!leavebound && isBoundValue(temp)) {
            return eval(temp);
          } else {
            return temp;
          }
        } else {
          if( cell.name.length && cell.name[0] == ':')
            return cell;
          //writefln("XXX:", environment.globals.isBound(cell.name));
          throw new UnboundSymbolState("Unbound symbol: " ~ cell.name, cell.pos); 
        }
      }

      Cell* func = eval(cell.car);
      if( func is null )
        throw new ErrorState("Not an executable function: " ~ cellToString(cell.car),cell.pos);

      char[] name = cell.car.name;
      switch (func.cellType) {
        case CellType.ctFUNC:
          // Lots of magic!!!
          bool ismacro = func.ismacro;
          Cell* args = func.cdr;
          Cell* params = cell.cdr;
          Cell*[] macroforms;
          try {
            bool dotrace = (name in tracefuncs) != null;
            Cell* parms = null;
            if (dotrace) {
              tracelevel += tracetabs;
            }
            bool isoptional = false;
            char[] isrest = "";
	    version(debugContext) writef(func.name);
	    assert( func.context !is null );
	    assert( func.context.master !is null);
            Context ctxt = func.context;
            while (args) {
              if (args.car.name == "&OPTIONAL" ) {
                isoptional = true;
              } else if (args.car.name == "&REST") {
                args = args.cdr;
                isrest = args.car.name;
              } else {
                if (params) {
                  if (ismacro) {
                    cell = params.car;
                  } else {
                    cell = eval(params.car);
                  }
                  params = params.cdr;
                } else {
                  if (isoptional) {
                    if (isList(args.car)) {
                      cell = args.car.cdr.car;
                    } else {
                      cell = null;
                    }
                  } else {
                    throw new ArgumentState((ismacro ? "Macro " : "Function ") ~ 
                                                func.name ~ " got to few arguments.", func.pos);
                  }
                }
                if (dotrace) {
                  parms = appendToList(parms, newCons(cell, null));
                }
                if (isList(args.car)) {
                  ctxt.bind(args.car.car.name, cell);
                } else {
                  ctxt.bind(args.car.name, cell);
                }
              }
              args = args.cdr;
            }
            if (params) {
              if (isrest == "") {
                throw new ArgumentState((ismacro ? "Macro " : "Function ") ~ 
                                            func.name ~ " got to many arguments.", func.pos);
              } else {
                if (ismacro) {
                  ctxt.bind(isrest, params);
                } else {
                  Cell* rest = null;
                  while (params) {
                    rest = appendToList(rest, newCons(eval(params.car), null));
                    params = params.cdr;
                  }
                  ctxt.bind(isrest, rest);
                }
              }
            } else {
              if (isrest != "") {
                ctxt.bind(isrest, null);
              }
            }
            if (dotrace) {
              writefln(repeat(" ", tracelevel), "Trace ", name, " in: ", cellToString(parms));
            }
	    //ctxt.master = func.context;
	    environment.pushContext(ctxt);
	    scope(exit) environment.popContext;
            func = func.car;
            while (func) {
              cell = eval(func.car);
              if (ismacro) {
//                 writefln("MACROFORMS: ",cellToString(cell));
                macroforms ~= cell;
              }
              func = func.cdr;
            }
            
          } finally {
//            environment.popContext();
          }
          if (ismacro) {
            foreach (Cell* mcell; macroforms) {
              // writefln(cellToString(mcell));
              cell = eval(mcell);
            }
          }
          break;

        case CellType.ctPREDEF:
            cell = func.func(this, cell);
          break;

        case CellType.ctCONS:
            func = newCons(eval(func.car),func.cdr);
            //writefln("Evaluating list to: ",cellToString(func));
            cell = eval(func);
          break;
        default:
            throw new EvalState("Not a function: " ~ cellToString(cell.car), func.pos);
      }
      if (name in tracefuncs) {
        writefln(repeat(" ", tracelevel), "Trace ", name, " out: ", cellToString(cell));
        tracelevel -= tracetabs;
      }
      return cell;
    }
    
  }
  
}
