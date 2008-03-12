/*
  control.d
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


module dlisp.predefs.control;

private {
  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  class ReturnFromState : DLispState {
    Cell* value;
    char[] block;
    this(char[] msg, char[] block, Cell* value = null) {
      this.block = block;
      this.value = value;
      super(msg);
    }
  }
  
  class GoState : DLispState {
    Cell* tag;
    this(char[] msg, Cell* tag) {
      this.tag = tag;
      super(msg);
    }
  }
  
  Cell* evalLet(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'l'.*", cell.cdr);
    Cell*[] locs = evalArgs(dlisp, "'l+", args[0]);
    // Do this in paralell somehow :/
    Cell*[char[]] vars;
    foreach (Cell* pair; locs) {
      Cell*[] arg = evalArgs(dlisp, "'y.?", pair);
      vars[arg[0].name] = (arg.length == 2) ? arg[1] : null;
    }
    dlisp.environment.pushScope();
    try {
      foreach(char[] name, Cell* value; vars) {
        dlisp.environment.addLocal(name, value);
      }
      for (uint i = 1; i < args.length; i++) {
        cell = dlisp.eval(args[i]);
      }
    } finally {
      dlisp.environment.popScope();
    }
    return cell;
  }
  
  Cell* evalProgn(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, ".*", cell.cdr);
    if (args.length == 0) {
      return null;
    } else {
      return args[$ - 1];    
    }
  }
  
  Cell* evalBlock(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y'.*", cell.cdr);
    Cell* ret = null;
    try {
      for (uint i = 1; i < args.length; i++) {
        ret = dlisp.eval(args[i]);
      }
    } catch (ReturnFromState rfe) {
      if (rfe.block == args[0].name) {
        ret = rfe.value;
      } else {
        throw rfe;
      }
    }
    return ret;
  }
  
  Cell* evalReturnFrom(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y.?", cell.cdr);
    throw new ReturnFromState("RETURN-FROM " ~ args[0].name, args[0].name,
                                  args.length > 1 ? args[1] : null);
    return null;
  }
  
  Cell* evalTagbody(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'.*", cell.cdr);
    uint s = 0;
start:
      for (uint i = s; i < args.length; i++) {
        try {
          if (!(isSym(args[i]) && !dlisp.environment.isBound(args[i].name))) {
            cell = dlisp.eval(args[i]);
          }
        } catch (GoState ge) {
          for (uint j = 0; j < args.length; j++) {
            if (isSym(args[j]) && !dlisp.environment.isBound(args[j].name)) {
              if (args[j].name == ge.tag.name) {
                s = j;
                goto start;
              }
            }
          }
          throw ge;
        }
      }
    return null;
  }
  
  Cell* evalGo(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'y", cell.cdr);
    throw new GoState("GO " ~ cellToString(args[0]), args[0]);
    return null;
  }

  Cell* evalCatch(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'l'.*", cell.cdr);
    Cell*[] cargs = evalArgs(dlisp, "'y'y?'.*", args[0]);
    Cell* ret = null;
    try {
      for (uint i = 1; i < args.length; i++) {
        ret = dlisp.eval(args[i]);
      }
    } catch (DLispState state) {
      bool isState(Cell* cell, DLispState state) {
        foreach(char[] statename; state.stateNames()) {
          if (statename == cell.name) {
            return true;
          }
        }
        return false;
      }
      if (isState(cargs[0], state)) {
        Cell* statelist = newList(newSym(state.stateName()), 
                                  newStr(state.toString()), 
                                  state.pos.row > 0 ? 
                                      newList(newInt(state.pos.row), 
                                              newInt(state.pos.col)) 
                                    : null);
        dlisp.environment()["*STATE*"] = statelist;
        if (cargs.length > 1) {
          dlisp.environment()[cargs[1].name] = statelist;
          for (uint i = 2; i < cargs.length; i++) {
            ret = dlisp.eval(cargs[i]);
          }
        } else {
          ret = null;
        }
      } else {
        throw state;
      }
    }
    return ret;
  }

  
  Cell* evalIf(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "b'.'.?", cell.cdr);
    if (isTrue(args[0])) {
      return dlisp.eval(args[1]);
    } else {
      if (args.length > 2) {
        return dlisp.eval(args[2]);
      } else {
        return null;
      }
    }
  }
  
  Cell* evalWhen(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "b'.+", cell.cdr);
    if (isTrue(args[0])) {
      for (uint i = 1; i < args.length; i++) {
        cell = dlisp.eval(args[i]);
      }
      return cell;
    } else {
      return null;
    }
  }
  
  Cell* evalUnless(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "b'.+", cell.cdr);
    if (!isTrue(args[0])) {
      for (uint i = 1; i < args.length; i++) {
        cell = dlisp.eval(args[i]);
      }
      return cell;
    } else {
      return null;
    }
  }
  
  Cell* evalCond(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "'l*", cell.cdr);
    foreach(Cell* arg; args) {
      Cell*[] test = evalArgs(dlisp, "b'.*", arg);
      if (isTrue(test[0])) {
        Cell* ret = null;
        for (uint i = 1; i < test.length; i++) {
          ret = dlisp.eval(test[i]);
        }
        return ret;
      }
    }
    return null;
  }

}

public Environment addToEnvironment(Environment environment) {

  environment["*STATE*"] = null;

  environment.bindPredef("let", &evalLet, "(LET ((<sym> <cons>) ...) <form> ...); Bind all <sym>s to <cons> in paralell and then valauta all <form>s.");
  environment.bindPredef("progn", &evalProgn, "(PROGN [<form>] ...); Evaluates all form in the order they apear, and returns the result of the last form.");
  environment.bindPredef("block", &evalBlock, "(BLOCK <sym> [<form>] ...); Establishes a block named <sym> and evaluates all forms as an implicit progn. See RETURN-FROM.");
  environment.bindPredef("return-from", &evalReturnFrom, "(RETURN-FROM <sym> [<value>]); Return control and optional value to enclosing block. See BLOCK.");
  environment.bindPredef("tagbody", &evalTagbody, "(TAGBODY {<tag>|<form>} ...); Evaluates all forms. See GO.");
  environment.bindPredef("go", &evalGo, "(GO <tag>); Return control to form following <tag> in enclosing tagbody. See TAGBODY.");
  environment.bindPredef("catch", &evalCatch, "(CATCH (<sym> [<sym> <form> ...]) <form> ...);");
  
  environment.bindPredef("if", &evalIf, "(IF <bool> <true> [<false>]); Executes <true> if <bool> is true, optionaly executes <false> if <bool> is false.");
  environment.bindPredef("when", &evalWhen, "(WHEN <bool> <form> ...); Executes <form>s and returns the result of the last form if <bool> yields true, otherwise returns nil");
  environment.bindPredef("unless", &evalUnless, "(UNLESS <bool> <form> ...); Executes <form>s and returns the result of the last form if <bool> yields false, otherwise returns nil");
  environment.bindPredef("cond", &evalCond, "(COND (<test> <form> ...) ...); Evaluates only <test> of each form until yields true, then evaluates each <form> returning the last result. Or NIL if no condition met.", true);
  
  return environment;
  
}