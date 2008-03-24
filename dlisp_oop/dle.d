/*
  dle.d
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

module dle;

import std.stdio;
import std.cstream;
import dlisp.concell;
import dlisp.dlisp;
import dlisp.predefs.all;
import dlisp.bind;

int main(char[][] args) {
  DLisp dlisp = new DLisp(addAllToEnvironment(new Environment()));
  
  Cell* cell = dlisp.parseEvalPrint(`(LOAD "system.lisp" T)`, true);


  int inputcount = 0;
  char[] inputprompt = "> ";
  char[] input;
  char[] resultprompt = "=> ";

  dlisp.environment.pushScope();

  dlisp.environment.bindValue("*input-count*", &inputcount);
  dlisp.environment.bindValue("*input-prompt*", &inputprompt);
  dlisp.environment.bindValue("*input-string*", &input);
  dlisp.environment.bindValue("*result-prompt*", &inputprompt);
  
  dout.writeLine(dlisp.versionString());

  foreach(string filename; args[1..$])
  {
    dlisp.eval(dlisp.parse("(load \"" ~ filename ~ "\")"));
  }

  while(1) {
    dout.writeString(inputprompt);
    input = din.readLine();
    inputcount++;
    if (input != "") {
      if (input == "quit") 
        break;
      try {
        dlisp.tracelevel = 0;
        Cell* cell_ = dlisp.parse(input);
        cell_ = dlisp.eval(cell_);
        dout.writeLine(resultprompt ~ cellToString(cell_));
      } catch (DLispState e) {
        dout.writeLine("RTE: " ~ e.toString() ~ " POS: " ~ toString(e.pos.row) ~ "/" ~toString(e.pos.col));
        dout.writeLine("STATES: " ~ join(e.stateNames," - "));
      }
    }
  }
  
  return 0;
}