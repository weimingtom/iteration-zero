/*
  stream.d
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


module dlisp.predefs.stream;

private {
  import std.stream;
  import std.string;
  import std.cstream;

  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

public {
  
  Cell* evalOpen(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "s", cell.cdr);
    try {
      return newStream(new File(args[0].strValue, FileMode.In | FileMode.Out));
    } catch (Exception e) {
      throw new FileState("Could no open " ~ args[0].strValue);
    }
  }
  
  Cell* evalClose(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "t", cell.cdr);
    try {
      args[0].streamValue.close();
    } catch (Exception e) {
      throw new FileState("Could not close stream");
    }
    return null;
  }
  
  Cell* evalRead_Int(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "t", cell.cdr);
    if (args[0].streamValue.eof()) {
      return null;
    }
    try {
      int value;
      args[0].streamValue.read(value);
      return newInt(value);
    } catch (Exception e) {
      throw new FileState("Could not read-line.");
    }
  }
  
  Cell* evalRead_Float(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "t", cell.cdr);
    if (args[0].streamValue.eof()) {
      return null;
    }
    try {
      real value;
      args[0].streamValue.read(value);
      return newFloat(value);
    } catch (Exception e) {
      throw new FileState("Could not read-line.");
    }
  }
  
  Cell* evalRead_Line(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "t", cell.cdr);
    if (args[0].streamValue.eof()) {
      return null;
    }
    try {
      return newStr(args[0].streamValue.readLine());
    } catch (Exception e) {
      throw new FileState("Could not read-line.");
    }
  }
  
  Cell* evalWrite(DLisp dlisp, Cell* cell) {
    Cell*[] args = evalArgs(dlisp, "t.+", cell.cdr);
    cell = args[0];
    char[] sep = dlisp.eval(newSym("*SEP*")).strValue;
    for (uint i = 1; i < args.length; i++) {
      char[] sout = cellToString(args[i]);
      if (isString(args[i])) {
        sout = sout[1..$-1];
      }
      cell.streamValue.writeString(sout);
      if (i < (args.length - 1)) {
        cell.streamValue.writeString(sep);
      }
    }
    return args[$ - 1];
  }
  
}

public Environment addToEnvironment(Environment environment) {
  
  environment["*STD-IN*"] = newStream(din);
  environment["*STD-OUT*"] = newStream(dout);
  environment["*STD-ERR*"] = newStream(derr);
  
  environment.bindPredef("open", &evalOpen, "(OPEN <path>); Open file at <path> as a stream, creates new file if it does not exist.");
  environment.bindPredef("close", &evalClose, "(CLOSE <stream>); Close stream.");
  environment.bindPredef("read-int", &evalRead_Int, "(READ-INT <stream>); Read and return int from <stream>.");
  environment.bindPredef("read-float", &evalRead_Float, "(READ-FLOAT <stream>); Read and return float from <stream>.");
  environment.bindPredef("read-line", &evalRead_Line, "(READ-LINE <stream>); Read and return string from <stream>.");
  environment.bindPredef("write", &evalWrite, "(WRITE <obj> ...); write sequential objects to stream, separated by *sep*. Return last object.");
  
  return environment;
  
}