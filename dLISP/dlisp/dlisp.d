/*
 dlisp.d
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

module dlisp.dlisp;

public {
  import dlisp.concell;
  import dlisp.environment;
  import dlisp.concellhelpers;
  import dlisp.oopextension;
  import dlisp.states;
  import dlisp.types;
}

private {
  import std.string;
  import dlisp.evaluator;
  import dlisp.parser;
}

const string VERSION = "0.100.1";

const int    VERSION_MAJOR = 0;
const int    VERSION_MINOR = 100;
const int    VERSION_PATCH = 1;


public class DLisp : IDLisp {
    
    private {
      IEnvironment _environment;
    }
    
    public {
      
      char[] versionString() {
        return "dLISP environment v" ~ VERSION ~ " (" ~ std.string.toString(Cell.sizeof) ~ ")";
      }
      
      this(IEnvironment environment) {
        _environment = environment;
        _environment["*version-string*"] = newStr(versionString());
      }
      
      IEnvironment environment() {
        return _environment;
      }
        
      mixin Evaluator;

      mixin Parser;

    }
  
  }
