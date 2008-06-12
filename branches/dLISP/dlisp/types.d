/*
  types.d
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

module dlisp.types;

private {
   import std.boxer;
   import std.stream;
   import std.cstream;
}

public {

   class IDLisp {

      abstract void addTrace(string name);
      abstract void removeTrace(string name);

      abstract Cell* parseEvalPrint(char[] source, bool silent = false);
      abstract Cell* parseEvalPrint(Stream stream, bool silent = false, bool dotraceback = true);
      abstract Cell* parse(Stream stream);
      abstract Cell* parse(string str);
      abstract Cell* eval(Cell* cell, bool leavebound = false);
      abstract IEnvironment environment();

      int conscount;
      int atomcount;
      int evalcount;

      Cell* traceback[];
      bool[char[]] tracefuncs;
   }


  struct Pos {
    uint row = 0;
    uint col = 0;
  }

  typedef Cell* delegate(IDLisp, Cell*) PredefFunc;
  typedef Cell* function(IDLisp, Cell*) PredefFuncPtr;
  
  enum CellType {
    ctSYM, ctINT, ctFLOAT, ctSTR, 
    ctSTREAM, ctCONS, ctFUNC, ctPREDEF,
    ctOBJECT,
    ctBINT, ctBFLOAT, ctBSTR
  }

  struct Cell {
    CellType cellType;
    char[] name;
    Pos pos;
    union {
      int intValue;         // ctINT
      int* pintValue;       // ctBINT
      real floatValue;      // ctFLOAT
      real* pfloatValue;    // ctBFLOAT
      char[] strValue;      // ctSTR
      char[]* pstrValue;    // ctBSTR
      Stream streamValue;   // ctSTREAM
      struct {              // ctCONS and ctFUNC, ctPREDEF
        bool ismacro;       // ctFUNC, ctPREDEF
        union {
          char[] docs;      // ctFUNC, ctPREDEF
          bool visited;
        }
        Context context; // Context
	Cell*[string] table;
        union {
          struct {
            Cell* car, cdr; // ctCONS, ctFUNC
          }
          struct {
            Box instance;
          }
          PredefFunc func;  // ctPREDEF
        }
      }
    }
  }

class Context {
    private Cell*[string] _locals;
    public Context master;

    void refresh()
    {
       _locals.rehash;
    }

    Context dup()
    {
       Context copy = new Context;
       //copy._locals = _locals.dup;
       foreach(string key, Cell* cell; _locals)
	  copy._locals[key] = cell;
       copy.master = master;
       return copy;
    }

    bool isBound(string name)
    {
       if( (name in _locals) !is null)
	  return true;
       if( master )
	  return master.isBound(name);
       return false;
    }

    Cell* bind(string name, Cell* cell)
    {
        _locals[name] = cell;
	return cell;
    }

    string[] allKeys()
    {
       string[] keys = _locals.keys;
//        if( master )
// 	  keys ~= master.allKeys;
       return keys;
    }

    Cell* opIndex(string name) {
       if( (name in _locals) !is null )
	  return _locals[name];
       if( master )
	  return master[name];
       //throw new Exception("Unbound symbol: " ~ name);
       return null;
    }
}

interface IEnvironment {
    void bindDelegate(char[] name, PredefFunc func, char[] docs = "", bool ismacro = false);
    void bindPredef(char[] name, PredefFuncPtr funcPtr, char[] docs = "", bool ismacro = false);
    void bindValue(char[] name, int* pointer);
    void bindValue(char[] name, real* pointer);
    void bindValue(char[] name, char[]* pointer);
    void clonePredef(char[] name, char[] newname);
// //     string[] allFuncs();
    bool isBound(char[] key);
//     void unbind(char[] key);
    void refresh();
    void addGlobal(char[] key, Cell* value);
    void addLocal(char[] key, Cell* value);
    void addGeneric(char[] key);
    bool isGeneric(char[] key);
    bool isLocal(string key);
    bool isGlobal(string key);
    Context context();
    Context globals();
    void pushContext(Context s = null);
    Context popContext();
    Cell* bind(string name, Cell* value);
    Cell* opIndex(string key);
    Cell* opIndexAssign(Cell* value, string key);
}

}

