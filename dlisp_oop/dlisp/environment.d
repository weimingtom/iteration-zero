/*
 environment.d
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

module dlisp.environment;

private {
  import std.math;
  import std.string;
  import std.cstream;
  import std.stdio;
  import dlisp.dlisp;
}

PredefFunc toDelegate(PredefFuncPtr fn)
{ 
    struct holder {
        PredefFuncPtr f;
        Cell* call(DLisp dlisp, Cell* cell)
        {
            return f(dlisp,cell);
        }
    }
    auto res = new holder;
    res.f = fn;
    return &res.call;
}

public class Environment {
  private Cell*[char[]] _globals;
  private Cell*[char[]][] _restore;
  private char[][][] _remove;
  
  public {

    this() {
    }
    
    void bindDelegate(char[] name, PredefFunc func, char[] docs = "", bool ismacro = false) {
      this[name] = newPredef(name, func, docs, ismacro);
    }

    void bindPredef(char[] name, PredefFuncPtr funcPtr, char[] docs = "", bool ismacro = false) {
//       auto func_dg = Cell* delegate(DLisp dlisp, Cell* cell) { return funcPtr(dlisp, cell); };
      this[name] = newPredef(name, toDelegate(funcPtr), docs, ismacro);
    }
    
    void bindValue(char[] name, int* pointer) {
      this[name] = newBoundInt(pointer);
    }

    void bindValue(char[] name, real* pointer) {
      this[name] = newBoundFloat(pointer);
    }
    
    void bindValue(char[] name, char[]* pointer) {
      this[name] = newBoundStr(pointer);
    }
    
    void clonePredef(char[] name, char[] newname) {
      this[newname] = this[name.toupper()];
    }
  
    char[][] allFuncs() {
      char[][] ret;
      foreach (char[] key, Cell* cell; _globals) {
        if (isFunc(cell)) {
          if (cell.docs != "") {
            ret ~= cell.name;
          }
        }
      }
      return ret;
    }
        
    bool isBound(char[] key) {
      return (key in _globals) != null;
    }
    
    void unbind(char[] key) {
      if (!isBound(key)) {
        throw new Exception("Unbound symbol: " ~ key);
      }
      _globals.remove(key);
    }
    
    void refresh() {
      this._globals.rehash;
    }
    
    void addLocal(char[] key, Cell* value) {  
      Cell** tcell = key in _globals;
      if (tcell) {
        _restore[_restore.length - 1][key] = *tcell;
      } else {
        _remove[_remove.length - 1] ~= key;
      }
      _globals[key] = value;
    }
    
    void pushScope() {
//       writefln ("PUSH-SCOPE _restore.length=%d  _remove.length=%d", _restore.length,  _remove.length);
       Cell*[char[]] append_restore;
      _restore ~= append_restore;
      _remove.length = _remove.length + 1;
    }
    
    void popScope() {
      if (_restore.length == 0) {
        throw new Exception("Local stack is empty");
      }
      foreach(char[] key, Cell* cell; _restore[_restore.length - 1]) {
        _globals[key] = cell;
      }
      foreach(char[] key; _remove[_remove.length - 1]) {
        _globals.remove(key);
      }
      _restore.length = _restore.length - 1;
      _remove.length = _remove.length - 1;  
    }
    
    Cell* opIndex(char[] key) {
      if (!isBound(key)) {
        throw new Exception("Unbound symbol: " ~ key);
      }
      return _globals[key];
    }

    Cell* opIndexAssign(Cell* value, char[] key) {
      _globals[key.toupper()] = value;
      return value;
    }

  }
    
}
