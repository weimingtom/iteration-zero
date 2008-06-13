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
//  import std.math;
  import std.string;
  import std.stdio;
  import dlisp.types;
  import dlisp.concell;
}

PredefFunc toDelegate(PredefFuncPtr fn)
{ 
    struct holder {
        PredefFuncPtr f;
        Cell* call(IDLisp dlisp, Cell* cell)
        {
            return f(dlisp,cell);
        }
    }
    auto res = new holder;
    res.f = fn;
    return &res.call;
}

public class Environment : public IEnvironment {
     private bool[char[]] _generics;
     private Context[] _stack;
  
    this() {
       _stack ~= new Context;
    }
    
    void bindDelegate(char[] name, PredefFunc func, char[] docs = "", bool ismacro = false) {
      this[name] = newPredef(name, func, docs, ismacro);
    }

    void bindPredef(char[] name, PredefFuncPtr funcPtr, char[] docs = "", bool ismacro = false) {
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
  
// //     char[][] allFuncs() {
// //       char[][] ret;
// //       foreach (char[] key, Cell* cell; _globals) {
// //         if (isFunc(cell)) {
// //           if (cell.docs != "") {
// //             ret ~= cell.name;
// //           }
// //         }
// //       }
// //       return ret;
// //     }
    
    bool isBound(char[] key) {
        return context.isBound(key);
    }
    
//     void unbind(char[] key) {
//       if (!isBound(key)) {
//         throw new Exception("Unbound symbol: " ~ key);
//       }
//       // FIXME
//       //_globals.remove(key);
//     }
    
    void refresh() {
      globals.refresh;
      context.refresh;
    }
    
    void addGlobal(char[] key, Cell* value) {
        globals.bind(key,value);  
    }

    void addLocal(char[] key, Cell* value) {
        context.bind(key,value);  
    }

    void addGeneric(char[] key)
    {
        _generics[key] = true;
    }

    bool isGeneric(char[] key)
    {
        return (key in _generics) !is null;
    }

    bool isLocal(string key)
    {
       return !isGlobal(key) && context.isBound(key);
    }

    bool isGlobal(string key)
    {
        return globals.isBound(key);
    }

    Context context()
    {
	return _stack[$-1];
    }

    Context globals()
    {
        return _stack[0];
    }

    void pushContext(Context s = null) {
       version(debugContext)
	  { scope(exit) writefln("push-scope:#",_stack.length," >>",context.allKeys); }

        if( s !is null )
        {
            _stack ~= s;
            return;
        }

        if( _stack.length == 1 )
        {
            _stack ~= new Context;
            context.master = globals;
        } else
        {
            _stack ~= context;
        }
    }
    
    Context popContext() {
       assert( _stack.length > 1 );
       version(debugContext)
          { scope(exit) writefln("pop-scope:#",_stack.length + 1); }

       Context top = context;
       _stack.length = _stack.length - 1;
       return context;
    }

    Cell* bind(string name, Cell* value)
    {
       return context.bind(name,value);
    }
    
    Cell* opIndex(string key) {
       if( !isBound(key) ) {
	  throw new Exception("Unbound symbol: " ~ key);
       }
       return context[key];
    }

    Cell* opIndexAssign(Cell* value, string key) {
       return context.bind(key.toupper(),value);
    }
}

unittest {
   Environment env = new Environment;

   Context ctxt = env.context;
   ctxt.bind("A",null);
   assert( env.isBound("A") );

   env.pushContext;
   ctxt = env.context;
   ctxt.bind("B",null);
   assert( env.isBound("A") );
   assert( env.isBound("B") );
}
