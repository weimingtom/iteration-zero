/*
  bind.d
  dLISP

  Author: Klaus Blindert <klaus.blindert@web.de>
  Copyright (c) 2008
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

module dlisp.bind;

public {
  import dlisp.dlisp;
  import std.string;

  import std.boxer;
  import std.traits : ReturnType, ParameterTypeTuple;
}

template FunParams(alias f)
{
  const FunParams = FunParamsImpl!(0, ParameterTypeTuple!(f));
}

template FunParamsImpl(int n, A ... )
{
    static if( A.length == 0 ) {
      const FunParamsImpl = "";
    } else {
      static if( A.length > 1 ) {
        const FunParamsImpl = FunParamsImpl!(n,A[0]) ~ "," ~ FunParamsImpl!(n+1,A[1..$]);
      } else {
        const FunParamsImpl = "unbox!(" ~ A[0].stringof ~ ")(args[" ~ n.stringof ~"])";
      }
    }
}

template HasParams(alias f)
{
    const HasParams = ParameterTypeTuple!(f).length > 0;
}

template MethodName(alias f)
{
  const MethodName = (&f).stringof[1..$];
}

template InvokeMethod(alias func)
{
  static if( !HasParams!(func) ) {
    const InvokeMethod = BindReturnValue!(func) ~ "instance." ~ func.stringof ~ ";";
  } else {
    const InvokeMethod = BindReturnValue!(func) ~ "instance." ~ MethodName!(func) ~ "("~ FunParams!(func) ~");";
  }
}

template HasReturnType(alias func)
{
  const HasReturnType = !is(ReturnType!(func) == void);
}

template BindReturnValue(alias func)
{
  static if( !HasReturnType!(func) )
    const BindReturnValue = "";
  else
    const BindReturnValue = "auto return_value = ";
}

template BoxArguments(T ...)
{
  static if( T.length == 0)
    const BoxArguments = "";
  else static if( T.length == 1 )
    const BoxArguments = "nextarg;" ~ BoxArgument!(T[0]);
  else
    const BoxArguments = "nextarg;" ~ BoxArgument!(T[0]) ~ BoxArguments!(T[1..$]);
}

template AddArgument(string argvalue)
{ 
    const AddArgument ="args~=" ~ argvalue ~ ";";
}

template CheckArgument(string checkfun)
{
    const CheckArgument = 
      "if( !" ~ checkfun ~ "(arg_car) ) { "
        ~ "throw new ArgumentState(\"Excepted " ~ checkfun ~ "\",cell.pos);"
        ~ "}";
}

template BoxArgument(T : int)
{
  const BoxArgument = CheckArgument!("isInt") ~ AddArgument!("box(arg_car.intValue)");
}

template BoxArgument(T : long)
{
  const BoxArgument = CheckArgument!("isInt") ~ AddArgument!("box(arg_car.intValue)");
}

template BoxArgument(T : string)
{
  const BoxArgument = CheckArgument!("isString") ~ AddArgument!("box(arg_car.strValue)");
}

template BoxArgument(T)
{
  pragma(msg,"DON'T KNOW HOW TO AUTOMATICALLY BIND THIS: " ~ T.stringof);
  static assert(0);
}

template BoxReturnValue(T : int)
{
  const BoxReturnValue = "return newInt(return_value);";
}

template BoxReturnValue(T : long)
{
  const BoxReturnValue = "return newInt(return_value);";
}

template BoxReturnValue(T : bool)
{
  const BoxReturnValue = "return newBool(return_value);";
}

template BoxReturnValue(T : string)
{
  const BoxReturnValue = "return newStr(return_value);";
}

template BoxReturnValue(T)
{
  pragma(msg,"DON'T KNOW HOW TO AUTOMATICALLY WRAP RETURN TYPE: " ~ T.stringof);
  static assert(0);
}

template BoundClass(string classname)
{
  private Cell* _instanceCell;

  private static Cell* _classCell;
  private static Cell*[string] _methods;

  static void bindClass(Environment environment)
  {
      environment[classname] = getClass();
  }

  static typeof(this) createInstance(Cell* cell)
  {
    typeof(this) instance = new typeof(this);
    instance._instanceCell = cell;
    return instance;
  }

  static typeof(this) getInstance(Cell* cell)
  {
    return unbox!(typeof(this))(cell.instance);
  }

  static Cell* getClass()
  {
    static Cell* makeInstance(DLisp dlisp, Cell*)
    {
      Cell* object = newObject("INSTANCE OF CLASS " ~ toupper(classname),getClass());
      object.instance = box(createInstance(object));
      return object;
    }

    if( _classCell is null )
    {
      _classCell = newObject(classname);
      _classCell.table["MAKE-INSTANCE"] = newPredef("MAKE-INSTANCE",toDelegate(&makeInstance),"CREATE AN INSTANCE OF " ~ toupper(classname));
      foreach(string name, Cell* method; _methods)
      {
        _classCell.table[name] = method;
      }
    }
    return _classCell;
  }
}

template BoundMethod(string name,alias func)
{
//     pragma(msg,name);
    static this()
    {
      static Cell* methodWrapper(DLisp dlisp, Cell* cell)
      {
        Cell* objectCell = cell.cdr.car;
        auto instance = getInstance(dlisp.eval(objectCell));

//         writefln ("METHOD: %s SELF: %s ARGS: %s",name, instance, cellToString(cell.cdr.cdr));

        if(ParameterTypeTuple!(func).length > listLen(cell.cdr.cdr))
        {
          throw new ArgumentState("Function " ~ name ~ " got too few arguments.",cell.pos);
        }

        if(ParameterTypeTuple!(func).length < listLen(cell.cdr.cdr))
        {
          throw new ArgumentState("Function " ~ name ~ " got too many arguments.",cell.pos);
        }

        Cell* arg_cons = cell.cdr.cdr;
        Cell* arg_car = null;

        void nextarg()
        {
          assert(arg_cons);
          arg_car = dlisp.eval(arg_cons.car);
          arg_cons = arg_cons.cdr;
        }

        Box[] args;
        mixin(BoxArguments!(ParameterTypeTuple!(func)));
        mixin(InvokeMethod!(func));

        static if( HasReturnType!(func) ){
          mixin(BoxReturnValue!(ReturnType!(func)));
        }
        return null;
      }

      _methods[name] = newPredef(name,toDelegate(&methodWrapper),"Autogenerated unbound function.");
    }
}

string _lispify(string name)
{
  string new_name;
  for(int i= 1; i != name.length; ++i)
  {
    if( toupper(name[i..i+1]) == name[i..i+1] )
      new_name ~= '-';
    new_name ~= name[i];
  }
  return toupper(new_name);
}

template BoundMethod(alias func)
{
  mixin BoundMethod!(_lispify(MethodName!(func)),func);
}
