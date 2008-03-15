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
  import dlisp.evalhelpers : evalArgs;
  import std.string;
  import std.stdio : writefln;

  import std.boxer;
  import std.traits : ReturnType, ParameterTypeTuple;
}

T Construct(T,A...)(A a) {
  return new T(a);
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

template InvokeConstructor(string classname,alias func)
{
  const InvokeConstructor = classname ~ " instance = new " ~ classname ~ "("~ FunParams!(func) ~");";
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
  const BoxArguments = BoxArgumentsImpl!(0,T);
}

template BoxArgumentsImpl(int n, T ...)
{
  static if( T.length == 0)
    const BoxArgumentsImpl = "";
  else static if( T.length == 1 )
    const BoxArgumentsImpl = BoxArgument!(n,T[0]);
  else
    const BoxArgumentsImpl = BoxArgument!(n,T[0]) ~ BoxArgumentsImpl!(n+1,T[1..$]);
}

template CArg(int n)
{
  const CArg = "cargs[" ~ n.stringof ~ "]";
}

template AddArgument(string argvalue)
{ 
    const AddArgument = "args ~=" ~ argvalue ~ ";";
}

template CheckArgument(int n, string checkfun)
{
    const CheckArgument = 
      "if( !" ~ checkfun ~ "("~ CArg!(n) ~") ) { "
        ~ "throw new ArgumentState(\"Excepted " ~ checkfun ~ "\",cell.pos);"
        ~ "}";
}

template BoxArgument(int n, T : int)
{
  const BoxArgument = CheckArgument!(n,"isInt") ~ AddArgument!("box(" ~ CArg!(n) ~ ".intValue)");
}

template BoxArgument(int n, T : long)
{
  const BoxArgument = CheckArgument!(n,"isInt") ~ AddArgument!("box(" ~ CArg!(n) ~ ".intValue)");
}

template BoxArgument(int n, T : string)
{
  const BoxArgument = CheckArgument!(n,"isString") ~ AddArgument!("box(" ~ CArg!(n) ~ ".strValue)");
}

template BoxArgument(int n, T)
{
  static if( IsBoundClass!(T) ) {
    const BoxArgument =  CheckArgument!(n,T.stringof ~ ".isInstance") ~ AddArgument!(CArg!(n) ~ ".instance");
  } else {
    pragma(msg,"DON'T KNOW HOW TO AUTOMATICALLY BIND THIS: " ~ T.stringof);
    static assert(0);
  }
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
  static if ( IsBoundClass!(T) ) {
    const BoxReturnValue = "return return_value.wrap(dlisp);"; 
  } else {
    pragma(msg,"DON'T KNOW HOW TO AUTOMATICALLY WRAP RETURN TYPE: " ~ T.stringof);
    static assert(0);
  }
}

template BindClass(string classname)
{
  private Cell* _instanceCell;

  private static Cell* _classCell;
  private static Cell*[string] _methods;
  private static typeof(this) function(DLisp,Cell*) _constructor;

  static void bindClass(Environment environment)
  {
      environment[classname] = getClass();
  }

  static bool isInstance(Cell* cell)
  {
    if( cell is null )
      return false;
    return cell.instance.unboxable(typeid(typeof(this)));
  }

  static Cell* wrapInstance(DLisp dlisp, typeof(this) instance)
  {
    if( instance._instanceCell )
      return instance._instanceCell;
    Cell* object = newObject("INSTANCE OF CLASS " ~ toupper(classname),getClass());
    object.instance = box(instance);
    return object;
  }

  Cell* wrap(DLisp dlisp)
  {
    return typeof(this).wrapInstance(dlisp,this);
  }

  static typeof(this) createInstance(DLisp dlisp,Cell* object, Cell* cell)
  {
    typeof(this) instance;

    writefln("CONSTRUCTOR ARGS: %s",cellToString(cell.cdr));
    if( cell.cdr is null )
    {
      static if (is(typeof( Construct!(typeof(this))))) {
        instance = Construct!(typeof(this));
      } else {
        throw new ArgumentState("No default constructor given for " ~ classname,cell.pos);
      }
    } else {
      instance = _constructor(dlisp, cell);
    }

    instance._instanceCell = object;
    return instance;
  }

  static typeof(this) getInstance(Cell* cell)
  {
    return unbox!(typeof(this))(cell.instance);
  }

  static Cell* getClass()
  {
    static Cell* makeInstance(DLisp dlisp, Cell* cell)
    {
      Cell* object = newObject("INSTANCE OF CLASS " ~ toupper(classname),getClass());
      object.instance = box(createInstance(dlisp, object, cell));
      return object;
    }

    if( _classCell is null )
    {
      _classCell = newObject(classname);
      _classCell.table["MAKE-INSTANCE"] = newPredef("MAKE-INSTANCE",toDelegate(&makeInstance),"CREATES AN INSTANCE OF " ~ toupper(classname));
      foreach(string name, Cell* method; _methods)
      {
        _classCell.table[name] = method;
      }
    }
    return _classCell;
  }
}

template IsBoundClass(T)
{
  const IsBoundClass = is( typeof( T.bindClass ) );
}

unittest {
  //
  // Internal compiler error with GDC :(
  //

  class UTest1 {
//     mixin BindClass!("UTest");
  }
  class UTest2 { }

  // static assert(  IsBoundClass!(UTest1) );
  static assert( !IsBoundClass!(UTest2) );
}

// Inclomplete Constructor Wrapper
template BindConstructor(T)
{
    static this()
    {
      

      static typeof(this) initWrapper(DLisp dlisp, Cell* cell)
      {
        T func;
//         pragma(msg,T.stringof);
        
        writefln ("CONSTRUCTOR: %s ARGS: %s", T.stringof, cellToString(cell.cdr.cdr));

        if(ParameterTypeTuple!(func).length > listLen(cell.cdr.cdr))
        {
          return null;
        }

        if(ParameterTypeTuple!(func).length < listLen(cell.cdr.cdr))
        {
          return null;
        }

        Cell*[] cargs = evalArgs(dlisp,cell.cdr.cdr);

        Box[] args;
        try {
          mixin(BoxArguments!(ParameterTypeTuple!(func)));
        } catch (ErrorState e) {
          return null;
        }
        mixin(InvokeConstructor!("Test",func));
        return instance;
      }

      _constructor = &initWrapper;
    }
}

template BindMethod(string name,alias func)
{
//     pragma(msg,name);
    static this()
    {
      static Cell* methodWrapper(DLisp dlisp, Cell* cell)
      {
        Cell* objectCell = cell.cdr.car;
        auto instance = getInstance(dlisp.eval(objectCell));

//         writefln ("METHOD: %s SELF: %s ARGS: %s",name, instance, cellToString(cell.cdr.cdr));

        Cell*[] cargs = evalArgs(dlisp,cell.cdr.cdr);

        if(ParameterTypeTuple!(func).length > cargs.length)
        {
          throw new ArgumentState("Function " ~ name ~ " got too few arguments.",cell.pos);
        }

        if(ParameterTypeTuple!(func).length < cargs.length)
        {
          throw new ArgumentState("Function " ~ name ~ " got too many arguments.",cell.pos);
        }

        Box[] args;
        mixin(BoxArguments!(ParameterTypeTuple!(func)));
        mixin(InvokeMethod!(func));

        static if( HasReturnType!(func) ){
          mixin(BoxReturnValue!(ReturnType!(func)));
        }
        return null;
      }

      _methods[name] = newPredef(name,toDelegate(&methodWrapper),"auto-generated unbound method.");
    }
}

string Lispify(string name)
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

template BindMethod(alias func)
{
  mixin BindMethod!(Lispify(MethodName!(func)),func);
}

template BindMethods(T ...)
{
  static if (T.length == 1) {
    mixin BindMethod!(T[0]);
  } else {
    mixin BindMethod!(T[0]);
    mixin BindMethods!(T[1..$]);
  }
}
