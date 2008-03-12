/*
 states.d
 dLISP
 
 Created by Fredrik Olsson on 2005-12-29.
 Copyright (c) 2005 __MyCompanyName__. All rights reserved.
 */

module dlisp.states;

private {
  import std.string;
  
  import dlisp.types;
}

public {
  
  class DLispState : Exception {
    Pos pos;
    public this(char[] msg) {
      super(msg);
    }
    public this(char[] msg, Pos pos) {
      this.pos = pos;
      this(msg);
    }
    public char[] stateName() {
      return "STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["STATE"];
      return tmp;
    }
  }

  class ErrorState : DLispState {
    this(char[] msg, Pos pos) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "ERROR-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["ERROR-STATE"];
      return super.stateNames() ~ tmp;
    }
  }
  
  class ParseState : ErrorState {
    this(char[] msg, Pos pos) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "PARSE-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["PARSE-STATE"];
      return super.stateNames() ~ tmp;
    }
  }
  
  class EvalState : ErrorState {
    this(char[] msg, Pos pos) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "EVAL-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["EVAL-STATE"];
      return super.stateNames() ~ tmp;
    }
  }
  
  public class UnboundSymbolState : EvalState {
    this(char[] msg, Pos pos) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "UNBOUND-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["UNBOUND-STATE"];
      return super.stateNames() ~ tmp;
    }
  } 
  
  class ArgumentState : EvalState {
    this(char[] msg, Pos pos) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "ARGUMENT-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["ARGUMENT-STATE"];
      return super.stateNames() ~ tmp;
    }
  }
  
  class TypeState : EvalState {
    this(char[] msg, Pos pos) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "TYPE-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["TYPE-STATE"];
      return super.stateNames() ~ tmp;
    }
  }
  
  class FileState : ErrorState {
    this(char[] msg) {
      super(msg, pos);
    }
    public char[] stateName() {
      return "FILE-STATE";
    }
    public char[][] stateNames() {
      static char[][] tmp = ["FILE-STATE"];
      return super.stateNames() ~ tmp;
    }
  }
    
}