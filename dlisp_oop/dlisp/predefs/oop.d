/*
  oop.d
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
module dlisp.predefs.oop;

private {
  import std.math;
  import std.string;
  import std.stdio;

  import dlisp.evalhelpers;
  import dlisp.dlisp;
}

private {
    Cell* typeType = null;
    Cell* classType = null;
    Cell* instanceType = null;
}

public {
    struct BoundClass
    {
        char[] name;
        char[] superclass;
    
        static BoundClass opApply(char[] name,char[] superclass="*object-class*")
        {
            BoundClass cls;
            cls.name = name;
            cls.superclass = superclass;
            return cls;
        }
    
        void addToEnvironment(Environment environment)
        {
            
        }
    }

    Cell* newClass(char[] name, Cell* superclass = null)
    {
        Cell* cell = newObject(name,classType);
        if( superclass )
        {
            writefln ("SUPERCLASS of %s is %s",cellToString(cell),cellToString(superclass));
            cell.table["SUPERCLASS"] = superclass;
            assert(cell.table["SUPERCLASS"].table["*TYPE*"] == classType);
        }
        return cell;
    }

    Cell* evalTypeOf(DLisp dlisp, Cell* cell)
    {
        Cell* cells[] = evalArgs(dlisp,"O",cell.cdr);
        if( isObject(cells[0]) )
        {
            return cells[0].table["*TYPE*"];
        }
        return nil;
    }

    Cell* evalDefClass(DLisp dlisp, Cell* cell)
    {

        Cell* cells[] = evalArgs(dlisp,"'yO?",cell.cdr);
        if( cells.length == 1 )
            cells ~= null;
        cell = newClass(cells[0].name,cells[1]);
        dlisp.environment.addLocal(cell.name,cell);
        return cell;
    }

    Cell* evalCreateInstance(DLisp dlisp, Cell* cell)
    {

        Cell* cells[] = evalArgs(dlisp,"'yO?",cell.cdr);
        if( cells.length == 1 )
            cells ~= null;
        cell = newClass(cells[0].name,cells[1]);
        dlisp.environment.addLocal(cell.name,cell);
        return cell;
    }

    Cell* evalGetAttr(DLisp dlisp, Cell* cell)
    {
        Cell* cells[] = evalArgs(dlisp,"Oy.?",cell.cdr);
        if( cells[1].name in cells[0].table )
        {
            return cells[0].table[cells[1].name];
        }
        if( cells.length == 3 )
        {
            return cells[2];
        }
        return nil;
    }

    Cell* evalSetAttr(DLisp dlisp, Cell* cell)
    {
        Cell* cells[] = evalArgs (dlisp, "Oy.", cell.cdr);
        cells[0].table[cells[1].name] = cells[2];
        return cells[2];
    }

    Cell* evalHasAttr(DLisp dlisp, Cell* cell)
    {
        Cell* cells[] = evalArgs (dlisp, "Oy", cell.cdr);
        if((cells[1].name in cells[0].table) !is null)
            return newSym("T");
        return nil;
    }

}

public Environment addToEnvironment(Environment environment) {
    environment["*TYPE*"] = typeType = newObject("*TYPE*", null);
    environment["*CLASS-TYPE*"] = classType = newObject("*CLASS-TYPE*", environment["*TYPE*"]);
    environment["*INSTANCE-TYPE*"] = classType = newObject("*INSTANCE-TYPE*", environment["*TYPE*"]);

    environment.bindPredef("type-of", &evalTypeOf, "(TYPE-OF <object>); Return <objects>s type");
    environment.bindPredef("get-attr", &evalGetAttr, "(GET-ATTR <object> attr); Return attr of <objects>");
    environment.bindPredef("set-attr", &evalSetAttr, "(SET-ATTR <object> attr value); Set attr of <objects>");
    environment.bindPredef("has-attr", &evalHasAttr, "(HAS-ATTR <object> attr); Return whether <objects> has attr");

    environment.bindPredef("defclass", &evalDefClass, "(DEFCLASS <sym> [<superclass>]); Define a new class.");
    return environment;
}