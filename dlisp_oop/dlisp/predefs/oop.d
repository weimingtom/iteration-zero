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
  import dlisp.oopextension;
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

    Cell* evalCreateObject(DLisp dlisp, Cell* cell)
    {

        Cell* cells[] = evalArgs(dlisp,"'yO?",cell.cdr);
        if( cells.length == 1 )
            cells ~= null;
        cell = newObject(cells[0].name,cells[1]);
        dlisp.environment.addLocal(cell.name,cell);
        return cell;
    }

    Cell* evalGetMethod(DLisp dlisp, Cell* cell)
    {

        Cell* cells[] = evalArgs(dlisp,"Oy",cell.cdr);
        return getAttribute(cells[0],cells[1].name);
    }

    Cell* evalGetAttr(DLisp dlisp, Cell* cell)
    {
        Cell* cells[] = evalArgs(dlisp,"Oy.?",cell.cdr);
        Cell* attr = getAttribute(cells[0],cells[1].name);
        if( attr is null && cells.length == 3 )
        {
            return cells[2];
        }
        return attr;
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
        if( getAttribute(cells[0], cells[1].name) !is null)
            return newSym("T");
        return nil;
    }

}

public Environment addToEnvironment(Environment environment) {
    environment.bindPredef("get-attr", &evalGetAttr, "(GET-ATTR <object> <sym>); Return attr of <objects>");
    environment.bindPredef("set-attr", &evalSetAttr, "(SET-ATTR <object> <sym> <value>); Set attr of <objects>");
    environment.bindPredef("has-attr", &evalHasAttr, "(HAS-ATTR <object> <sym>); Return whether <objects> has attr");

    environment.bindPredef("create-object", &evalCreateObject, "(CREATE-OBJECT <sym> [<superclass>]); Creates a new Object.");
    environment.bindPredef("get-method", &evalGetMethod, "(GET-METHOD <object> <sym>); Get unbound method.");
    return environment;
}