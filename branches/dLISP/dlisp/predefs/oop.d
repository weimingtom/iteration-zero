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

    Cell* evalCreateObject(DLisp dlisp, Cell* cell)
    {
        if( cell.cdr )
          return newObject("Unnamed object",dlisp.eval(cell.cdr));
        return newObject("Unnamed object");
    }

    Cell* evalGetMethod(DLisp dlisp, Cell* cell)
    {
        Cell* cells[] = evalArgs(dlisp,".y",cell.cdr);
        if( !isObject(cells[0]) )
        {
            if( dlisp.environment.isBound(cells[1].name) )
                return dlisp.environment[cells[1].name];
            return null;
        }

        cell = getAttribute(cells[0],cells[1].name);
        if( cell !is null )
            return cell;
        if( dlisp.environment.isBound(cells[1].name) )
            return dlisp.environment[cells[1].name];
        return null;
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

//     Cell* evalGenCall(DLisp dlisp, Cell* cell) {
//         Cell* cells[] = evalArgs (dlisp, "y", cell.cdr);
//         
//         return dlisp.eval(cell);
//     }
}

public Environment addToEnvironment(Environment environment) {
    environment.bindPredef("get-attr", &evalGetAttr, "(GET-ATTR <object> <sym>); Return attr of <objects>");
    environment.bindPredef("set-attr", &evalSetAttr, "(SET-ATTR <object> <sym> <value>); Set attr of <objects>");
    environment.bindPredef("has-attr", &evalHasAttr, "(HAS-ATTR <object> <sym>); Return whether <objects> has attr");

    environment.bindPredef("create-object", &evalCreateObject, "(CREATE-OBJECT [<superclass>]); Creates a new Object.");
    environment.bindPredef("get-method", &evalGetMethod, "(GET-METHOD <object> <sym>); Get unbound method.");
//     environment.bindPredef("gencall", &evalGenCall,
//         "(GENCALL <func> <object> <args>); Execute generic, function or macro <func> with arguments in <object> <args>.");

    return environment;
}