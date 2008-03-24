/*
  oopextension.d
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
module dlisp.oopextension;

private {
  import std.string;
  import std.stdio;

  import dlisp.concell;
}

public {

    Cell* newObject(char[] name, Cell* superclass = null)
    {
        atomcount++;
        name = toupper(name);
        Cell* cell = new Cell;
        cell.cellType = CellType.ctOBJECT;
        cell.name = name;
        cell.table["PARENT"] = superclass;
        return cell;
    }

    Cell* getParentObject(Cell* obj)
    {
        if( "PARENT" in obj.table )
            return obj.table["PARENT"];
        return null;
    }

    Cell* getAttribute(Cell* obj, string name)
    {
//         writefln( "get-attr: ",name);
//         writefln( "obj: ",obj.table.keys);
        while( obj !is null && obj.cellType == CellType.ctOBJECT )
        {
            if( (name in obj.table) !is null )
                return obj.table[name];
            obj = getParentObject(obj);
        }
        return null;
    }
}
