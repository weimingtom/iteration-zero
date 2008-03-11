module Sofu.Save;

private {
    import std.stdio;
    import std.stream;
    import std.system;
    import std.utf;
    import Sofu.Map;
}


/** Saves map to the newly created (or overwritten) file specified by fileName,
    using the encoding specified in bom. You can use any of the following
    constants defined in std.stream:
    enum BOM {
        UTF8,		/// UTF-8
        UTF16LE,	/// UTF-16 Little Endian
        UTF16BE,	/// UTF-16 Big Endian
        UTF32LE,	/// UTF-32 Little Endian
        UTF32BE,	/// UTF-32 Big Endian
    }
*/

void saveFile(Map map, char[] fileName, BOM bom = BOM.UTF8)
{
    char[] outputString = map.outputString();
    //writefln("%d", outputString.length);
    
    Endian endian;
    switch(bom) {
        case BOM.UTF8: endian = std.system.endian; break;
        case BOM.UTF16LE: case BOM.UTF32LE: endian = Endian.LittleEndian; break;
        case BOM.UTF16BE: case BOM.UTF32BE: endian = Endian.BigEndian; break;
    }
    
    File realFile = new File(fileName, FileMode.OutNew);
    BufferedStream bufferedFile = new BufferedStream(realFile);
    EndianStream file = new EndianStream(bufferedFile, endian);
    
    switch(bom) {
        case BOM.UTF8:
        {
            // Need exlicit cast to void* ??? kb
            file.writeExact(cast(void*)outputString, outputString.length);
            break;
        }
        case BOM.UTF16LE:
        case BOM.UTF16BE:
        {
            debug(SofuLex) {
                if(bom == BOM.UTF16LE)
                    writefln("UTF16LE");
                else
                    writefln("UTF16BE");
            }
            file.writeBOM(bom);
            wchar[] wstr = toUTF16(outputString);
            foreach(wchar ch; wstr) {
                wchar ch2 = ch;
                with(file) {
                    fixBO(&ch,ch.sizeof); writeExact(&ch, ch.sizeof);
                }
                debug(SofuLex) {
                    if(ch2 != ch) {
                        writefln("Changed byte order");
                    }
                    else {
                        writefln("Didn't change byte order");
                    }
                }
            }
            break;
        }
        case BOM.UTF32LE:
        case BOM.UTF32BE:
        {
            file.writeBOM(bom);
            dchar[] dstr = toUTF32(outputString);
            foreach(dchar ch; dstr) {
                with(file) {
                    fixBO(&ch,ch.sizeof); writeExact(&ch, ch.sizeof);
                }
            }
            break;
        }
    }
    bufferedFile.close();
	realFile.close();
}
