/** wrapres.d
 *
 * A program to wrap up binary or string resources into D byte[] or 
 * character arrays. 
 *
 * Also includes the ability to read images and decode them into bytes.
 * Image formats are limited to those supported by DevIL 
 */
/*
  Copyright (C) 2006 William V. Baxter III

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  William Baxter wbaxter@gmail.com
*/
import std.file;
import std.cstream;
import std.stream;
import std.ctype;
static import std.conv;

static import std.string;
alias std.string.replace replace;
alias std.string.rfind rfind;
alias std.string.toStringz toStringz;
alias std.string.format format;

// This is a workaround for http://d.puremagic.com/issues/show_bug.cgi?id=590
// But it could be useful for allowing non-native endianness as well
static char[] NL = "\n";

import std.c.stdlib : exit,getenv;


import derelict.devil.il;

import std.stdio : writefln, writef;

void show_usage()
{
    char[] msg = 
        r"wrapres v1.0 - a program to wrap resources into D arrays
Usage:
    wrapres [flags] resfile [flags] resfile ...

Flags can be:
-h,-help:  Display this help message

-b,-binary:
           Use binary mode output.  The output is a ubyte[].
           [default]
-t,-text:
           Use text mode output.  The output is a char[] instead of a
           ubyte[].
-tw:
           Text mode output with 'wysiwyg' strings.  Like -t, but a
           wsyiwyg r-string is used.
-o,-out,-output [filename]:
           Write output to filename instead of stdout. Use '-' to
           specify stdout.
-n,-name [identifier]:
           Name to use for the variable in the output.  If not
           specified the name is generated from the filename with
           non-identifier characters replaced by '_'.  If the name
           contains a number format like %%03d, then this will be used
           to create a numbered sequence of variables.
-x,-hex:   Hex output for binary mode [the default]
-X,-HEX:   Uppercase hex output for binary mode
-oct:      Octal output for binary mode
-d,-dec:   Decimal output for binary mode
-w,-width [number]: 
           Width used in formatting output numbers
-i,-indent [number]:
           Number of spaces of indentation to use in formatting code.
           [default=4]
-wrap [number]:
           Column at which to wrap output. If 0 then no wrapping.
           [default=78]
-stripext: If no -name is specifed, then this drops the filename
           extension from the generated variable name.
           I.e. animage.png becomes 'animage'.
-keepext: 
           If no -name is specifed, then this keeps the filename
           extension in the generated variable name.  I.e. animage.png
           becomes 'animage_png'.  [default]
-image: 
           Treat the resfile as an image, decode it and write the
           result as a 2D array of ubyte[].  This functionality
           requires the presence of the DevIL image library, which
           can be obtained from
                 http://openil.sourceforge.net/download.php
-imagewh: 
           Treat the resfile as an image, decode it and write the
           result as an array of ubyte[].  The first two bytes
           represent the width and height of the image.  Because these
           values are limited to 0<x<255, this method cannot be used
           for images bigger than 255x255.  This functionality
           requires the presence of the DevIL image libary, which
           can be obtained from
                  http://openil.sourceforge.net/download.php
-raw,-noimage: 
           The opposite of -image/imagewh.  Do no special decoding on the
           input data [default]
-m,-module [modulename]:
           Put the module statement 'module modulename;' at the top
           of the output.
@[filename]
           Read additional commands from 'filename'. These are treated
           exactly as if they had been typed at the same location in the
           commandline.  The file can contain // or # style comments
           and can have multiple lines.  They can also contain environment
           variables using $FOO, $(FOO), or ${FOO} syntax.

Flags are sticky, that is they apply to all subsequent files, unless
overridden by a subsequent command.  For instance the -image flag
will hold until a -raw flag appears to counteract it.
";

    writefln(msg);
    exit(0);
}

struct WriterParams
{
    char[] varname = "";
    int linelen = 78;
    int indent = 4;
    int formatwidth = 3;
    char[] formatchar = "x";
    bool textmode = false;
    bool textwysiwyg = false;
    bool stripext = false;
    bool decode_image = false;
    bool image2d = true;
    char[] modulename = "";
}

class Writer
{
    WriterParams params;

    ubyte[] readAll(InputStream inf) 
    {
        ubyte[] ret; 
        // prealloc trick
        ret.length = inf.available;
        ret.length = 0;
        
        const uint rdbuflen = 4096;
        ubyte[] scratch = new ubyte[rdbuflen];
        while (true) {
            uint nread = inf.read(scratch);
            ret ~= scratch[0..nread];
            if (rdbuflen != nread) 
                break;
        }
        return ret;
    }
    
    char[] makeDecimalFormatStr()
    {
        char [] fmt;
        fmt ~= "%";
        if (params.formatwidth) {
            fmt ~= " ";
            fmt ~= std.string.toString(params.formatwidth);
        }
        fmt ~= params.formatchar;
        fmt ~= ", ";
        return fmt;
    }
    char[] makeHexFormatStr()
    {
        char [] fmt;
        fmt ~= "%#";
        if (params.formatwidth) {
            fmt ~= "0";
            fmt ~= std.string.toString(params.formatwidth+1);
        }
        fmt ~= params.formatchar;
        fmt ~= ", ";
        return fmt;
    }

    char[] makeOctalFormatStr()
    {
        char [] fmt;
        fmt ~= "%#";
        if (params.formatwidth) {
            fmt ~= "0";
            fmt ~= std.string.toString(params.formatwidth+1);
        }
        fmt ~= params.formatchar;
        fmt ~= ", ";
        return fmt;
    }
    char[] makeFormatStr()
    {
        switch (params.formatchar)
        {
        case "d":
        case "s":  
            return makeDecimalFormatStr();
        case "x": 
        case "X": 
            return makeHexFormatStr();
        case "o":  
        case "O": 
            return makeOctalFormatStr();
        default:
            throw new Exception("Bad format string");
        }
    }

    void output(InputStream inf, OutputStream outf)
    {
        if (params.textmode) {
            if (params.textwysiwyg) {
                outputTextWSYIWYG(inf,outf);
            } else {
                outputTextEscaped(inf,outf);
            }
        }
        else {
            outputBinary(inf,outf);
        }
        outf.writef(NL);
    }

    void outputTextWSYIWYG(InputStream inf, OutputStream outf) 
    {
        char[] tab = std.string.repeat(" ", params.indent);

        char[] outstr; //outstr.length = inf.available; outstr.length = 0;
        int outlen = 0;
        foreach (char[] line; inf)
        {
            // the newline is stripped, so +1
            outlen += line.length+1;

            // Wysywig strings are great, but there's no freaking way to put a quote
            // inside of one.  I tried several options, but none of them 
            // looks particularly clean.
            // But overall wsywig strings are nice (no \t's or \n's etc)
            
            //outstr ~= line.replace("\"", "\"\"\\\"\"r\"");
            //outstr ~= line.replace("\"", "\"~'\"'~\"r");
            outstr ~= line.replace("\"", "\"\\\"r\"");
            outstr ~= NL;
        }
        outf.writef("static const char[%s] %s = ",outlen, params.varname);
        outf.writef("  "~NL~"r\"");
        outf.writeString(outstr);
        outf.writeString("\";"~NL~"");
    }

    void outputTextEscaped(InputStream inf, OutputStream outf) 
    {
        char[] tab = std.string.repeat(" ", params.indent);

        char[] outstr; //outstr.length = inf.available; outstr.length = 0;
        int outlen = 0;
        foreach (char[] line; inf)
        {
            // the newline is stripped, so +1
            outlen += line.length+1;

            line = line.replace(`\`, `\\`);
            line = line.replace("\"", "\\\"");
            outstr ~= tab;
            outstr ~= "\"";
            outstr ~= line;
            outstr ~= "\\n\""~NL;
        }
        outf.writef("static const char[%s] %s = "~NL, outlen, params.varname);
        outstr[$-NL.length] = ';';
        outf.writeString(outstr);
        outf.writeString(NL);
    }

    void outputBinaryData(ubyte[] data, OutputStream outf)
    {
        char [] fmt = makeFormatStr();

        char[] tab = std.string.repeat(" ", params.indent);

        outf.writef("static const ubyte[%s] %s = ["~NL, data.length, params.varname);
        uint llen = 0;
        outf.writef(tab);
        llen = tab.length;
        foreach(b; data) {
            char[] v = format(fmt, cast(uint)b);
            llen += v.length;
            if (params.linelen > 0 && llen > params.linelen) {
                outf.writef(NL, tab);
                llen = tab.length + v.length;
            }
            outf.writef(v);
        }
        outf.writef(NL~"];"~NL);
    }

    void outputBinaryData2D(ubyte[] data, int width, int height, OutputStream outf)
    {
        char [] fmt = makeFormatStr();

        char[] tab = std.string.repeat(" ", params.indent);

        int rowlen = data.length/height;
        outf.writef("static const ubyte %s[%s][%s] = ["~NL, params.varname,height,rowlen);
        uint llen = 0;
        for(int j=0; j<height; j++)
        {
            outf.writef(tab,"[");
            llen = tab.length+1;
            for(int i=0; i<rowlen; i++)
            {
                ubyte b = data[j*rowlen + i];
                char[] v = format(fmt, cast(uint)b);
                llen += v.length;
                if (params.linelen>0 && llen > params.linelen) {
                    outf.writef(NL, tab, " ");
                    llen = tab.length + v.length + 1;
                }
                outf.writef(v);
            }
            outf.writef("],"~NL);
        }
        outf.writef(NL~"];"~NL);
    }

    void outputBinary(InputStream inf, OutputStream outf) 

    {
        ubyte[] data = readAll(inf);
        outputBinaryData(data,outf);
    }

    void outputImage(char[] fname, OutputStream outf) 
    {
        ILuint id; 
        ilGenImages(1, &id);
        ilBindImage(id);
        if (!ilLoadImage(std.string.toStringz(fname))) {
            report_fatal_error(format("Unable to open image file '%s'.", fname));
        }

        int w = ilGetInteger(IL_IMAGE_WIDTH);
        int h = ilGetInteger(IL_IMAGE_HEIGHT);
        int ifmt = ilGetInteger(IL_IMAGE_FORMAT);
        int myfmt;
        int chan;
        switch(ifmt) {
        case IL_COLOUR_INDEX:
            chan = 4;
            myfmt = IL_RGBA;
            break;
        case IL_RGB:
            chan = 3;
            myfmt = IL_RGB;
            break;
        case IL_RGBA:
            chan = 4;
            myfmt = IL_RGBA;
            break;
        case IL_BGR:
            chan = 3;
            myfmt = IL_RGB;
            break;
        case IL_BGRA:
            chan = 4;
            myfmt = IL_RGBA;
            break;
        case IL_LUMINANCE:
            chan = 1;
            myfmt = IL_LUMINANCE;
            break;
        case IL_LUMINANCE_ALPHA:
            chan = 2;
            myfmt = IL_LUMINANCE_ALPHA;
            break;
        default:
            report_fatal_error(format("Unknown format for image file '%s'.", fname));
        }
        
        auto data = new ubyte[w*h*chan];
        ilCopyPixels(0,0,0,
                     w, h, 1,
                     myfmt, IL_UNSIGNED_BYTE,
                     &data[0]);
        ILenum err = ilGetError();
        if (err != IL_NO_ERROR) {
            report_fatal_error(format("Unable to get data from image file '%s'.", fname));
        }
        
        if (!params.image2d) {
            if (w>ubyte.max || h>ubyte.max) {
                report_fatal_error(format("Image %s too big for w,h header mode", fname));
            }
            data = [cast(ubyte)w,cast(ubyte)h] ~ data;
            outputBinaryData(data, outf);
        }
        else {
            outputBinaryData2D(data, w, h, outf);
        }

        ilDeleteImages(1, &id);
    }
}

char[] filename_to_argname(char[] fname, bool stripext)
{
    // replace anything not in _a-zA-Z with __
    // Ignore final .foo extension
    char[] varname; varname.length = fname.length; varname.length = 0;
    int end = -1;
    if (stripext) {
        end = fname.rfind('.');
    }
    if (end<0) end = fname.length;
    foreach(dchar c; fname[0..end]) {
        if ( (c>='a' && c<='z') ||
             (c>='A' && c<='Z') )
        {
            varname ~= c;
        }
        else 
        {
            varname ~= "_";
        }
    }
    return varname;
}


void report_fatal_error(char [] msg)
{
    derr.writefln(msg);
    exit(-1);
}



bool initImageLib()
{
    try {
        DerelictIL.load();
        ilInit();
    }
    catch (Exception e) {
        return false;
    }
    return true;
}

char[][] fileglob(char[] pattern)
{
    char[][] list = listdir("", pattern);
    return list;
}

char[][] read_args_from_file(char[] fname)
{
    scope InputStream f = new BufferedFile(fname);
    // basically we want to do a 'split' on whitespace.
    // but if there are quoted strings then they should be treated as 
    // one token.
    // Also allow // comments
    char getc_esc() {
        char c = f.getc();
        if (c == '\\') {
            // if escaped return the next char literally
            return f.getc();
        }
        return c;
    }

    void skipspace() {
        char c;
        while(!f.eof && (c=f.getc())!=char.init && isspace(c)) {}
        if (c!=char.init) {
            f.ungetc(c);
        }
    }
    void skip_to_eol() {
        char c;
        while((c=f.getc())!=char.init && c!='\n' && c!='\r') { }
    }
    char[] munch_quote(char q) {
        // go till we find unescaped q char
        char[] qbuf; qbuf.length = 64; qbuf.length = 0;
        bool escaped = false;
        char c;
        while((c=f.getc())!=char.init) {
            if (escaped) {
                qbuf ~= c;
                escaped = false;
            }
            else if (c=='\\') {
                escaped = true;
            }
            else if (c==q) {
                return qbuf;
            }
            else {
                qbuf ~= c;
            }
        }
        return qbuf;
    }

    char[] munch_identifier() {
        char[] id; id.length = 64; id.length=0;

        char p = f.getc();
        if (p=='(') p = ')';
        else if (p=='{') p = '}';
        else { 
            id ~= p;
            p=0;
        }

        char c;
        if (p) {
            while((c=f.getc())!=char.init && c!=p) {
                if (!isspace(c)) 
                    id ~= c;
            }
        }
        else {
            while((c=f.getc())!=char.init && (isalnum(c) || c=='_')) {
                id ~= c;
            }
            if (c!=char.init) f.ungetc(c);
        }
        return id;
    }

    char[] tbuf; tbuf.length = 128; tbuf.length = 0;
    char[] nextToken()
    {
        tbuf.length = 0;
        skipspace();
        bool escaped = false;
        char c;
        while((c=f.getc())!=char.init && (escaped || !isspace(c))) {
            if (escaped) {
                tbuf ~= c;
                escaped = false;
            }
            else if (c=='\\') {
                escaped = true;
            }
            else if (c=='#') {  // # comment
                skip_to_eol();
                skipspace();
            }
            else if (c=='/') {  // start of // comment?
                char c2=f.getc();
                if (c2=='/') {
                    skip_to_eol();
                    skipspace();
                    if (tbuf.length) {
                        // return the current token
                        break;
                    }
                    // otherwise keep looking for the next token
                }
                else {
                    tbuf ~= c;
                    f.ungetc(c2);
                }
            }
            else if (c=='$') {
                // start of env var identifier
                char[] id = munch_identifier();
                tbuf ~= std.string.toString(getenv(toStringz(id)));
            }
            else if (c=='"' || c=='\'') {
                // start quoted string
                tbuf ~= munch_quote(c);
                break;
            }
            else {
                tbuf ~= c;
            }
        }
        return tbuf.dup;
    }

    char[][] ret;
    while (true) {
        char[] t = nextToken();
        if (!t.length) break;
        ret ~= t;
    }
    return ret;
}

T[] insert(T)(T[] s, size_t index, T[] sub)
in
{
    assert(0 <= index && index <= s.length);
}
body
{
    if (sub.length == 0)
	return s;

    if (s.length == 0)
	return sub;

    int newlength = s.length + sub.length;
    T[] result = new T[newlength];

    result[0 .. index] = s[0 .. index];
    result[index .. index + sub.length] = sub;
    result[index + sub.length .. newlength] = s[index .. s.length];
    return result;
}


int main(char[][] args)
{
    bool imageLibOk = true;

    WriterParams param;

    char[] input = "-";
    OutputStream ostream = dout;

    int varcount = 0;

    for(int i=1; i<args.length; i++)
    {
        char[] arg = args[i];
        switch(arg) {
        case "-h":
        case "-help":
        case "--help":
            show_usage(); break;

        case "-x":
        case "-hex":  param.formatchar = "x";  break;

        case "-X":
        case "-HEX":  param.formatchar = "X";  break;

        case "-oct":  param.formatchar = "o";  break;

        case "-d":
        case "-dec":  param.formatchar = "d";  break;
          
        case "-o":
        case "-out":
        case "-output":
            arg = args[++i];
            if (ostream !is dout) {
                // force deletion
                ostream.close(); // buffered file doesn't do it!
                delete ostream;
            }

            if (arg == "-") {
                ostream = dout;
                NL = "\n";
            } else {
                ostream = new BufferedFile(arg, FileMode.OutNew);
                NL = std.string.newline;
            }
            break;
          
        case "-w":
        case "-width":
            arg = args[++i];
            param.formatwidth = std.conv.toInt(arg);
            break;

        case "-i":
        case "-indent":
            arg = args[++i];
            param.indent = std.conv.toInt(arg);
            break;

        case "-wrap":
            arg = args[++i];
            param.linelen = std.conv.toInt(arg);
            break;

        case "-n":
        case "-name":
            arg = args[++i];
            param.varname = arg;
            break;

        case "-stripext":
            param.stripext = true;
            break;

        case "-keepext":
            param.stripext = false;
            break;
            
        case "-t":
        case "-text":
            param.textmode = true;
            param.textwysiwyg = false;
            break;

        case "-tw":
            param.textmode = true;
            param.textwysiwyg = true;
            break;

        case "-b":
        case "-binary":
            param.textmode = false;
            break;

        case "-image":
        case "-imagewh":
            imageLibOk = initImageLib();
            if (!imageLibOk) {
                auto msg  = 
        "Unable to load the image library -- '-image' mode not possible.\n"
        "Please make sure you have the devil shared library (devil.dll/devil.so)\n"
        "somewhere on your PATH.\n"
        "To obtain DevIL binaries visit http://openil.sourceforge.net/download.php";
                report_fatal_error(msg);
            }
            param.decode_image = true;
            if (arg=="-image") {
                param.image2d = true;
            } else {
                param.image2d = false;
            }
            break;

        case "-raw":
        case "-noimage":
            param.decode_image = false;
            break;

        case "-m":
        case "-module":
            arg = args[++i];
            param.modulename = "module " ~ arg ~ ";"~NL~NL;

        default:
            // Generally speaking should be an input file name to wrap
            // or could be @file

            if (arg[0] == '-' && arg != "-") {
                throw new Exception(format("Unknown argument: %s", arg));
            }
            else if (arg[0] == '@') {
                // this is a file from which we should take more args
                args = insert!(char[])(args, i+1, read_args_from_file(arg[1..$]));
            }
            else {
                // it's the input filename
                Writer w = new Writer;
                w.params = param;
                InputStream istream;
                
                if (arg == "-") {
                    if (w.params.varname == "") {
                        w.params.varname = format("data%03d", varcount++);
                    }
                    istream = din;
                    w.output(istream, ostream);
                }
                else {
                    char[][] files = fileglob(arg);
                    bool nameformat = std.string.find(w.params.varname,'%')>=0;
                    foreach(farg; files) {
                        if (w.params.varname == "") {
                            w.params.varname = filename_to_argname(farg,param.stripext);
                        }
                        if (nameformat) {
                            w.params.varname = format(w.params.varname, varcount++);
                        }
                        ostream.writef(param.modulename);
                        param.modulename = "";
                        // devil can't load everything (like GIF) from memory
                        // so give it the file name
                        if (param.decode_image) {
                            w.outputImage(farg, ostream);
                        }
                        else {
                            istream = new BufferedFile(farg);
                            w.output(istream, ostream);
                            delete istream;
                        }
                        if (!nameformat) 
                            w.params.varname = "";
                    }
                }
            }
            param.varname = "";
        }
    }

    if (ostream !is dout) {
        ostream.close();
        delete ostream;
        ostream = dout;
    }

    return 0;
}
