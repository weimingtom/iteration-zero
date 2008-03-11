module Sofu.Lex;


private {
    import std.utf;
    import std.stream;
    import std.stdio;
    import std.string;
    import Sofu.Exception;
}

/+ Type of a token +/

enum LexTokenType {
    WHITESPACE, COMMENT,
    WORD, QUOTED,        // word "quoted"
    LISTSTART, LISTEND,  // ( )
    MAPSTART, MAPEND,    // { }
    ASSIGN,              // =
    EOF                  // \0
}


/+ A token (element of a file) +/

class LexToken
{
public:
    /+ Autodetects the type from the first character +/
    
    this(dchar firstch, int lineNumber, int colNumber, char[] fileName)
    {
        _text ~= firstch;
        _lineNumber = lineNumber;
        _colNumber = colNumber;
        _fileName = fileName;
        
        if(isWhiteSpace(firstch)) {
            _type = LexTokenType.WHITESPACE;
        }
        else if(isCommentStart(firstch)) {
            _type = LexTokenType.COMMENT;
        }
        else if(isQuote(firstch)) {
            _type = LexTokenType.QUOTED;
        }
        else if(isListStart(firstch)) {
            _type = LexTokenType.LISTSTART;
        }
        else if(isListEnd(firstch)) {
            _type = LexTokenType.LISTEND;
        }
        else if(isMapStart(firstch)) {
            _type = LexTokenType.MAPSTART;
        }
        else if(isMapEnd(firstch)) {
            _type = LexTokenType.MAPEND;
        }
        else if(isAssign(firstch)) {
            _type = LexTokenType.ASSIGN;
        }
        else if(isEof(firstch)) {
            _type = LexTokenType.EOF;
            _text = "EOF";
        }
        else{
            _type = LexTokenType.WORD;
        }
    }
    
    
    /+ Appends the character to the text if it matches the token's type.
       Returns true if the character was appended, false otherwise. +/
    bit append(dchar ch)
    {
        switch(_type) {
            case LexTokenType.WHITESPACE:
                if(isWhiteSpace(ch)) {
                    _blindAppend(ch);
                    return true;
                }
                return false;
            case LexTokenType.COMMENT:
                if(isLineBreak(ch)) {
                    _blindAppend(ch);
                    _finished = true;
                    return true;
                }
                if(_finished) {
                    return false;
                }
                _blindAppend(ch);
                return true;
            case LexTokenType.WORD:
                if(isSpecial(ch) || isWhiteSpace(ch)) {
                    return false;
                }
                _blindAppend(ch);
                return true;
            case LexTokenType.QUOTED:
                if(_escape) {
                    switch(ch) {
                        case 'n':
                            ch = '\n';
                            break;
                        case 'r':
                            ch = '\r';
                            break;
                        case 't':
                            ch = '\t';
                            break;
                        case '\\':
                            ch = '\\';
                            break;
                        case '"':
                            ch = '"';
                            break;
                        default:
                            dchar[] seq;
                            seq ~= _escapeChar1;
                            seq ~= ch;
                            throw new InvalidEscapeSequenceException(
                                seq, lineNumber(), colNumber(), fileName());
                    }
                    _escape = false;
                    _blindAppend(ch);
                    return true;
                }
                if(isQuote(ch)) {
                    _finished = true;
                    _blindAppend(ch);
                    return true;
                }
                else if(isEscapeStart(ch)) {
                    _escape = true;
                    _escapeChar1 = ch;
                    return true;
                }
                if(!_finished) {
                    _blindAppend(ch);
                    return true;
                }
                return false;
            case LexTokenType.LISTSTART:
                return false;
            case LexTokenType.LISTEND:
                return false;
            case LexTokenType.MAPSTART:
                return false;
            case LexTokenType.MAPEND:
                return false;
            case LexTokenType.ASSIGN:
                return false;
            case LexTokenType.EOF:
                return false;
        }
    }
    

    LexTokenType type()
    {
        return _type;
    }
    
    
    char[] typeString()
    {
        switch(_type)
        {
            case LexTokenType.WHITESPACE:
                return "WhiteSpace";
            case LexTokenType.COMMENT:
                return "Comment";
            case LexTokenType.WORD:
                return "Word";
            case LexTokenType.QUOTED:
                return "Quoted Value";
            case LexTokenType.LISTSTART:
                return "List Start";
            case LexTokenType.LISTEND:
                return "List End";
            case LexTokenType.MAPSTART:
                return "Map Start";
            case LexTokenType.MAPEND:
                return "Map End";
            case LexTokenType.ASSIGN:
                return "Assignment Character";
            case LexTokenType.EOF:
                return "End of File";
        }
    }
    
    
    wchar[] text()
    {
        return _text;
    }
    
    
    int lineNumber()
    {
        return _lineNumber;
    }
    
    
    int colNumber()
    {
        return _colNumber;
    }
    
    
    char[] fileName()
    {
        return _fileName;
    }
    
    
private:
    LexTokenType _type;
    wchar[] _text;
    int _lineNumber;
    int _colNumber;
    char[] _fileName;
    
    bit _finished = false;
    
    bit _escape = false;
    dchar _escapeChar1;
    
    
    /+ Appends the character without checking for the appropriate type +/
    void _blindAppend(dchar ch)
    {
        wchar[] wc;
        encode(wc, ch);
        _text ~= wc;
    }
}


bit isIgnoreChar(dchar ch) {
    return ch == '\r';
}

bit isWhiteSpace(dchar ch) {
    return ch == ' ' || ch == '\t' || ch == '\n';
}

bit isLineBreak(dchar ch) {
    return ch == '\n';
}

bit isQuote(dchar ch) {
    return ch == '"';
}

bit isCommentStart(dchar ch) {
    return ch == '#';
}

bit isListStart(dchar ch) {
    return ch == '(';
}

bit isListEnd(dchar ch) {
    return ch == ')';
}

bit isMapStart(dchar ch) {
    return ch == '{';
}

bit isMapEnd(dchar ch) {
    return ch == '}';
}

bit isAssign(dchar ch) {
    return ch == '=';
}

bit isSpecial(dchar ch) {
    return isCommentStart(ch) || isQuote(ch) || isListStart(ch) || isListEnd(ch)
        || isMapStart(ch) || isMapEnd(ch) || isAssign(ch);
}

bit isEof(dchar ch) {
    return ch == 0;
}

bit isEscapeStart(dchar ch) {
    return ch == '\\';
}


/+ Opens a certain file, lexes it, and closes it again. +/

LexToken[] lexFile(char[] fileName)
{
    debug(SofuLex) {
        writefln("DEBUG SofuLex: Lexing file '%s'", fileName);
    }
    LexToken[] tokens;
    
    File realfile = new File(fileName);
    BufferedStream bfile = new BufferedStream(realfile);
    EndianStream file = new EndianStream(bfile);

    int bom = file.readBOM();
    if(bom == -1) {
        debug(SofuLex) writefln("DEBUG SofuLex: No BOM found");
        bom = BOM.UTF8;
    }
    
    if(bom == BOM.UTF8) {
        debug(SofuLex) {
            writefln("DEBUG SofuLex: Detected encoding UTF-8");
        }
        char[] buffer;
        buffer.length = 4;
        uint filled = 0; // The first char that isn't filled
        bit eof = false;
        
        dchar getc8()
        {
            while(filled < 4 && !file.eof()) {
                buffer[filled] = file.getc();
                    debug(SofuLex) {
                        writefln("DEBUG SofuLex: Read Byte %s (%X)",
                            buffer[filled], cast(uint)buffer[filled]);
                    }
                    ++filled;
            }
            uint idx = 0;
            dchar result = decode(buffer, idx);
            debug(SofuLex) writefln("DEBUG SofuLex: Decoded %d UTF-8 units", idx);
            
            char[] nbuffer;
            nbuffer.length = 4;
            nbuffer[0..filled-idx] = buffer[idx..filled];
            buffer = nbuffer;
            filled -= idx;
            
            if(filled == 0 && file.eof()) {
                eof = true;
            }
            return result;
        }
        
        
        bit eof8()
        {
            return eof;
        }
        
        tokens = lex(&getc8, &eof8, fileName.dup);
    }
    else if(bom == BOM.UTF16BE || bom == BOM.UTF16LE) {
        debug(SofuLex) {
            writefln("DEBUG SofuLex: Detected encoding UTF-16 %s",
                bom == BOM.UTF16BE ? "BE" : "LE");
        }
        
        wchar[] buffer;
        buffer.length = 2;
        uint filled = 0;
        bit eof = false;

        dchar getc16()
        {
            while(filled < 2 && !file.eof()) {
                buffer[filled] = file.getcw();
                if(!file.eof()) {
                    debug(SofuLex) {
                        writefln("DEBUG SofuLex: Read WChar %s (%X)",
                            buffer[filled], cast(uint)buffer[filled]);
                    }
                    ++filled;
                }
            }
            uint idx = 0;
            dchar result = decode(buffer, idx);
            debug(SofuLex) writefln("DEBUG SofuLex: Decoded %d UTF-16 units", idx);

            wchar[] nbuffer;
            nbuffer.length = 2;
            nbuffer[0..filled-idx] = buffer[idx..filled];
            buffer = nbuffer;
            filled -= idx;

            if(filled == 0 && file.eof()) {
                eof = true;
            }

            return result;
        }
        
        bit eof16() {
            return eof;
        }
        
        tokens = lex(&getc16, &eof16, fileName.dup);
    }
    else if(bom == BOM.UTF32BE || bom == BOM.UTF32LE) {
        debug(SofuLex) {
            writefln("DEBUG SofuLex: Detected encoding UTF-32 %s",
                bom == BOM.UTF32BE ? "BE" : "LE");
        }
        
        dchar getc32() {
            dchar ch;
            file.read(ch);
            return ch;
        }
        
        bit eof32() {
            return file.eof();
        }
        
        tokens = lex(&getc32, &eof32, fileName.dup);
    }
    
    file.close();
    bfile.close();
    realfile.close();
    
    return tokens;
}


/+ Lexes a complete file +/

LexToken[] lex(dchar delegate() getc, bit delegate() eof, char[] fileName)
{
    LexToken[] tokens;
    LexToken curToken;
    
    int lineNumber = 1;
    int colNumber = 1;
    
    if(eof()) {
        tokens ~= new LexToken(0, lineNumber, colNumber, fileName);
        return tokens;
    }

    dchar firstchar = getc();
    debug(SofuLex) {
        writefln("DEBUG SofuLex: Got Character %s (%X)", firstchar, cast(uint)firstchar);
    }
    
    curToken = new LexToken(firstchar, lineNumber, colNumber, fileName);
    tokens ~= curToken;
    
    while(!eof())
    {
        dchar ch = getc();

        while(isIgnoreChar(ch) && !eof()) {
            debug(SofuLex) writefln("DEBUG SofuLex: Ignored a character");
            ch = getc();
        }

        debug(SofuLex) {
            writefln("DEBUG SofuLex: Got Character %s (%X)", ch, cast(uint)ch);
        }

        if(!curToken.append(ch)) {
            curToken = new LexToken(ch, lineNumber, colNumber, fileName);
            tokens ~= curToken;
        }


        if(!isLineBreak(ch)) {
            colNumber += 1;
        }
        else {
            lineNumber += 1;
            colNumber = 1;
        }
    }

    // Append EOF token
    tokens ~= new LexToken(0, lineNumber, colNumber, fileName);

    debug(SofuLex) {
        writefln("DEBUG SofuLex: Lexed %d tokens", tokens.length);
        writef("DEBUG SofuLex: ");
        foreach(LexToken token; tokens) {
            writef("%s, ", token.typeString());
        }
        writefln();
    }
    
    return tokens;
}


class InvalidEscapeSequenceException : SofuException
{
    dchar[] sequence;
    int lineNumber;
    int colNumber;
    char[] fileName;
    
    this(dchar[] isequence, int ilineNumber, int icolNumber, char[] ifileName)
    {
        sequence = isequence;
        lineNumber = ilineNumber;
        colNumber = icolNumber;
        fileName = ifileName;
        char[] msg = format("Sofu: Invalid escape sequence '%s' "
            "in file %s, line %d column %d",
            toUTF8(sequence), fileName, lineNumber, colNumber);
        super(msg);
    }
}
