module Sofu.Value;

private import Sofu.Object;
private import Sofu.Parse;
private import Sofu.Lex;
private import Sofu.Exception;
private import std.string;
private import std.utf;
private import std.conv;
private import std.c.stdlib;
private import std.stdio;

class Value : SofuObject
{
public:
    this()
    {
        super();
    }
	
	
	this(wchar[] text)
	{
		_text = text.dup;
		super();
	}
	
	
	this(char[] text)
	{
		_text = std.utf.toUTF16(text);
		super();
	}
    
    
	this(int value)
	{
		_text = std.utf.toUTF16(std.string.toString(value));
		super();
	}


	this(double value)
	{
		_text = std.utf.toUTF16(std.string.toString(value));
		super();
	}
    
    
    this(wchar[] text, int lineNumber, int colNumber, char[] fileName)
    {
        _text = text.dup;
        super(lineNumber, colNumber, fileName);
    }
    
    
    this(LexToken token)
    {
        wchar[] text;
        if(token.type() == LexTokenType.WORD)
        {
            text = token.text();
        }
        else if(token.type() == LexTokenType.QUOTED)
        {
            text = token.text()[1..length-1];
        }
        else {
            throw new ValueExpectedException(token);
        }
        this(text, token.lineNumber(), token.colNumber(), token.fileName());
    }
    
    
    Value asValue() {
        return this;
    }
    
    
    bool isValue() {
        return true;
    }
    
    
    char[] toString() {
        return this.toUTF8();
    }
    
    
    char[] toUTF8() {
        return std.utf.toUTF8(_text);
    }
    
    
    wchar[] toUTF16() {
        return _text.dup;
    }
    
    
    dchar[] toUTF32() {
        return std.utf.toUTF32(_text);
    }
    
    
    int toInt() {
        try {
            int result = std.conv.toInt(this.toUTF8());
            return result;
        }
        catch(ConvError err) {
            throw new ValueConversionException(this, "integer");
        }
    }
    
    
    long toLong() {
        try {
            int result = std.conv.toLong(this.toUTF8());
            return result;
        }
        catch(ConvError err) {
            throw new ValueConversionException(this, "long integer");
        }
    }
    
    
    float toFloat() {
        return cast(float) toDouble();
    }
    
    
    double toDouble() {
        try {
			return std.conv.toDouble(this.toUTF8());
		}
		catch(ConvError err) {
            throw new ValueConversionException(this, "floating point");
        }
    }
    
    
    char[] typeString() {
        return "Value";
    }
    
    
    char[] outputString(int indent = 0) {
        char[] result;
        dchar[] utf32 = std.utf.toUTF32(_text);
        for(int i = 0; i < utf32.length; ++i) {
            if(utf32[i] == '\\') {
                result ~= "\\\\";
            }
            else if(utf32[i] == '\n') {
                result ~= "\\n";
            }
            else if(utf32[i] == '\r') {
                result ~= "\\r";
            }
            else if(utf32[i] == '\t') {
                result ~= "\\t";
            }
            else if(utf32[i] == '"') {
                result ~= "\\\"";
            }
            else {
                encode(result, utf32[i]);
            }
        }
        return result;
    }
    
    
    char[] startSeq() {
        return "\"";
    }
    
    
    char[] endSeq(int indent = 0) {
        return "\"";
    }
private:
    wchar[] _text;
}


class ValueException : SofuException
{
    Value value;
    
    this(char[] msg, Value ivalue)
    {
        value = ivalue;
        super(msg);
    }
}


class ValueConversionException : ValueException
{
    char[] conversionType;
    
    this(Value value, char[] iconversionType)
    {
        conversionType = iconversionType;
        char[] msg = format("Sofu: Can't convert the value \"%s\" "
            "(from %s, line %d column %d) to %s",
            value.toUTF8(), value.fileName(), value.lineNumber(),
            value.colNumber(), conversionType);
        super(msg, value);
    }
}
