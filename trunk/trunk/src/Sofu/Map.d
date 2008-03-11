module Sofu.Map;

private import Sofu.Object;
private import Sofu.Value;
private import Sofu.List;
private import std.utf;
private import Sofu.Exception;
private import Sofu.Lex;
private import Sofu.Parse;
private import std.stdio;
private import std.string;

class Map : SofuObject
{
public:
    this()
    {
        super();
    }
    
    
    this(int lineNumber, int colNumber, char[] fileName)
    {
        super(lineNumber, colNumber, fileName);
    }
    
    
    this(LexToken token)
    {
        if(token.type() != LexTokenType.MAPSTART) {
            throw new MapExpectedException(token);
        }
        with(token) this(lineNumber(), colNumber(), fileName());
    }
    
    
    void setAttribute(wchar[] name, SofuObject value)
    {
        _attribs[name] = value;
    }
    
    
    void remAttribute(wchar[] name)
    {
        _attribs.remove(name);
    }
    
    
    Map asMap() {
        return this;
    }
    
    bool isMap() {
        return true;
    }
    
    
    /+ Functions for accessing attributes +/
    
    SofuObject object(wchar[] name)
    {
        if((name in _attribs) is null) {
            throw new AttributeDoesNotExistException(this, name);
        }
		else {
			SofuObject result = _attribs[name];
			return result;
		}
    }
    
    
    Value value(wchar[] name)
    {
        return object(name).asValue();
    }
    
    
    List list(wchar[] name)
    {
        return object(name).asList();
    }
    
    
    Map map(wchar[] name)
    {
        return object(name).asMap();
    }
    
    
    bool hasAttribute(wchar[] name)
    {
        return (name in _attribs) !is null;
    }
    
    bool hasValue(wchar[] name)
    {
        if(hasAttribute(name)) {
            return _attribs[name].isValue();
        }
        return false;
    }
    
    bool hasList(wchar[] name)
    {
        if(hasAttribute(name)) {
            return _attribs[name].isList();
        }
        return false;
    }
    
    bool hasMap(wchar[] name)
    {
        if(hasAttribute(name)) {
            return _attribs[name].isMap();
        }
        return false;
    }
    
    
    /+ Enumerating attributes with foreach +/
    
    /+ Enumerate all attribute names and attributes +/
    int opApply(int delegate(inout wchar[] attribName, inout SofuObject attrib) dg)
    {
        int result = 0;

        foreach(wchar[] attribName, SofuObject attrib; _attribs)
        {
            result = dg(attribName, attrib);
            if (result)
                break;
        }
        return result;
    }
    
    
    /+ Enumerate only the attributes - you don't get their names +/
    int opApply(int delegate(inout SofuObject attrib) dg)
    {
        int result = 0;
        
        foreach(wchar[] attribName, SofuObject attrib; _attribs)
        {
            result = dg(attrib);
            if(result)
                break;
        }
        return result;
    }
    
    
    /+ Used for error messages +/
    char[] typeString() {
        return "Map";
    }
    
    
    /+ Output the map in Sofu format with indent spaces indentation +/
    char[] outputString(int indent = 0)
    {
        char[] result;
        foreach(wchar[] key; _attribs.keys) {
            for(int i = 0; i < indent; ++i) {
                result ~= " ";
            }
            result ~= toUTF8(key);
            result ~= " = ";
            result ~= _attribs[key].startSeq();
            result ~= _attribs[key].outputString(indent + 4);
            result ~= _attribs[key].endSeq(indent);
            result ~= "\n";
        }
        return result;
    }
    
    
    /+ Output a map starting sequence (including newline) +/
    char[] startSeq() {
        return "{\n";
    }
    
    
    /+ Output a map ending sequence +/
    char[] endSeq(int indent = 0) {
        char[] result;
        for(int i = 0; i < indent; ++i) {
            result ~= " ";
        }
        result ~= "}";
        return result;
    }
    
private:
    SofuObject[wchar[]] _attribs;
}


class AttributeException : SofuException
{
    Map map;
    wchar[] attribName;
    
    this(char[] msg, Map imap, wchar[] iattribName)
    {
        super(msg);
        map = imap;
        attribName = iattribName;
    }
}


class AttributeDoesNotExistException : AttributeException
{
    this(Map map, wchar[] attribName)
    {
        
        char[] msg = format("Sofu: Map attribute %s does not exist in "
            "map at %s, line %d column %d",
            toUTF8(attribName), map.fileName(), map.lineNumber(), map.colNumber());

        super(msg, map, attribName);
    }
}
