module Sofu.Object;

private import Sofu.Value;
private import Sofu.List;
private import Sofu.Map;
private import Sofu.Exception;
private import std.string;

class SofuObject
{
public:
    this()
    {
        _inAFile = false;
    }
    
    this(int lineNumber, int colNumber, char[] fileName)
    {
        _inAFile = true;
        _lineNumber = lineNumber;
        _colNumber = colNumber;
        _fileName = fileName;
    }
    
    
    int lineNumber() {
        return _lineNumber;
    }
    
    int colNumber() {
        return _colNumber;
    }
    
    char[] fileName() {
        return _fileName;
    }
    
    
    Value asValue() {
        throw new TypeException(this, "Value");
        return null;
    }
    
    
    List asList() {
        throw new TypeException(this, "List");
        return null;
    }
    
    
    Map asMap() {
        throw new TypeException(this, "Map");
        return null;
    }
    
    
    bool isValue() {
        return false;
    }
    
    
    bool isList() {
        return false;
    }
    
    
    bool isMap() {
        return false;
    }
    
    
    char[] typeString() { assert(0); return null; }
    
    
    char[] outputString(int indent = 0) { assert(0); return null; }
    char[] startSeq() { assert(0); return null; }
    char[] endSeq(int indent = 0) { assert(0); return null; }

private:
    bool _inAFile;
    int _lineNumber;
    int _colNumber;
    char[] _fileName;
}


class TypeException : SofuException
{
    SofuObject object;
    char[] assumedType;
    
    this(SofuObject iobject, char[] iassumedType)
    {
        object = iobject;
        assumedType = iassumedType;
        
        char[] msg = format("Sofu: Assumed the object"
            " at %s, line %d column %d to be of type %s. "
            "Actual type is %s",
            object.fileName(), object.lineNumber(),
            object.colNumber(),
            assumedType, object.typeString());
            
            
        super(msg);
    }
}
