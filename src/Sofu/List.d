module Sofu.List;

private import Sofu.Object;
private import Sofu.Exception;
private import Sofu.Lex;
private import Sofu.Parse;
private import Sofu.Value;
private import Sofu.Map;
private import std.string;

class List : SofuObject
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
        if(token.type() != LexTokenType.LISTSTART) {
            throw new ListExpectedException(token);
        }
        with(token) this(lineNumber(), colNumber(), fileName());
    }
    
    
    List asList() {
        return this;
    }
    
    
    bool isList() {
        return true;
    }
    
    
    /+ Functions for accessing elements +/
    
    SofuObject object(int index)
    {
        if(index >= 0 && index < _elements.length) {
            return _elements[index];
        }
        else {
            throw new ListIndexOutOfRangeException(this, index);
        }
    }
    
    
    Value value(int index)
    {
        return object(index).asValue();
    }
    
    
    List list(int index)
    {
        return object(index).asList();
    }
    
    
    Map map(int index)
    {
        return object(index).asMap();
    }
	
	
	bool hasElement(int index)
	{
		return index < _elements.length;
	}
	
	
	bool hasValue(int index)
	{
		return hasElement(index) && _elements[index].isValue();
	}
    
	bool hasList(int index)
	{
		return hasElement(index) && _elements[index].isList();
	}
    
	bool hasMap(int index)
	{
		return hasElement(index) && _elements[index].isMap();
	}
    
    
    int opApply(int delegate(inout SofuObject elem) dg)
    {
        int result = 0;
        foreach(SofuObject elem; _elements) {
            result = dg(elem);
            if(result)
                break;
        }
        return result;
    }


    char[] typeString() {
        return "List";
    }
    
    
    void appendElement(SofuObject object)
    {
        _elements ~= object;
    }
    
    
    char[] outputString(int indent = 0)
    {
        char[] result;
        foreach(SofuObject object; _elements) {
            for(int i = 0; i < indent; ++i) {
                result ~= " ";
            }
            result ~= object.startSeq();
            result ~= object.outputString(indent + 4);
            result ~= object.endSeq(indent);
            result ~= "\n";
        }
        return result;
    }
    
    
    char[] startSeq() {
        return "(\n";
    }
    
    
    char[] endSeq(int indent = 0) {
        char[] result;
        for(int i = 0; i < indent; ++i) {
            result ~= " ";
        }
        result ~= ")";
        return result;
    }
    
private:
    SofuObject[] _elements;
}


class ListElementException : SofuException
{
    List list;
    int index;
    
    this(char[] msg, List ilist, int iindex)
    {
        super(msg);
        list = ilist;
        index = iindex;
    }
}


class ListIndexOutOfRangeException : ListElementException
{
    this(List list, int index)
    {
        
        char[] msg = format("Sofu: List index %d is out of range for "
            "list at %s, line %d column %d",
            index, list.fileName(), list.lineNumber(), list.colNumber());

        super(msg, list, index);
    }
}


class ListElementTypeException : ListElementException
{
    char[] assumedType;
    
    this(List list, int index, char[] iassumedType)
    {
        assumedType = iassumedType;
        
        char[] msg = format("Sofu: Assumed list element number %d to be of type %s."
            "Actual type is %s in "
            "list at %s, line %d column %d",
            index, assumedType, list.object(index).typeString(),
            list.fileName(), list.lineNumber(),
            list.colNumber());
            
        super(msg, list, index);
    }
}
