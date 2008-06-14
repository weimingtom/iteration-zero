module Sofu.Parse;

private import Sofu.Object;
private import Sofu.Value;
private import Sofu.List;
private import Sofu.Map;
private import Sofu.Lex;
private import Sofu.Exception;
private import std.string;
private import std.utf;
private import std.stdio;

void parseMapAssignment(LexToken[] tokens, inout uint idx, Map map)
{
    wchar[] name;
    if(tokens[idx].type() == LexTokenType.WORD) {
        name = tokens[idx].text();
        ++idx;
    }
    else {
        throw new AttributeNameExpectedException(tokens[idx]);
    }
    parseWhiteSpace(tokens, idx);
    if(tokens[idx].type() == LexTokenType.ASSIGN) {
        ++idx;
    }
    else {
        throw new AssignmentCharacterExpectedException(tokens[idx]);
    }
    parseWhiteSpace(tokens, idx);
    SofuObject object = parseObject(tokens, idx);
    map.setAttribute(name, object);
    parseWhiteSpace(tokens, idx);
}


void parseWhiteSpace(LexToken[] tokens, inout uint idx)
{
    while(tokens[idx].type() == LexTokenType.WHITESPACE
        || tokens[idx].type() == LexTokenType.COMMENT)
    {
        ++idx;
    }
}


SofuObject parseObject(LexToken[] tokens, inout uint idx)
{
    if(tokens[idx].type() == LexTokenType.WORD
        || tokens[idx].type() == LexTokenType.QUOTED)
    {
        return parseValue(tokens, idx);
    }
    else if(tokens[idx].type() == LexTokenType.LISTSTART) {
        return parseList(tokens, idx);
    }
    else if(tokens[idx].type() == LexTokenType.MAPSTART) {
        return parseMap(tokens, idx);
    }
    else {
        throw new ObjectExpectedException(tokens[idx]);
    }
}


Value parseValue(LexToken[] tokens, inout uint idx)
{
    Value value = new Value(tokens[idx]);
    ++idx;
    return value;
}


List parseList(LexToken[] tokens, inout uint idx)
{
    if(tokens[idx].type() == LexTokenType.LISTSTART) {
        List list = new List(tokens[idx]);
        ++idx;
        parseWhiteSpace(tokens, idx);
        while(tokens[idx].type() != LexTokenType.LISTEND) {
            SofuObject object = parseObject(tokens, idx);
            list.appendElement(object);
            parseWhiteSpace(tokens, idx);
        }
        ++idx;
        return list;
    }
    else {
        throw new ListExpectedException(tokens[idx]);
    }
}


Map parseMap(LexToken[] tokens, inout uint idx)
{
    if(tokens[idx].type() == LexTokenType.MAPSTART) {
        Map map = new Map(tokens[idx]);
        ++idx;
        parseWhiteSpace(tokens, idx);
        while(tokens[idx].type() != LexTokenType.MAPEND) {
            parseMapAssignment(tokens, idx, map);
        }
        ++idx;
        return map;
    }
    else {
        throw new MapExpectedException(tokens[idx]);
    }
}


class ParseException : SofuException
{
    LexToken token;
    
    this(char[] msg, LexToken itoken)
    {
        token = itoken;
        
        super(msg);
    }
}


class AttributeNameExpectedException : ParseException
{
    this(LexToken token)
    {
        char[] msg = format("Sofu: Expected attribute name or closing brace instead of "
            "'%s' (of type %s) in "
            "file %s, line %d column %d",
            toUTF8(token.text()), token.typeString(), token.fileName(),
            token.lineNumber(), token.colNumber());
        super(msg, token);
    }
}


class AssignmentCharacterExpectedException : ParseException
{
    this(LexToken token)
    {
        char[] msg = format("Sofu: Expected assignment character (=) instead of "
            "'%s' (of type %s) in file %s, line %d column %d",
            toUTF8(token.text()), token.typeString(), token.fileName(),
            token.lineNumber(), token.colNumber());
        super(msg, token);
    }
}


class ObjectExpectedException : ParseException
{
    this(LexToken token)
    {
        char[] msg;
        if(token.type() != LexTokenType.EOF) {
            msg = format("Sofu: Expected an object (Value, List or Map) instead of "
                "'%s' (of type %s) in file %s, line %d column %d",
                toUTF8(token.text()), token.typeString(), token.fileName(),
                token.lineNumber(), token.colNumber());
        }
        else {
            msg = format("Sofu: Preliminary End of File in file %s, "
                "line %d column %d - did you forget to close a bracket?",
                token.fileName(), token.lineNumber(), token.colNumber());
        }
        super(msg, token);
    }
}


class ValueExpectedException : ParseException
{
    this(LexToken token)
    {
        char[] msg = format("Sofu: Expected a Value instead of "
            "'%s' (of type %s) in file %s, line %d column %d",
            toUTF8(token.text()), token.typeString(), token.fileName(),
            token.lineNumber(), token.colNumber());
        super(msg, token);
    }
}


class ListExpectedException : ParseException
{
    this(LexToken token)
    {
        char[] msg = format("Sofu: Expected a List instead of "
            "'%s' (of type %s) in file %s, line %d column %d",
            toUTF8(token.text()), token.typeString(), token.fileName(),
            token.lineNumber(), token.colNumber());
        super(msg, token);
    }
}


class MapExpectedException : ParseException
{
    this(LexToken token)
    {
        char[] msg = format("Sofu: Expected a Map instead of "
            "'%s' (of type %s) in file %s, line %d column %d",
            toUTF8(token.text()), token.typeString(), token.fileName(),
            token.lineNumber(), token.colNumber());
        super(msg, token);
    }
}
