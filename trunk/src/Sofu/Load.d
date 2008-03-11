module Sofu.Load;

private import Sofu.Map;
private import Sofu.Lex;
private import Sofu.Parse;
private import std.stdio;


Map loadFile(char[] fileName)
{
    LexToken[] tokens = lexFile(fileName);
    uint idx = 0;
    
    Map root = new Map(1, 1, fileName);
    
    while(tokens[idx].type != LexTokenType.EOF) {
        parseWhiteSpace(tokens, idx);
        parseMapAssignment(tokens, idx, root);
    }
    
    return root;
}
