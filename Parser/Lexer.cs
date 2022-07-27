using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Abscript.Parser
{
    internal class Script
    {

    }
    internal class ScriptList
    {
        public Dictionary<string, Script> Data = new Dictionary<string, Script>();
    }

    internal class ScriptTokens
    {
        public List<Token> Classes { get; set; } = new List<Token>();

    }



    internal class Token
    {
        public TokenTypes Type { get; set; } = TokenTypes.UNKNOWN;
        public int ScopeDepth { get; set; } = 0;
        public string Identifier { get; set; } = "";

        public int StartPos { get; set; } = -1;
        public int EndPos { get; set; } = -1;

        public Token(int start, int depth)
        {
            StartPos = start;
            ScopeDepth = depth;
        }
        

    }

    internal class ActionToken
    {
        
    }


    public enum TokenTypes
    {
        UNKNOWN,
        IDENTIFIER,
        CLASS,
        ACTION,
        FUNCTION,
        CONSTRUCTOR,
        DESTRUCTOR,
        IMPORT,
        IMPORTSCRIPT,
        DECLARATION,
        ARITHMATIC,
        LOGICAL,
        ACCESS,
        END

    }
    public static class Tokens
    {

        public static readonly Dictionary<string, TokenTypes>
            ReservedWords = new()
            {
                { "in", TokenTypes.IMPORT },
                { "using", TokenTypes.IMPORT },
                { "script", TokenTypes.IMPORT },
                { "class", TokenTypes.CLASS },
                { "act", TokenTypes.ACTION },
                { "func", TokenTypes.FUNCTION },
                { "end", TokenTypes.END },
                { "con", TokenTypes.CONSTRUCTOR },
                { "+~", TokenTypes.CONSTRUCTOR },
                { "des", TokenTypes.DESTRUCTOR },
                { "-~", TokenTypes.DESTRUCTOR },
            },
            ReservedArithmatic = new()
            {
                { "+", TokenTypes.ARITHMATIC },
                { "-", TokenTypes.ARITHMATIC },
                { "*", TokenTypes.ARITHMATIC },
                { "/", TokenTypes.ARITHMATIC }
                
            },
            ReservedLogical = new()
            {
                { "&&", TokenTypes.LOGICAL },
                { "||", TokenTypes.LOGICAL },
                { "for" , TokenTypes.LOGICAL },
                { "each", TokenTypes.LOGICAL },
                { "loop", TokenTypes.LOGICAL },
                { "if", TokenTypes.LOGICAL },
                { "else", TokenTypes.LOGICAL },
                { "elseif", TokenTypes.LOGICAL },
                { "eif", TokenTypes.LOGICAL },
                { "==", TokenTypes.LOGICAL },
                { "!=", TokenTypes.LOGICAL },
                { "<", TokenTypes.LOGICAL },
                { ">", TokenTypes.LOGICAL },
                { ">=", TokenTypes.LOGICAL },
                { "<=", TokenTypes.LOGICAL }
            },
            ReservedAccess = new()
            {
                { ".", TokenTypes.ACCESS },
                { "(", TokenTypes.ACCESS },
                { ")", TokenTypes.ACCESS },
                { "\"", TokenTypes.ACCESS },
                { ":", TokenTypes.ACCESS },
                { "::", TokenTypes.ACCESS }
            },
            ReservedDeclarative = new()
            {
                { "new", TokenTypes.DECLARATION },
                { "=", TokenTypes.DECLARATION },
                { ",", TokenTypes.DECLARATION },
                { "var", TokenTypes.DECLARATION }
            };
        private static Dictionary<string, TokenTypes> _allFull = new();
        public static Dictionary<string, TokenTypes> AllFull
        {
            get
            {
                if (_allFull.Keys.Count > 0) return _allFull;
                else
                {
                    var outlist = new Dictionary<string, TokenTypes>();
                    // Use linq to merge the dictionaries
                    foreach (var item in ReservedWords.Concat(ReservedArithmatic).
                        Concat(ReservedLogical).Concat(ReservedDeclarative).Concat(ReservedAccess))
                    {
                        outlist.Add(item.Key, item.Value);
                    }
                    _allFull = outlist;
                }
                return _allFull;
            }
        }
        private static List<string> _all = new();
        public static List<string> All 
        { 
            get
            {
                if (_all.Count > 0) return _all;
                else
                {
                    var outlist = new List<string>();
                    outlist.AddRange(ReservedWords.Keys);
                    outlist.AddRange(ReservedLogical.Keys);
                    outlist.AddRange(ReservedArithmatic.Keys);
                    outlist.AddRange(ReservedDeclarative.Keys);
                    outlist.AddRange(ReservedAccess.Keys);
                    _all = outlist;
                }
                return _all;
            } 
        }
        public static bool IsReserved(int startPos, int endPos, string rawScript)
        {
            return ReservedWords.Keys.Contains(rawScript.Substring(startPos, endPos - startPos));
        }
        public static bool IsReserved(string stringIn)
        {
            return All.Contains(stringIn);
        }

        public static string Reserved(int startPos, string rawScript)
        {
            // Check the All list by checking if the string matches the upcoming chars in rawScript
            foreach (var item in All)
            {
                if (rawScript.Length >= item.Length + startPos && 
                    rawScript.Substring(startPos, item.Length) == item)
                {
                    return item;
                }
            }
            return "";
        }
        
        public static TokenTypes GetType(string stringIn)
        {
            return AllFull[stringIn];
        }
        
    }

    // This is a basic translation lexer to create readable script objects
    internal class Lexer
    {


        bool PrepareTypeIdentifier(Token token, string rawScript)
        {
            for (int pos = token.StartPos; pos < rawScript.Length; pos++ )
            {
                char curChar = rawScript[pos];
                if (Char.IsLetterOrDigit(curChar) || curChar == '_')
                {
                    token.EndPos = pos;
                    token.Identifier += curChar;
                }
                else
                {
                    switch (curChar)
                    {
                        case ' ': 
                        case '=':
                            if (Tokens.IsReserved(token.Identifier))
                            {
                                token.Type = Tokens.GetType(token.Identifier);
                                return true;
                            } 
                            else
                            {
                                token.Type = TokenTypes.IDENTIFIER;
                                return true;
                            }
                    }
                }
            }
            return true;
        }

        public bool PrepareToken(Token token, string rawScript)
        {
            // The start of a variable must be a letter or _
            var reserved = Tokens.Reserved(token.StartPos, rawScript);
            if ( reserved.Length > 0 )
            {
                token.Type = Tokens.GetType(reserved);
                token.Identifier = reserved;
                token.EndPos = token.StartPos + reserved.Length - 1;
                return true;
            }
            if (Char.IsLetter(rawScript[token.StartPos]) || rawScript[token.StartPos] == '_')
            {
                return PrepareTypeIdentifier(token, rawScript);
            }
            
            return false;
        }

        public void TranslateIntoRawTokens(string rawScriptFile)
        {
            // Replace newlines with spaces.
            string rawScript = rawScriptFile.Replace("\n", " ");
            int currentScope = 0;
            List<Token> list = new List<Token>();
            for(int pos = 0; pos < rawScript.Length; pos++)
            {
                char currentChar = rawScript[pos];
                // Empty character, move along sir.
                if (currentChar == ' ') continue; 
                Token currentToken = new(pos, currentScope);
                if(PrepareToken(currentToken, rawScript))
                {
                    pos = currentToken.EndPos;
                    list.Add(currentToken);
                }
            }

        }

        public Lexer(string _codeInput)
        {
            TranslateIntoRawTokens(_codeInput);
        }

    }
}
