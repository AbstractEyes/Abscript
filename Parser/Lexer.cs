using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
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
        public List<TokenSimple> Types { get; set; } = new() { TokenSimple.NULL };
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


    /*
     * Token combinations;
     *      These are the combinations of tokens that can be found in a script.
     *      Every single one needs to have a series of patterns for matching and processing
     *      The individual patterns are defined in the TokenCombination class.
     *      The Tokens class houses a complete Directives dictionary of all of the patterns currently in use.
     */
    public enum TokenCategory
    {
        UNKNOWN,
        IDENTIFIER,
        CLASS,
        ACTION,
        FUNCTION,
        CONSTRUCTOR,
        DESTRUCTOR,
        IMPORT,
        DECLARATION,
        ARITHMATIC,
        COMPARATIVE,
        LOGICGATE,
        ITERATION,
        ACCESS,
        COMMENT
    }

    public enum TokenCombo
    {
        UNKNOWN,
        COMMENT_LINE,
        COMMENT_BLOCK_OPEN,
        COMMENT_BLOCK_CLOSE,
        ANY_IDENTIFIER,
        CLASS_IDENTIFIER
        
    }

    /*
     * 
     * Okay when we get a string, we determine which type of token it is.
     * If the string matches any of the LexerDirectives, we create a token.
     * We then follow the LexerDirective's instructions on how to read the characters from there to get more tokens.
     * We can then seek ahead and determine if the tokens match the required directive pattern.
     * 
     * There will be a specific order of checking for the directives.
     * The encapsulation directives will split into tree tokens on first check.
     * 
     */

    public enum TokenSimple
    {
        ANY,
        ANY_CHAR,
        ANY_DIGIT,
        ANY_LETTER,
        OPERATOR,
        DEFAULT,
        NULL,
        BOOL,
        TYPE,
        WHITESPACE,
        NEWLINE,
        ESCAPE_CHAR,
        WILDCARD,
        END,
        DOT,
        COLON,
        EQUAL,
        NOT_EQUAL,
        LESS_THAN,
        LESS_THAN_EQUAL,
        GREATER_THAN,
        GREATER_THAN_EQUAL,
        PLUS,
        MINUS,
        MULTIPLY,
        DIVIDE,
        MODULUS,
        EXPONENT,
        AMPERSAND,
        PIPE,
        CARET,
        TILDE,
        QUESTION,
        EXCLAMATION,
        DOUBLE_QUOTE,
        SINGLE_QUOTE,
        BACKTICK,
        BACKSLASH,
        PERCENT,
        DOLLAR,
        HASH,
        AT,
        AMPERSAND_AMPERSAND,
    }

    public enum LexerOperation
    {
        NONE,               // Unknown pattern, does not exist.
        OPEN,               // Opens and increases the scope depth by 1
        CLOSE,              // Closes and decreases the scope depth by 1
        NULLIFY_TO,         // Nullifies all ahead of this until a comparator token is hit or the lexer state is changed.
        NORMALIZE,          // Normalizes and discontinues token nullification.
        MATCH_REGEX         // Matches regex pattern until it hits a comparator token or the lexer state is changed.
    }

    public class LexerCommand
    {
        public LexerOperation Operation { get; private set; } = LexerOperation.NONE;
        public TokenSimple Token { get; private set; } = TokenSimple.NULL;
        public TokenCombo Specific { get; private set; } = TokenCombo.UNKNOWN;
        public TokenCategory Category { get; private set; } = TokenCategory.UNKNOWN;
        public List<object> ComparatorClose { get; private set; } = new List<object>();
        public List<object> ComparatorInvalid { get; private set; } = new List<object>();
        public string Value { get; private set; } = "";
        public bool IsSimple()
        {
            return Token != TokenSimple.NULL && Specific == TokenCombo.UNKNOWN && TokenCategory.UNKNOWN == Category;
        }

        public LexerCommand SetValue(string value)
        {
            Value = value;
            return this;
        }
        public LexerCommand SetOp(LexerOperation operation)
        {
            Operation = operation;
            return this;
        }
        public LexerCommand SetComparatorClose(params object[] tokens)
        {
            ComparatorClose = tokens.ToList();
            return this;
        }
        public LexerCommand SetComparatorInvalid(params object[] tokens)
        {
            ComparatorInvalid = tokens.ToList();
            return this;
        }

        public bool IsWhitelisted(params object[] tokens)
        {
            // determines if the whitelist contines any of the tokens
            return ComparatorClose.Any(token => tokens.Contains(token));
        }
        public bool IsBlacklisted(params object[] tokens)
        {
            // determines if the whitelist contines any of the tokens
            return ComparatorInvalid.Any(token => tokens.Contains(token));
        }

    }
    
    public class LexerCombination
    {
        public List<LexerCommand> Pattern { get; private set; } = new List<LexerCommand>();

        public LexerCombination SetCombinationPattern(params LexerCommand[] patternIn)
        {
            Pattern = patternIn.ToList();
            return this;
        }

    }

    public class TokenDirective
    {
        public List<LexerCombination> Combinations { get; private set; } = new();
        public List<object> Identifiers { get; private set; } = new();
        public TokenCombo Type { get; private set; } = TokenCombo.UNKNOWN;
        public TokenDirective SetType(TokenCombo token)
        {
            Type = token;
            return this;
        }

        public TokenDirective SetLexerMatch(params object[] match)
        {
            Identifiers = match.ToList();
            return this;
        }
        public TokenDirective DefineCombinations(params LexerCombination[] combinations)
        {
            Combinations = combinations.ToList();
            return this;
        }
    }
    public class TokenDirectiveContainer
    {
        public List<TokenDirective> Identifiers { get; private set; } = new();
        public TokenCombo Type { get; private set; } = TokenCombo.UNKNOWN;
        public TokenDirectiveContainer SetDirectives(params TokenDirective[] identifiers)
        {
            Identifiers = identifiers.ToList();
            return this;
        }
        public TokenDirectiveContainer SetType(TokenCombo tokenType)
        {
            Type = tokenType;
            return this;
        }
    }
    public static class Tokens
    {

        // A singular directives list to dictate what all of the basic token types can do.
        /*
         * TokenDirectiveContainer contains a sequence of token directives.
         * Each TokenDirective contains a list of identifiers that tell the lexer what to look for
         * The lexer will iterate each of these every time it pulls a character attempting to match a sequence
         * If a sequence is matched, it automatically defines the basic token.
         * 
         * After a token is defined, the lexer defines token as the matched category and directive type then moves on
         * 
         * The second pass of the lexer uses the defined categories to determine what fits where and then
         * creates tokenized patterns for lexical analysis and dictionary storage using a Lexer Indexing system.
         * 
         * Todo; Each category will have it's parameters defined for lexical storage and access defined
         *      They will be within the same objects here. Hopefully it's readable? It's kinda clean, getting messy.
         * 
         */

        public static readonly Dictionary<TokenCategory, TokenDirectiveContainer>
            TokenDirectives = new()
            {
                {
                    TokenCategory.COMMENT,
                    new TokenDirectiveContainer()
                    .SetDirectives(
                        new TokenDirective()
                        .SetType(TokenCombo.COMMENT_LINE)
                        .SetLexerMatch("//")
                        .DefineCombinations(
                            new LexerCombination().SetCombinationPattern(
                                new LexerCommand().SetOp(LexerOperation.NULLIFY_TO)
                                .SetComparatorClose(TokenSimple.NEWLINE)
                            )
                        ),
                        new TokenDirective()
                        .SetType(TokenCombo.COMMENT_BLOCK_OPEN)
                        .SetLexerMatch("/*")
                        .DefineCombinations(
                            new LexerCombination()
                            .SetCombinationPattern(
                                new LexerCommand().SetOp(LexerOperation.OPEN),
                                new LexerCommand().SetOp(LexerOperation.NULLIFY_TO)
                                .SetComparatorClose(TokenCombo.COMMENT_BLOCK_CLOSE)
                            )
                        ),
                        new TokenDirective()
                        .SetType(TokenCombo.COMMENT_BLOCK_CLOSE)
                        .SetLexerMatch("*/")
                        .DefineCombinations(
                            new LexerCombination()
                            .SetCombinationPattern(
                                new LexerCommand().SetOp(LexerOperation.CLOSE),
                                new LexerCommand().SetOp(LexerOperation.NORMALIZE)
                            )
                        )
                    )
                },
                {
                    TokenCategory.IDENTIFIER,
                    new TokenDirectiveContainer().SetDirectives(
                        new TokenDirective().SetType(TokenCombo.ANY_IDENTIFIER)
                        // Set entry identifier of regex grabbing one return value
                        .SetLexerMatch(new Regex(@"[a-zA-Z_][a-zA-Z0-9_]*"))
                        .DefineCombinations(
                            new LexerCombination()
                            .SetCombinationPattern(
                                new LexerCommand()
                                .SetOp(LexerOperation.MATCH_REGEX)
                                .SetComparatorClose(
                                    TokenSimple.WHITESPACE,
                                    TokenSimple.NEWLINE,
                                    TokenSimple.OPERATOR
                                )
                            ),
                            new LexerCombination()
                            .SetCombinationPattern(
                                new LexerCommand()
                                .SetOp(LexerOperation.MATCH_REGEX)
                                .SetComparatorClose(
                                    TokenSimple.WHITESPACE,
                                    TokenSimple.NEWLINE,
                                    TokenSimple.OPERATOR,
                                    TokenSimple.COMMA,
                                    TokenSimple.SEMICOLON,
                                    TokenSimple.OPEN,
                                    TokenSimple.CLOSE,
                                    TokenSimple.BRACKET_OPEN,
                                    TokenSimple.BRACKET_CLOSE,
                                    TokenSimple.PARENTHESIS_OPEN,
                                    TokenSimple.PARENTHESIS_CLOSE,
                                    TokenSimple.BRACE_OPEN,
                                    TokenSimple.BRACE_CLOSE,
                                    TokenSimple.DOT,
                                    TokenSimple.COLON,
                                    TokenSimple.EQUAL,
                                    TokenSimple.NOT_EQUAL,
                                    TokenSimple.LESS_THAN,
                                    TokenSimple.LESS_THAN_EQUAL,
                                    TokenSimple.GREATER_THAN,
                                    TokenSimple.GREATER_THAN_EQUAL,
                                    TokenSimple.PLUS,
                                    TokenSimple.MINUS,
                                    TokenSimple.MULTIPLY,
                                    TokenSimple.DIVIDE,
                                    TokenSimple.MODULUS,
                                    TokenSimple.EXPONENT,
                                    TokenSimple.AMPERSAND,
                                    TokenSimple.PIPE,
                                    TokenSimple.CARET,
                                    TokenSimple.TILDE,
                                    TokenSimple.QUESTION,
                                    TokenSimple.EXCLAMATION,
                                    TokenSimple.DOUBLE_QUOTE,
                                    TokenSimple.SINGLE_QUOTE,
                                    TokenSimple.BACKTICK,
                                    TokenSimple.BACKSLASH,
                                    TokenSimple.PERCENT,
                                    TokenSimple.DOLLAR,
                                    TokenSimple.HASH,
                                    TokenSimple.AT,
                                    TokenSimple.AMPERSAND_AMPERSAND,
                                    TokenSimple.PIPE_PIPE,
                                    TokenSimple.EQUAL_EQUAL,
                                    TokenSimple.NOT_EQUAL_EQUAL,
                                    
                        )
                    )

                }

                

            };
            
        public static readonly Dictionary<string, TokenCategory>
            ReservedWords = new()
            {
                { "in", TokenCategory.IMPORT },
                { "using", TokenCategory.IMPORT },
                { "script", TokenCategory.IMPORT },
                { "class", TokenCategory.CLASS },
                { "act", TokenCategory.ACTION },
                { "func", TokenCategory.FUNCTION },
                //{ "end", TokenCategory.END },
                { "con", TokenCategory.CONSTRUCTOR },
                { "+~", TokenCategory.CONSTRUCTOR },
                { "des", TokenCategory.DESTRUCTOR },
                { "-~", TokenCategory.DESTRUCTOR },
            },
            ReservedArithmatic = new()
            {
                { "+", TokenCategory.ARITHMATIC },
                { "-", TokenCategory.ARITHMATIC },
                { "*", TokenCategory.ARITHMATIC },
                { "/", TokenCategory.ARITHMATIC }
                
            },
            ReservedLogical = new()
            {
                { "for" , TokenCategory.ITERATION },
                { "each", TokenCategory.ITERATION },
                { "loop", TokenCategory.ITERATION },
                { "&&", TokenCategory.COMPARATIVE },
                { "and", TokenCategory.COMPARATIVE },
                { "||", TokenCategory.COMPARATIVE },
                { "or", TokenCategory.COMPARATIVE },
                { "if", TokenCategory.LOGICGATE },
                { "else", TokenCategory.LOGICGATE },
                { "elseif", TokenCategory.LOGICGATE },
                { "eif", TokenCategory.LOGICGATE },
                { "==", TokenCategory.COMPARATIVE },
                { "!=", TokenCategory.COMPARATIVE },
                { "<", TokenCategory.COMPARATIVE },
                { ">", TokenCategory.COMPARATIVE },
                { ">=", TokenCategory.COMPARATIVE },
                { "<=", TokenCategory.COMPARATIVE }
            },
            ReservedFlowControl = new()
            {
                { ".", TokenCategory.ACCESS },
                { "(", TokenCategory.ACCESS },
                { ")", TokenCategory.ACCESS },
                { "\"", TokenCategory.ACCESS },
                { ":", TokenCategory.ACCESS },
                { "::", TokenCategory.ACCESS },
                //{ " ", TokenCategory.WHITESPACE }
            },
            ReservedDeclarative = new()
            {
                { "new", TokenCategory.DECLARATION },
                { "=", TokenCategory.DECLARATION },
                { ",", TokenCategory.DECLARATION },
                { "var", TokenCategory.DECLARATION }
            };



        private static Dictionary<string, TokenCategory> _allFull = new();
        public static Dictionary<string, TokenCategory> AllFull
        {
            get
            {
                if (_allFull.Keys.Count > 0) return _allFull;
                else
                {
                    var outlist = new Dictionary<string, TokenCategory>();
                    // Use linq to merge the dictionaries
                    foreach (var item in ReservedWords.Concat(ReservedArithmatic).
                        Concat(ReservedLogical).Concat(ReservedDeclarative).Concat(ReservedFlowControl))
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
                    outlist.AddRange(ReservedFlowControl.Keys);
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
        
        public static TokenCategory GetType(string stringIn)
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
                                token.Type = TokenCategory.IDENTIFIER;
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

        void TranslateProcessedTokens(List<Token> tokens)
        {
            
        }

        public Lexer(string _codeInput)
        {
            // First pass
            TranslateIntoRawTokens(_codeInput);
        }

    }
}
