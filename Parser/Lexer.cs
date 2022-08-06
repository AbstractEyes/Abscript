using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.ComponentModel;

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
        List<object> Types { get; set; } = new() { TokenSimple.NONE };
        public int ScopeDepth { get; set; } = 0;
        public string Identifier { get; set; } = "";

        public int StartPos { get; set; } = -1;

        public void AddType(object type)
        {
            if (!Types.Contains(type))
            {
                if(type is List<object> tList)
                {
                    foreach (var t in tList)
                    {
                        AddType(t);
                    }
                }
                else if(type is List<string> sList)
                {
                    foreach (var s in sList)
                    {
                        AddType(s);
                    }
                }
                else
                {
                    Types.Add(type);
                }
            }
        }

        public bool ContainsType(object type)
        {
            return Types.Contains(type);
        }

        public Token(int start, int depth)
        {
            StartPos = start;
            ScopeDepth = depth;
        }
        public Token()
        {
            
        }
    }


    /*
     * Token combinations;
     *      These are the combinations of tokens that can be found in a script.
     *      Every single one needs to have a series of patterns for matching and processing
     *      The individual patterns are defined in the TokenCombination class.
     *      The Tokens class houses a complete Directives dictionary of all of the patterns currently in use.
     */
    public enum TokenSuperCategory
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
    public class TokenDelimiterAttribute : Attribute
    {
        public List<string> Assignments { get; set; } = new List<string>();
        public TokenDelimiterAttribute(params string[] names)
        {
            Assignments = names.ToList();
        }
    }
    public static class EnumExtensions
    {

        // This extension method is broken out so you can use a similar pattern with 
        // other MetaData elements in the future. This is your base method for each.
        public static T? GetAttribute<T>(this Enum value) where T : Attribute
        {
            var type = value.GetType();
            var memberInfo = type.GetMember(value.ToString());
            var attributes = memberInfo[0].GetCustomAttributes(typeof(T), false);
            return attributes.Length > 0
              ? (T)attributes[0]
              : null;
        }

        // This method creates a specific call to the above method, requesting the
        // Description MetaData attribute.
        public static string ToName(this Enum value)
        {
            var attribute = value.GetAttribute<DescriptionAttribute>();
            return attribute == null ? value.ToString() : attribute.Description;
        }
        
        public static List<string> GetIdentifiers(this Enum value)
        {
            var attribute = value.GetAttribute<TokenDelimiterAttribute>();
            return attribute == null ? new List<string>() : attribute.Assignments;
        }

    }

    public enum TokenSimple
    {
        [TokenDelimiter("")]
        NONE,
        [TokenDelimiter("regex[a-zA-Z]")]
        ANY_LETTER,
        [TokenDelimiter("regex[0-9]")]
        ANY_NUMBER,
        //[TokenDelimiter(".", ",", "(", ")", "[", "]", 
        //    "{", "}", "`", "~", "$", "@", "#", "<", ">", 
        //    "=", "+", "-", "/", "*", "%", "^", "!", "&", "|",
        //    ";", "\n", "\r", "\t", "<=", ">=", "==", "!=", "&&", "||")]
        //ANY_OPERATOR,
        [TokenDelimiter("*")]
        WILDCARD,
        [TokenDelimiter(" ")]
        WHITESPACE,
        [TokenDelimiter("\n")]
        NEWLINE,
        [TokenDelimiter("\\")]
        ESCAPE_CHAR,
        [TokenDelimiter(".")]
        DOT, 
        [TokenDelimiter(",")]
        COMMA,
        [TokenDelimiter(":")]
        COLON,
        [TokenDelimiter(";")]
        SEMICOLON,
        [TokenDelimiter("=")]
        EQUAL,
        [TokenDelimiter("!=")]
        NOT_EQUAL,
        [TokenDelimiter("<")]
        LESS_THAN,
        [TokenDelimiter("<=")]
        LESS_THAN_EQUAL,
        [TokenDelimiter(">")]
        GREATER_THAN,
        [TokenDelimiter(">=")]
        GREATER_THAN_EQUAL,
        [TokenDelimiter("+")]
        PLUS,
        [TokenDelimiter("-")]
        MINUS,
        [TokenDelimiter("*")]
        MULTIPLY           ,
        [TokenDelimiter("/")]
        DIVIDE,
        [TokenDelimiter("%")]
        MODULUS,
        [TokenDelimiter("^")]
        EXPONENT           ,
        [TokenDelimiter("&")]
        AMPERSAND,
        [TokenDelimiter("|")]
        PIPE,
        [TokenDelimiter("^")]
        CARET,
        [TokenDelimiter("~")]
        TILDE              ,
        [TokenDelimiter("?")]
        QUESTION           ,
        [TokenDelimiter("!")]
        EXCLAMATION        ,
        [TokenDelimiter("\"")]
        DOUBLE_QUOTE       ,
        [TokenDelimiter("\'")]
        SINGLE_QUOTE       ,
        [TokenDelimiter("`")]
        BACKTICK           ,
        [TokenDelimiter("\\")]
        BACKSLASH          ,
        [TokenDelimiter("%")]
        PERCENT            ,
        [TokenDelimiter("$")]
        DOLLAR             ,
        [TokenDelimiter("#")]
        HASH               ,
        [TokenDelimiter("@")]
        AT                 ,
        [TokenDelimiter("&&")]
        AMPERSAND_AMPERSAND,
        [TokenDelimiter("||")]
        PIPE_PIPE,
        [TokenDelimiter("==")]
        EQUAL_EQUAL,
        [TokenDelimiter("(")]
        PARENTHESIS_OPEN,
        [TokenDelimiter(")")]
        PARENTHESIS_CLOSE,
        [TokenDelimiter("[")]
        BRACE_OPEN,
        [TokenDelimiter("]")]
        BRACE_CLOSE,
        [TokenDelimiter("{")]
        BRACKET_OPEN,
        [TokenDelimiter("}")]
        BRACKET_CLOSE,

        [TokenDelimiter("if")]
        IF,
        [TokenDelimiter("else")]
        ELSE,
        [TokenDelimiter("elseif", "elsif", "else if")]
        ELSEIF,
        [TokenDelimiter("while")]
        WHILE,
        [TokenDelimiter("for")]
        FOR,
        [TokenDelimiter("foreach")]
        FOREACH,
        [TokenDelimiter("do")]
        DO,
        [TokenDelimiter("break")]
        BREAK,
        [TokenDelimiter("continue")]
        CONTINUE,
        [TokenDelimiter("return")]
        RETURN,
        [TokenDelimiter("new")]
        NEW,
        [TokenDelimiter("delete")]
        DELETE,
        [TokenDelimiter("throw")]
        THROW,
        [TokenDelimiter("try")]
        TRY,
        [TokenDelimiter("catch")]
        CATCH,
        [TokenDelimiter("finally")]
        FINALLY,
        [TokenDelimiter("switch")]
        SWITCH,
        [TokenDelimiter("case")]
        CASE,
        [TokenDelimiter("default")]
        DEFAULT,
        [TokenDelimiter("null")]
        NULL,
        [TokenDelimiter("true")]
        TRUE,
        [TokenDelimiter("false")]
        FALSE,
        [TokenDelimiter("this")]
        THIS,
        [TokenDelimiter("super")]
        SUPER,
        [TokenDelimiter("extends")]
        EXTENDS,
        [TokenDelimiter("import")]
        IMPORT,
        [TokenDelimiter("package")]
        PACKAGE,
        [TokenDelimiter("static")]
        STATIC,
        [TokenDelimiter("public")]
        PUBLIC,
        [TokenDelimiter("private")]
        PRIVATE,
        [TokenDelimiter("protected")]
        PROTECTED,
        [TokenDelimiter("internal")]
        INTERNAL,
        [TokenDelimiter("abstract")]
        ABSTRACT,
        [TokenDelimiter("final")]
        FINAL,
        [TokenDelimiter("override")]
        OVERRIDE,
        [TokenDelimiter("synchronized")]
        SYNCHRONIZED,
        [TokenDelimiter("volatile")]
        VOLATILE,
        [TokenDelimiter("transient")]
        TRANSIENT,
        [TokenDelimiter("native")]
        NATIVE,
        [TokenDelimiter("strictfp")]
        STRICTFP,
        [TokenDelimiter("assert")]
        ASSERT,
        [TokenDelimiter("enum")]
        ENUM,
    }

    public enum TokenCombo
    {
        [TokenDelimiter("unknown")]
        UNKNOWN,
        [TokenDelimiter("//")]
        COMMENT_LINE,
        [TokenDelimiter("/*")]
        COMMENT_BLOCK_OPEN,
        [TokenDelimiter("*/")]
        COMMENT_BLOCK_CLOSE,
        [TokenDelimiter(@"regex[a-zA-Z_][a-zA-Z0-9_]*")]
        ANY_IDENTIFIER,
        [TokenDelimiter("class")]
        CLASS_BLOCK_OPEN,
        [TokenDelimiter("act")]
        ACTION_BLOCK_OPEN,
        [TokenDelimiter("func")]
        FUNCTION_BLOCK_OPEN,
        [TokenDelimiter("con", "regex~+[a-zA-Z0-9_]*")]
        CONSTRUCTOR_BLOCK_OPEN,
        [TokenDelimiter("des", "regex~-[a-zA-Z0-9_]*")]
        DESTRUCTOR_BLOCK_OPEN,
        [TokenDelimiter("end")]
        GENERAL_CLOSE,
        [TokenDelimiter("import", "script")]
        IMPORT_LINE,
        [TokenDelimiter("+", "-", "/", "*", "%", "^", "=", "<", ">", "!", "&", "|")]
        UNARY_OPERATOR,
        [TokenDelimiter(".", ",", "(", ")", "[", "]", "{", "}", "`", "~", "$", "@", "#")]
        PRIMARY_OPERATOR,
        [TokenDelimiter("\n", ";")]
        NEW_LINE,
        [TokenDelimiter("class", "act", "func", "con", "des", "import", "script")]
        WORD_OPERATOR,


    }

    public enum LexerOperation
    {
        NONE,               // Unknown pattern, does not exist.
        OPEN,               // Opens and increases the scope depth by 1
        CLOSE,              // Closes and decreases the scope depth by 1
        NULLIFY_TO,         // Nullifies all ahead of this until a comparator token is hit or the lexer state is changed.
        NORMALIZE,          // Normalizes and discontinues token nullification.
        MATCH_REGEX,        // Matches regex pattern until it hits a comparator token or the lexer state is changed.
        EXPECT,             // Expects one from a list of comparator tokens. If alternative is hit, the token is discarded.
        ENSUE,              // If the expected token is hit, it runs the ensue opcode.
    }

    // Only one LexerOperation can exist in a LexerCommand
    public class LexerCommandAttribute : Attribute
    {
        public LexerOperation Operation { get; private set; } = LexerOperation.NONE;
        public TokenSuperCategory Category { get; private set; } = TokenSuperCategory.UNKNOWN;
        List<object> ComparatorEndpointobj { get; set; } = new List<object>();
        public List<string> ComparatorEndpoint { get; private set; } = new List<string>();
        List<object> ComparatorInvalidobj { get; set; } = new List<object>();
        public List<string> ComparatorInvalid { get; private set; } = new List<string>();
        public string Value { get; private set; } = "";

        public LexerCommandAttribute SetValue(string value)
        {
            Value = value;
            return this;
        }
        public LexerCommandAttribute SetOperation(LexerOperation operation)
        {
            Operation = operation;
            return this;
        }
        public LexerCommandAttribute SetComparatorEndpoint(params object[] tokens)
        {
            ComparatorEndpointobj = tokens.ToList();
            PrepareComparatorEndpointStrings();
            return this;
        }
        public LexerCommandAttribute SetComparatorInvalid(params object[] tokens)
        {
            ComparatorInvalidobj = tokens.ToList();
            PrepareComparatorInvalidStrings();
            return this;
        }

        public List<object> GetComparatorEndpointObjects()
        {
            return ComparatorEndpointobj;
        }
        public List<object> GetComparatorInvalidObjects()
        {
            return ComparatorInvalidobj;
        }
        
        public List<string> GetComparatorEndpoint()
        {
            return ComparatorEndpoint;
        }
        public List<string> GetComparatorInvalid()
        {
            return ComparatorInvalid;
        }


        private void PrepareComparatorEndpointStrings()
        {
            if (ComparatorEndpoint.Count == 0)
            {
                List<string> list = new();
                foreach (var close in ComparatorEndpointobj)
                {
                    if (close is string c)
                    {
                        list.Add(c);
                    }
                    else if (close is TokenSimple s)
                    {
                        list.AddRange(s.GetIdentifiers());
                    }
                    else if (close is TokenCombo combo)
                    {
                        list.AddRange(combo.GetIdentifiers());
                    }
                }
                ComparatorEndpoint = list;
            }
        }
        private void PrepareComparatorInvalidStrings()
        {
            if (ComparatorInvalid.Count == 0)
            {
                List<string> list = new();
                foreach (var close in ComparatorInvalidobj)
                {
                    if (close is string c)
                    {
                        list.Add(c);
                    }
                    else if (close is TokenSimple s)
                    {
                        list.AddRange(s.GetIdentifiers());
                    }
                    else if (close is TokenCombo combo)
                    {
                        list.AddRange(combo.GetIdentifiers());
                    }
                }
                ComparatorInvalid = list;
            }
        }

        public bool IsEndpoint(params string[] tokens)
        {
            // determines if the whitelist contines any of the tokens
            return ComparatorEndpoint.Any(token => tokens.Contains(token));
        }
        public bool IsInvalid(params string[] tokens)
        {
            // determines if the whitelist contines any of the tokens
            return ComparatorInvalid.Any(token => tokens.Contains(token));
        }

    }
    
    public class LexerCombinationAttribute : Attribute
    {
        public List<LexerCommandAttribute> ComparatorPattern { get; private set; } = new List<LexerCommandAttribute>();

        public bool IsEndpoint(string token)
        {
            // determines if the whitelist contines any of the tokens
            return ComparatorPattern.Any(command => command.IsEndpoint(token));
        }
        public bool IsInvalid(string token)
        {
            // determines if the whitelist contines any of the tokens
            return ComparatorPattern.Any(command => command.IsInvalid(token));
        }

        public LexerCombinationAttribute SetComparatorPattern(params LexerCommandAttribute[] patternIn)
        {
            ComparatorPattern = patternIn.ToList();
            return this;
        }

    }

    public class TokenDirectiveAttribute : Attribute
    {
        public List<LexerCombinationAttribute> Combinations { get; private set; } = new();
        List<object> Identifiers { get; set; } = new();
        List<object> IdentifiersPrepared { get; set; } = new();
        public TokenCombo Type { get; private set; } = TokenCombo.UNKNOWN;
        public TokenDirectiveAttribute SetType(TokenCombo token)
        {
            Type = token;
            return this;
        }


        private Regex? MakeRegex(string str)
        {
            if (str.StartsWith("regex"))
            {
                return new Regex(str[5..]);
            }
            return null;
        }

        public bool IsIdentifier(string compareString)
        {
            // Returns if the identifiers list contains compareString
            foreach (var identifier in IdentifiersPrepared)
            {
                if (identifier is string str)
                {
                    if (str == compareString)
                    {
                        return true;
                    }
                }
                else if (identifier is Regex regex)
                {
                    if (regex.IsMatch(compareString))
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        public bool IsWhitelistMatch(string str)
        {
            foreach (var comparator in Combinations)
            {
                if (comparator.IsEndpoint(str))
                {
                    return true;
                }
            }
            return false;
        }
        public bool IsBlacklistMatch(string str)
        {
            foreach (var comparator in Combinations)
            {
                if (comparator.IsInvalid(str))
                {
                    return true;
                }
            }
            return false;
        }
        public List<object> GetIdentifiers()
        {   
            return IdentifiersPrepared;
        }

        private void PrepareIdentifiers()
        {
            if (IdentifiersPrepared.Count == 0)
            {
                IdentifiersPrepared = new();
                foreach (var identifier in Identifiers)
                {
                    var type = identifier.GetType().ToString();
                    if (identifier is string id)
                    {
                        var regex = MakeRegex(id);
                        if (regex != null)
                        {
                            IdentifiersPrepared.Add(regex);
                        }
                        else
                        {
                            IdentifiersPrepared.Add(id);
                        }
                    }
                    else if (identifier is Regex id2)
                    {
                        IdentifiersPrepared.Add(id2);
                    }
                    else if (identifier is TokenSimple id3)
                    {
                        var tokens = EnumExtensions.GetIdentifiers(id3);
                        foreach (var token in tokens)
                        {
                            var regex = MakeRegex(token);
                            if (regex != null) IdentifiersPrepared.Add(regex);
                            else IdentifiersPrepared.Add(token);
                        }
                    }
                    else if (identifier is TokenCombo id4)
                    {
                        var tokens = EnumExtensions.GetIdentifiers(id4);
                        foreach (var token in tokens)
                        {
                            var regex = MakeRegex(token);
                            if (regex != null) IdentifiersPrepared.Add(regex);
                            else IdentifiersPrepared.Add(token);
                        }

                    }
                }
            }
        }

        public TokenDirectiveAttribute SetIdentifiers(params object[]? match)
        {
            if(match != null)
            {
                foreach(object obj in match)
                {
                    if(obj is List<string> lst) Identifiers.AddRange(lst);
                    if(obj is List<object> lst2) Identifiers.AddRange(lst2);
                    else Identifiers.Add(obj);
                
                }
                PrepareIdentifiers();
            }
            return this;
        }
        public TokenDirectiveAttribute DefineCombinations(params LexerCombinationAttribute[] combinations)
        {
            Combinations = combinations.ToList();
            return this;
        }
    }
    public class TokenDirectiveContainerAttribute : Attribute
    {
        public List<TokenDirectiveAttribute> Directives { get; private set; } = new();
        public TokenCombo Type { get; private set; } = TokenCombo.UNKNOWN;

        public TokenDirectiveContainerAttribute SetDirectives(params TokenDirectiveAttribute[] identifiers)
        {
            Directives = identifiers.ToList();
            return this;
        }
        public TokenDirectiveContainerAttribute SetType(TokenCombo tokenType)
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

        public static readonly Dictionary<TokenSuperCategory, TokenDirectiveContainerAttribute>
            DirectiveContainer = new()
            {
                {
                    TokenSuperCategory.COMMENT,
                    new TokenDirectiveContainerAttribute()
                    .SetDirectives(
                        new TokenDirectiveAttribute()
                        .SetType(TokenCombo.COMMENT_LINE)
                        .SetIdentifiers(TokenCombo.COMMENT_LINE.GetIdentifiers())
                        .DefineCombinations(
                            new LexerCombinationAttribute()
                            .SetComparatorPattern(
                                new LexerCommandAttribute().SetOperation(LexerOperation.NULLIFY_TO)
                                .SetComparatorEndpoint(TokenSimple.NEWLINE)
                            )
                        ),
                        new TokenDirectiveAttribute()
                        .SetType(TokenCombo.COMMENT_BLOCK_OPEN)
                        .SetIdentifiers(TokenCombo.COMMENT_BLOCK_OPEN.GetIdentifiers())
                        .DefineCombinations(
                            new LexerCombinationAttribute()
                            .SetComparatorPattern(
                                new LexerCommandAttribute().SetOperation(LexerOperation.OPEN),
                                new LexerCommandAttribute().SetOperation(LexerOperation.NULLIFY_TO)
                                .SetComparatorEndpoint(TokenCombo.COMMENT_BLOCK_CLOSE)
                            )
                        ),
                        new TokenDirectiveAttribute()
                        .SetType(TokenCombo.COMMENT_BLOCK_CLOSE)
                        .SetIdentifiers(TokenCombo.COMMENT_BLOCK_CLOSE.GetIdentifiers())
                        .DefineCombinations(
                            new LexerCombinationAttribute()
                            .SetComparatorPattern(
                                new LexerCommandAttribute().SetOperation(LexerOperation.CLOSE),
                                new LexerCommandAttribute().SetOperation(LexerOperation.NORMALIZE)
                            )
                        )
                    )
                },
                {
                    TokenSuperCategory.IDENTIFIER,
                    new TokenDirectiveContainerAttribute().SetDirectives(
                        new TokenDirectiveAttribute()
                        .SetType(TokenCombo.ANY_IDENTIFIER)
                        // Set entry identifier of regex grabbing one return value
                        .SetIdentifiers(TokenCombo.ANY_IDENTIFIER.GetIdentifiers())
                        .DefineCombinations(
                            new LexerCombinationAttribute()
                            .SetComparatorPattern(
                                new LexerCommandAttribute()
                                .SetOperation(LexerOperation.MATCH_REGEX)
                                .SetComparatorEndpoint(
                                    TokenSimple.WHITESPACE,
                                    TokenSimple.NEWLINE,
                                    TokenCombo.PRIMARY_OPERATOR,
                                    TokenCombo.UNARY_OPERATOR
                                )
                            )
                        )
                    )
                }
            };
            

        public static TokenDirectiveContainerAttribute GetDirectives(TokenSuperCategory category)
        {
            return DirectiveContainer[category];
        }
        /*
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
        */

        /*
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
        */
        
    }

    class LexerState
    {
        public Token? CurrentToken { get; set; } = new Token();
        public LexerOperation CurrentOperation { get; set; } = LexerOperation.NONE;
        public int CurrentOperationPosition { get; set; } = 0;
        public int CurrentScope { get; set; } = 0;
    }


    // This is a basic translation lexer to create readable script objects
    internal class Lexer
    {

        LexerState State;

        bool PrepareTypeIdentifier(Token token, string rawScript)
        {
            for (int pos = token.StartPos; pos < rawScript.Length; pos++)
            {
                char curChar = rawScript[pos];
                
            }
            return true;
        }

        bool IdentifyToken(string rawToken)
        {
            // If a token matches, it needs to register the type and return true
            var directives = Tokens.DirectiveContainer; //token.StartPos, rawScript);
            foreach (var containerPair in directives)
            {
                // Attempt to match the current character with a directive.
                
                TokenSuperCategory category = containerPair.Key;
                foreach(var directive in containerPair.Value.Directives)
                {
                    // Compare the identifier with the currently captured token.
                    if(directive.IsIdentifier(rawToken))
                    {
                        
                        State.CurrentToken.AddType(directive.Type);
                        State.CurrentToken.Identifier = rawToken;
                        goto gohere;
                    }


                }

                //return PrepareTypeIdentifier(token, rawScript);
            }
            return false;
        gohere:
            return true;
        }

        bool LexerCommandActive(string comparatorToken)
        {
            return State.CurrentOperation != LexerOperation.NONE;
        }
        
        void TranslateIntoRawTokens(string rawScript)
        {
            // Replace newlines with spaces.
            int currentScope = 0;
            List<Token> list = new List<Token>();
            State.CurrentToken = new Token();
            string stringCombo = "";
            for (int pos = 0; pos < rawScript.Length; pos++)
            {
                if (State.CurrentToken == null) State.CurrentToken = new(pos, currentScope);
                char currentChar = rawScript[pos];
                stringCombo += currentChar;
                // Operates based on the lexer command after a comparator is found.
                if (LexerCommandActive(stringCombo))
                {
                    switch (State.CurrentOperation)
                    {
                        case LexerOperation.OPEN:
                            break;
                        case LexerOperation.CLOSE:
                            break;
                        case LexerOperation.NORMALIZE:
                            break;
                        case LexerOperation.NULLIFY_TO:
                            break;
                        case LexerOperation.ENSUE:
                            break;
                        case LexerOperation.MATCH_REGEX:
                            break;
                        case LexerOperation.EXPECT:
                            break;
                        default:
                            break;
                    }
                }
                // Continues to identify; need rewrite.
                if (IdentifyToken(stringCombo))
                {
                    pos += State.CurrentToken.Identifier.Length;
                    list.Add(State.CurrentToken);
                    State.CurrentToken = null;
                    stringCombo = "";
                }
                else
                {
                    State.CurrentToken.StartPos = pos;
                }
            }

        }

        void TranslateProcessedTokens(List<Token> tokens)
        {
            
        }

        public Lexer(string _codeInput)
        {
            // First pass
            State = new LexerState();
            TranslateIntoRawTokens(_codeInput);
        }

    }
}
