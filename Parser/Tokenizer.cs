using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Abscript.Parser
{
    internal class ScriptClass
    {
        
    }

    internal class ScriptTokens
    {

        // Generic class for classes.
        public List<object> Classes { get; set; } = new List<object>();
        // Delegates for methods.
        public List<string> Methods { get; set; } = new List<string>();
        // Delegate for functions
        public List<string> Functions { get; set; } = new List<string>();
        
        public ScriptTokens()
        {
            
        }

    }

    public class TypeToken
    {
        // We have to take in A DEFINED TOKEN TYPE
        // We use this defined token type, to determine what can be accepted as a followup token.
        public string Identifier { get; set; } = "";
        public string Expectations { get; set; } = Tokens.Eequals;

        public TypeToken(string input)
        {
            
        }
        
    }

    

    public static class Tokens
    {
        public static readonly string
            TypeIn              = "",
            Space               = " ",
            Comma               = ",",
            Quote               = "\"",
            
            Import              = "in",
            Eequals              = "=",
            Equality            = "==",
            ParenthesisOpen     = "(",
            ParanthesisClose    = ")",
            ClassOpen           = "class",
            MethodOpen          = "act",
            FunctionOpen        = "func",
            EveryEnd            = "end";

    }

    // This is a basic translation lexer to create readable script objects
    internal class Tokenizer
    {
        
        public ScriptTokens TokenizeScript(string _codeInput )
        {
            ScriptTokens tokenBase = new ScriptTokens();
            // Split the _codeInput into new lines for token check
            // Use linq to split based on the newline characters.
            string[] tokens = _codeInput
                .Split(new string[] { "\r\n", "\n" , " " }, StringSplitOptions.None);
            // Iterate through each for quotations.
            // Link each pair of quotations together in a token.


            //var Out = new Script();
            return tokenBase;
        }

        public Tokenizer(string _codeInput)
        {
            ScriptTokens tokens = TokenizeScript(_codeInput);
        }
        
    }
}
