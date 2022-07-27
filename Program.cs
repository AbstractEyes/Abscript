// See https://aka.ms/new-console-template for more information
using Abscript.Parser;

Console.WriteLine("Hello, World!");

string Location = @"C:\Users\PHIL\source\repos\Abscript\Test\Abscript.abp";

Lexer tokenize = new Lexer(File.ReadAllText(Location));
