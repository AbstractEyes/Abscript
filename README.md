﻿
Abscript Basics;

Loosely and strongly typed system of abstractions built to support the development of a scripting language.

This language is built with a basic interpreter in C# with cross communication from C# to it.

Scripts are loaded from either a manifest using ABSCRIPT or in C# directly using the C# objects.

Design Basics;
The system is essentially designed to not expect anything by default.
All imported libraries are called directly by the main interpreter.
All objects are encapsulated in the ScriptList by default and only invoked when manually invoked.
Any unknown or generic objects are stored as "object" types in the C# system and must be typecast functionally.

/// Reserved words and their purposes;

	// ------------------------------------------------------------------------------------------
	in
		This is the "using" operator for Abscript. It imports C# libraries or Abscript libraries for use.
		It also accepts "using" as a standard.
	script
		This keyword imports a single use script library from the main ScriptList.
		If fed a file location or extension, it'll automatically seek within scope of the defined script folder.
	// ------------------------------------------------------------------------------------------
		in System;
		using System.Collections;
		script Abscript;
		script BaconScript.abp;
	
	// ------------------------------------------------------------------------------------------
	class
		// This is the standard strong type definition object. Abscript is meant to support both strong and loose type.
		// Anything marked with e is expected.
		// Anything encapsulated in [] is completely optional. 
		// Anything with * is a wildcard optional choice.
		// Anything in quotes is a direct identifier.
		*scope class *eType
			*scope eVariableIdentifier
			*scope eVariableIdentifier = *anything
			*scope var eVariableIdentifier = *anything
			*scope VarType eVariableIdentifier
			*scope VarType eVariableIdentifier = *anything
			
			// Func will always return the last return from their stack.
			// void returns are null
			// Mismatched type returns; throw TypeNotMatchException
			*scope func *identifier
			*scope func *Type *identifier
			*scope func *Type *identifier args
			*scope func *Type *identifier *ArgsType *args, ...
			*scope func *Type *identifier params *ArgsType[] *args

			// Act cannot have a return value.
			// Cannot return; throw VoidReturnException
			*scope act *identifier
			*scope act *identifier args
			*scope act *identifier *ArgsType *args, ...
			*scope act *identifier params *ArgsType[] *args

			// Any constructor can be used to create a new object.
			// The closest matching constructor will be called when using the new operator.
			// If no matching constructors or destructors are present; throw ConstructorMismatchException
			// These are automatically declared public unless otherwise declared.
			*scope (con /+)*identifier
			*scope (con /+)*identifier args
			*scope (con /+)*identifier *ArgsType *args, ...
			*scope (dis /~)*identifier
			*scope (dis /~)*identifier args
			*scope (dis /~)*identifier *ArgsType *args, ...
			
		*end
	// ------------------------------------------------------------------------------------------
	con(+), dis(-)
		A constructor and destructor can be defined within a class with a con keyword.
		The +Method keyword cannot be used outside of class declarations.
		Neither the constructor nor destructor need bodies.

		class ClassType
		
			+AnyConstructorMethod
			con AnyConstructorMethod
		
			~AnyDestructorMethod
			dis AnyDestructorMethod
		
		end
	// ------------------------------------------------------------------------------------------
	act
		This is the "action" operator for Abscript. It is used to define a void method with no return.
		
		// Act cannot have a return value.
		// Cannot return; throw VoidReturnException
		act *identifier
		act *identifier args
		act *identifier *ArgsType *args, ...
		act *identifier params *ArgsType[] *args
	// ------------------------------------------------------------------------------------------
	func
		This is the "function" operator for Abscript. It is used to define a method with a return value.
		With no type expectations and no return. 
			func name  params
				value
			end
		
		Conceptually, the return values can also be defined as so;
			func Type name params
				return value
			end
		// Func will always return the last return from their stack when ended.
		// void returns are null
		// Mismatched type returns; throw TypeNotMatchException
		func *identifier
		func *Type *identifier
		func *Type *identifier args
		func *Type *identifier *ArgsType *args, ...
		func *Type *identifier params *ArgsType[] *args
	// ------------------------------------------------------------------------------------------
	end
		This is the "end" operator for Abscript. It is used to end a function, act, or class definition.
		Everything within a scope must use the keyword "end" to end the scope.
		class ClassType
			Value
		
			act name params
				Value = params
			end
			
			func name params
				return Value
			end
		end 
		
		class ClassType3
			var Value;
		end
	end
	

// ------------------------------------------------------------------------------------------

// Scoping Parameters;

	// ------------------------------------------------------------------------------------------
	All default scoping for func, act, and variables are considered private by default.
	Thus eliminating the need for a private keyword.
	// ------------------------------------------------------------------------------------------
	pub
		The public scope identifier. Accessible by objects outside of the current scope.
	&
		The global scope identifier. The entire scope of the active program will have access to it's information.
	abc
		Exportation scope identifier. This can communicate directly back and forth with C#.
		
	/////////////////////////////////////////////////////////////////////////////////////////////
	seal
		// Define later