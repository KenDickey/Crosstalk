/* object.c -- UMB Scheme, object package.

UMB Scheme Interpreter  				$Revision: 3.2 $
Copyright (C) 1988, 1996 William R Campbell

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

UMB Scheme was written by Bill Campbell with help from Karl Berry,
Barbara Dixey, Ira Gerstein, Mary Glaser, Kathy Hargreaves, Bill McCabe,
Long Nguyen, Susan Quina, Jeyashree Sivasubram, Bela Sohoni, John Tam 
and Thang Quoc Tran.

For additional information about UMB Scheme, contact the author:

	Bill Campbell
	Department of Mathematics and Computer Science
	University of Massachusetts at Boston
	Harbor Campus
	Boston, MA 02125

	Telephone: 617-287-6449		Internet: bill@cs.umb.edu

*/

#include "portable.h"
#include "eval.h"
#include "object.h"
#include "architecture.h"
#include "compiler.h"
#include "steering.h"
#include "io.h"
#include "primitive.h"
#include "number.h"

#define	INDENT 2 

Private Integer Print_As_Formals();

/* Boolean. */

Op_Vector Boolean_Ops = { Self_Eval, Self_Compile, 
			Boolean_Print, Boolean_Print, Boolean_Print, Boolean_GC};

Scheme_Type Boolean_Type = &Boolean_Ops;

Object The_True_Object, The_False_Object;

Private void Init_Boolean()

{
	The_True_Object = Allocate(Boolean_Size);
	The_False_Object = Allocate(Boolean_Size);

	Get_Type(The_True_Object) = Boolean_Type;
	Get_Type_Name(The_True_Object) = "Boolean_Type/t";
	Get_Type(The_False_Object) = Boolean_Type;
	Get_Type_Name(The_False_Object) = "Boolean_Type/f";
}

Public Integer Boolean_Print(o,m)

	Object	o;
	Integer	m;
{
	if (o == The_True_Object)
		Output("#t");
	else if (o == The_False_Object)
		Output("#f");
	else
		Panic("Boolean_Print called on a non-boolean");
	return( m+2 );
}

Public Object Boolean_GC(old)

	Object old;
{
	Object new = Move_Object(old, Boolean_Size);

	return new;
}

/* Pair. */

Op_Vector Pair_Ops = {
	Self_Eval, Compile_Form, Pair_Display, Pair_Write, Pair_Show, Pair_GC};

Scheme_Type Pair_Type = &Pair_Ops;

/* Make_Pair Car Cdr, i.e., Car is Top(2), Cdr is Top(1). In
other words, push the car first, then the cdr. */
Public	void Make_Pair()
{
	Value_Register = Allocate(Pair_Size);
	Get_Pair_Car(Value_Register) = Top(2);
	Get_Pair_Cdr(Value_Register) = Top(1);

	Set_Result_Type(Pair_Type);
	Pop(2);
}


Public	Integer Pair_Write( o , m )

	Object	o;
	Integer	m;
{
	Output( "(" ); m += 1;
	
	m = Write_Object( Get_Pair_Car( o ) , m );
	o = Get_Pair_Cdr( o );

	while ( Is_Pair( o ) )
	{
		Output( " " );  m += 1;
		m = Write_Object( Get_Pair_Car( o ) , m );
		o = Get_Pair_Cdr( o );
	}

	if ( o == Nil )
	{
		Output( ")" ); m += 1;
	}
	else
	{
		Output( " . " ); m += 2;
		m = Write_Object( o , m );
		Output( ")" );  m += 1;
	}
	return( m );
}

Public	Integer Pair_Display( o , m )

	Object	o;
	Integer	m;
{
	Output( "(" ); m += 1;
	
	m = Display_Object( Get_Pair_Car( o ) , m );
	o = Get_Pair_Cdr( o );

	while ( Is_Pair( o ) )
	{
		Output( " " );  m += 1;
		m = Display_Object( Get_Pair_Car( o ) , m );
		o = Get_Pair_Cdr( o );
	}

	if ( o == Nil )
	{
		Output( ")" ); m += 1;
	}
	else
	{
		Output( " . " ); m += 2;
		m = Display_Object( o , m );
		Output( ")" );  m += 1;
	}
	return( m );
}


Public	Integer	Pair_Show( o, m )

	Object	o;
	Integer	m;
{
	Integer length = Length(o);

	if  (length > 3)
	{
		Output("("); m += 1;
		m = Write_Object( First(o) , m ) + 1; Output( " " );
		m = Write_Object( Second(o) , m ) + 1; Output( " " );
		m = Write_Object( Third(o) , m ) + 1; Output( " " );
		Output( "...)" ); m += 4;
	}
	else
	{
		Pair_Write( o , m );
	}

	return( m );
}

Public Object Pair_GC(old)

	Object old;
{
	Object new = Move_Object(old, Pair_Size);

	Relocate(&Get_Pair_Car(new));
	Relocate(&Get_Pair_Cdr(new));

	return new;
}


/* These internal routines are not directly callable from Lisp. We assume 
`list' is a list, but no more. */

Integer Length(list)

	Object list;
{
	Integer length = 0;

	while (Is_Pair(list))
	{
		list = Get_Pair_Cdr(list);
		length++;
	}

	return length;
}

Private Object Last_Cdr(list)
		
	Object list;
{
	while (Is_Pair(list))
	{
		list = Get_Pair_Cdr(list);
	}

	return list;
}

Public Object First(list)

	Object list;
{
	if (! Is_Pair(list))
	{
		Display_Error("Syntax error in First: list doesn't have a car",
		    list);
		return Nil;
	}

	return Get_Pair_Car(list);
}

Public Object Rest(list)
	
	Object list;
{
	if (! Is_Pair(list))
	{
		Display_Error("Syntax error in Rest: list doesn't have a cdr",
		    list);
		return Nil;
	}

	return Get_Pair_Cdr(list);
}

Object Second(list)

	Object list;
{
	if (!Is_Pair(list) || !Is_Pair(Get_Pair_Cdr(list)))
	{
	Display_Error(
		"Syntax error in second: list doesn't have two elements",
		 list);
		return Nil;
	}
	return Get_Pair_Car(Get_Pair_Cdr(list));
}

Object Third(list)

	Object list;
{
	if (!Is_Pair(list) || !Is_Pair(Get_Pair_Cdr(list))
	    || !Is_Pair(Get_Pair_Cdr(Get_Pair_Cdr(list))))
	{
		Display_Error(
		"Syntax error in third: list doesn't have three elements",
	         list);
		return Nil;
	}

	return Get_Pair_Car(Get_Pair_Cdr(Get_Pair_Cdr(list)));
}

Object Fourth(list)

	Object list;
{
	if (!Is_Pair(list)
	    || !Is_Pair(Get_Pair_Cdr(list))
	    || !Is_Pair(Get_Pair_Cdr(Get_Pair_Cdr(list)))
	    || !Is_Pair(Get_Pair_Cdr(Get_Pair_Cdr(Get_Pair_Cdr(list))))
	    )
	{
		Display_Error(
		"Syntax error in fourth: list doesn't have four elements",
	         list);
		return Nil;
	}

	return Get_Pair_Car(Get_Pair_Cdr(Get_Pair_Cdr(Get_Pair_Cdr(list))));
}

Public	Boolean	Member( item , list )

	Object	item , list;
{
	while ( Is_Pair( list ) )
	{
		if ( Get_Pair_Car( list ) == item ) return( TRUE );
		list = Get_Pair_Cdr( list );
	}
	return( FALSE );
}

/* Empty List. */

Op_Vector Empty_List_Ops = {
	Self_Eval, Compile_The_Empty_Object,
	Empty_List_Print, Empty_List_Print, Empty_List_Print,
	Empty_List_GC};

Scheme_Type Empty_List_Type = &Empty_List_Ops;

Public Object Nil;

Private void Init_Empty_List()

{
	Nil = Allocate(Empty_List_Size);

	Get_Type(Nil) = Empty_List_Type;
	Get_Type_Name(Nil) = "Empty_List_Type";
}

Public	Integer Empty_List_Print( o, m )

	Object	o;
	Integer m;
{
	Output("()");
	return( m + 2 );
}

Object Empty_List_GC(old)

	Object old;
{
	Object new = Move_Object(old, Empty_List_Size);

	return new;
}

/* Symbol */

Op_Vector Symbol_Ops = {
	Self_Eval, Compile_Symbol, Symbol_Print, Symbol_Print, Symbol_Print,
        Symbol_GC};

Scheme_Type Symbol_Type = &Symbol_Ops;

Public	Object	QUOTE_Symbol, DEFINE_Symbol, SET_Symbol, 
		IF_Symbol, DEFMACRO_Symbol,
		BEGIN_Symbol, DELAY_Symbol, LAMBDA_Symbol;

Public	Object	The_Syntactic_Keyword, The_Undefined_Symbol, An_Argument;

Private Object Special_Symbol( Representation )

	String	Representation;
{
	Object	new = Intern_Name( Representation );

	Get_Global_Binding( new ) = The_Syntactic_Keyword;
	return( new );
}


Private void Init_Symbol()
{
	Make_Symbol("<undefined symbol>");
	The_Undefined_Symbol = Value_Register;
	Get_Global_Binding(The_Undefined_Symbol) = The_Undefined_Symbol;

	Make_Symbol("<special symbol binding object>");
	The_Syntactic_Keyword = Value_Register;
	Get_Global_Binding(The_Syntactic_Keyword) = NULL;

	Make_Symbol( "<an argument>" );
	An_Argument = Value_Register;
	Get_Global_Binding( An_Argument ) = An_Argument;

	QUOTE_Symbol = Special_Symbol( "quote" );
	DEFINE_Symbol = Special_Symbol( "define" );
	SET_Symbol = Special_Symbol( "set!" );
	IF_Symbol = Special_Symbol( "if" );
	DEFMACRO_Symbol = Special_Symbol( "defmacro" );
	BEGIN_Symbol = Special_Symbol( "begin" );
	DELAY_Symbol = Special_Symbol( "delay" );
	LAMBDA_Symbol = Special_Symbol( "lambda" );

}

Public	void Make_Symbol(name)

	String name;
{
	String Copy_String();

	Value_Register = Allocate(Symbol_Size);

	Get_Symbol_Name(Value_Register) = Copy_String(name);
	Get_Property_List(Value_Register) = Nil;
	Get_Global_Binding(Value_Register) = The_Undefined_Symbol;
	Get_Symbol_How(Value_Register) = The_Undefined_Symbol;
	Get_Symbol_User_Defined(Value_Register) = FALSE;

	Set_Result_Type(Symbol_Type);
}

Public	Integer	Symbol_Print( o , m )  /* Assumes no nulls in name */

	Object	o;
	Integer	m;
{
	Output( Get_Symbol_Name(o) );
	return( m + strlen( Get_Symbol_Name(o) ) );
}

Public Object Symbol_GC(old)

	Object old;
{
	Object new = Move_Object(old, Symbol_Size);

	Relocate(&Get_Global_Binding(new));
	Relocate(&Get_Symbol_How(new));
	Relocate(&Get_Property_List(new));
	return new;
}

/* Numbers. */

Op_Vector Number_Ops = {
	Self_Eval, Self_Compile, Number_Print,
        Number_Print, Number_Print,  Number_GC};

Scheme_Type Number_Type = &Number_Ops;

Public void Make_Fixnum_Number(n)

	Short n;
{
	Value_Register = Allocate( Fixnum_Size );

	Get_Number_Tower_Position(Value_Register) = FIXNUM_LEVEL;
	Get_Number_Fixnum_Value(Value_Register) = n;
	Is_Exact_Number(Value_Register) = TRUE;

	Set_Result_Type(Number_Type);
}


Public	void Make_Bignum_Number(length)

	Integer length;
{
	Integer this_digit;

	Value_Register = Allocate(Bignum_Size(length));

	Get_Number_Tower_Position(Value_Register) = BIGNUM_LEVEL;
	Get_Number_Length(Value_Register) = length;
	Is_Exact_Number(Value_Register) = TRUE;

	for (this_digit = 0; this_digit < length; this_digit++)
	{
		Get_Number_Digits(Value_Register)[this_digit] = 0;
	}

	Set_Result_Type(Number_Type);
}

Public void Make_Rational_Number()
{
	Import 	void Reduce_Rational();
	Boolean	exact = Is_Exact_Number(Top(1)) && Is_Exact_Number(Top(2));

	/* Represent Numerator and Denominator by exacts */

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Top(1) );
		Number_Inexact_To_Exact(); Pop(1);
		Replace( 1 , Value_Register );
	}

	if ( ! Is_Exact_Number( Top(2) ) )
	{
		Push( Top(2) );
		Number_Inexact_To_Exact(); Pop(1);
		Replace( 2 , Value_Register );
	}

	/* Check for zero Denominator */

	Push( Top(1) );
	Is_Number_Zero(); 
	Pop(1);
	if ( Value_Register == The_True_Object )
	{
		Error( "Division by Zero" );
	}

	/* All rationals have a non-negative Denominator */
	
	Push( Top(1) );
	Is_Number_Negative();
	Pop(1);

	if (Value_Register == The_True_Object)
	{
		Push( Top( 1 ) );
		Number_Negate();
		Pop(1);
		Replace(1, Value_Register);
		Push(Top(2));
		Number_Negate();
		Pop(1);
		Replace(2, Value_Register);
	}
	
	Value_Register = Allocate(Rational_Size);

	Get_Number_Tower_Position(Value_Register) = RATIONAL_LEVEL;
	Get_Number_Rational_Numerator(Value_Register) = Top(2);
	Get_Number_Rational_Denominator(Value_Register) = Top(1);
	Is_Exact_Number(Value_Register) = exact;

	Set_Result_Type(Number_Type);
	Reduce_Rational();
}

Public void Make_Real_Number(r)

	Double r;
{
	Value_Register = Allocate(Real_Size);

	Get_Number_Tower_Position(Value_Register) = REAL_LEVEL;
	Get_Number_Real_Value(Value_Register) = r;
	Is_Exact_Number(Value_Register) = FALSE;

	Set_Result_Type(Number_Type);
}

Public void Make_Complex_Number(r,i)

	Double r,i;
{
	Value_Register = Allocate(Complex_Size);

	Get_Number_Tower_Position(Value_Register) = COMPLEX_LEVEL;
	Get_Number_Complex_Real_Part(Value_Register) = r;
	Get_Number_Complex_Imaginary_Part(Value_Register) = i;
	Is_Exact_Number(Value_Register) = FALSE;

	Set_Result_Type(Number_Type);
}


Public Integer	Number_Print(o, m)

	Object	o;
	Integer m;
{
	Push( Value_Register );				/* Save it */

	Push(o);
	Integer_To_Number( 10 );
	Push( Value_Register );
	Number_To_String(); Pop(2);
	Output(Get_String_Value(Value_Register));
	m += Get_String_Length( Value_Register );

	Value_Register = Top(1);			/* Restore it */
	Pop(1);

	return( m );
}

Public Object Number_GC(old)

	Object old;
{
	Object new;

	switch ( Get_Number_Tower_Position(old) )
	{
	    case FIXNUM_LEVEL:
		new = Move_Object(old, Fixnum_Size);
		break;
	    case BIGNUM_LEVEL:
		new = Move_Object(old, Bignum_Size(Get_Number_Length(old)));
		break;
	    case RATIONAL_LEVEL:
		new = Move_Object(old, Rational_Size);
		Relocate( &Get_Number_Rational_Numerator(new) );
		Relocate( &Get_Number_Rational_Denominator(new) );
		break;
	    case REAL_LEVEL:
		new = Move_Object(old, Real_Size);
		break;
	    case COMPLEX_LEVEL:
		new = Move_Object(old, Complex_Size);
		break;
	    default:
		new = Nil ;
		Panic("I'm trying to garbage collect an unimplemented number");
	}

	return new;
}


/* Character. */

Op_Vector Character_Ops = { Self_Eval, Self_Compile,
	Character_Display, Character_Write, Character_Write, Character_GC};

Scheme_Type Character_Type = &Character_Ops;

Public void Make_Character(c)

	Character c;
{
	Value_Register = Allocate(Character_Size);
	Get_Character_Value(Value_Register) = c;

	Set_Result_Type(Character_Type);
}

Public Integer Character_Write( o, m )

	Object	o;
	Integer	m;
{
	Output("#\\"); m += 2;

	if (Get_Character_Value(o) == ' ')
	{
		Output("space"); m += 5;
	}
	else if (Get_Character_Value(o) == '\n')
	{
		Output("newline"); m += 7;
	}
	else
	{
		Output_Char(Get_Character_Value(o)); m += 1;
	}

	Output(" "); m += 1;

	return( m );
}

Public Integer Character_Display( o, m )

	Object	o;
	Integer	m;
{
	Output_Char(Get_Character_Value(o));
	return( m + 1 );
}

Public Object Character_GC(old)

	Object old;
{
	Object new = Move_Object(old, Character_Size);

	return new;
}

/* String. */

Op_Vector String_Ops = {
	Self_Eval, Self_Compile, String_Display,
        String_Write, String_Write, String_GC};

Scheme_Type String_Type = &String_Ops;

Public void Make_String(l)

	Integer l;
{
	Value_Register = Allocate(String_Size(l+1));
	Get_String_Length(Value_Register) = l;

	Set_Result_Type(String_Type);
}

Public void Make_Constant_String(s)

	String s;
{
	Value_Register = Allocate(String_Size(strlen(s)+1));
	Get_String_Length(Value_Register) = strlen(s);
	strcpy(Get_String_Value(Value_Register),s);

	Set_Result_Type(String_Type);
}

Public String Copy_String(str)

	String str;
{
	String answer = (String) malloc(strlen(str) + 1);

	if (answer == NULL)
		Panic("I ran out of memory in Copy_String");

	strcpy(answer, str);
	return answer;
}

Public Integer String_Write( o, m )

	Object	o;
	Integer	m;
{
	Integer this_char;

	Output("\""); m += 1;

	for (this_char = 0; this_char < Get_String_Length(o); this_char++)
	{
		if (Get_String_Value(o)[this_char] == '\"')
		{
			Output("\\\""); m += 2;
		}
		else if (Get_String_Value(o)[this_char] == '\\')
		{
			Output("\\\\"); m += 2;
		}
		else
		{
			Output_Char(Get_String_Value(o)[this_char]); m += 1;
		}
	}

	Output("\""); m += 1;
	return( m );
}

Public	Integer	String_Display( o , m )

	Object	o;
	Integer	m;
{
	Integer this_char;

	for (this_char = 0; this_char < Get_String_Length(o); this_char++)
	{
		Output_Char(Get_String_Value(o)[this_char]); 
	}
	return( m + Get_String_Length(o)  );
}

Public Object String_GC(old)

	Object old;
{
	Object new = Move_Object(old, String_Size(Get_String_Length(old)));

	return new;
}


/* Vector. */

Op_Vector Vector_Ops = {
	Self_Eval, Self_Compile, Vector_Display,
        Vector_Write, Vector_Show,  Vector_GC};

Scheme_Type Vector_Type = &Vector_Ops;

Public void Make_Vector(length)

	Integer length;
{
	Value_Register = Allocate(Vector_Size(length));
	Get_Vector_Length(Value_Register) = length;

	while (length--)
	{
		Get_Vector_Elem(Value_Register, length) = The_Undefined_Symbol;
	}

	Set_Result_Type(Vector_Type);
}

Public	Integer	Vector_Display( o, m )

	Object	o;
	Integer	m;
{
	Integer this_element;

	Output("#("); m += 3;

	for (this_element = 0; this_element < Get_Vector_Length(o); 
	     this_element++)
	{
		if ( this_element )
		{
			Output(" "); m += 1;
		}
		m = Display_Object( Get_Vector_Elem(o, this_element) , m );
	}

	Output(")"); m += 1;
	return( m );
}

Public	Integer	Vector_Write( o, m )

	Object	o;
	Integer	m;
{
	Integer this_element;

	Output("#("); m += 3;

	for (this_element = 0; this_element < Get_Vector_Length(o); 
	     this_element++)
	{
		if ( this_element )
		{
			Output(" "); m += 1;
		}
		m = Write_Object( Get_Vector_Elem(o, this_element) , m );
	}

	Output(")"); m += 1;
	return( m );
}


Public	Integer	Vector_Show( o, m )

	Object	o;
	Integer	m;
{
	Integer this_element;
	Integer length = Get_Vector_Length(o);

	Output("#("); m += 2;

	if  (length > 3)
	{
		m = Write_Object( Get_Vector_Elem(o,0) , m ) + 1; Output( " " );
		m = Write_Object( Get_Vector_Elem(o,1) , m ) + 1; Output( " " );
		m = Write_Object( Get_Vector_Elem(o,2) , m ) + 1; Output( " " );
		Output( "...)" ); m += 4;
	}
	else
	{
		for (this_element = 0; this_element < length; this_element++)
		{
			m = Write_Object(Get_Vector_Elem(o,this_element),m) + 1;
			Output( this_element == (length-1) ? ")" : " " );
		}
	}

	return( m );
}


Public Object Vector_GC(old)

	Object old;
{
	Object new = Move_Object(old, Vector_Size(Get_Vector_Length(old)));
	Integer this_element;

	for (this_element = 0; this_element < Get_Vector_Length(old); 
	     this_element++)
	{
		Relocate(&Get_Vector_Elem(new,this_element));
	}

	return new;
}


/* Procedure. */

Op_Vector Procedure_Ops = {
	Self_Eval, Self_Compile,
        Procedure_Print, Procedure_Print, Procedure_Show, Procedure_GC};

Scheme_Type Procedure_Type = &Procedure_Ops;

Public void Make_Procedure()
{
	Object lambda;

	Value_Register = Allocate(Procedure_Size);

	lambda = Top(1);
	Get_Procedure_Name(Value_Register) = "<Anonymous>";
	Get_Procedure_Numargs(Value_Register) = Get_Lambda_Numargs(lambda);
	Get_Procedure_Tracing(Value_Register) = FALSE;
	Get_Procedure_Has_Rest(Value_Register) =
	    Get_Lambda_Has_Rest(lambda);
	Get_Procedure_Body(Value_Register) = Get_Lambda_Body(lambda);
	Get_Procedure_Frame(Value_Register) = Get_Lambda_Frame(lambda);
	Get_Procedure_Environment(Value_Register) = Environment_Register;

	Set_Result_Type(Procedure_Type);
	Pop(1);
}

Public	Integer	Procedure_Print( o, m )

	Object	o;
	Integer	m;
{
	Integer in_m = m;

	Output( "(lambda " ); m +=  8;
	m = Print_As_Formals( Get_Procedure_Frame(o) , m );

	m = Write_Object( Get_Procedure_Body(o) , in_m ); /* Prints `)' */

	return( m );
}



Public	Integer	Procedure_Show( o , m )

	Object	o;
	Integer	m;
{
	Output("(lambda "); m += 8;
	m = Print_As_Formals( Get_Procedure_Frame(o) , m );
	Output("  ...)"); m += 5;
	return( m );
}

Public Object Procedure_GC(old)

	Object old;
{
	Object new = Move_Object(old, Procedure_Size);

	Relocate(&Get_Procedure_Body(new));
	Relocate(&Get_Procedure_Environment(new));
	Relocate(&Get_Procedure_Frame(new));

	return new;

}
 
/* Primitive. (Scheme procedures implemented in C.) */

Op_Vector Primitive_Ops = {
	Self_Eval, Self_Compile, Primitive_Print,
        Primitive_Print, Primitive_Print,  Primitive_GC};

Scheme_Type Primitive_Type = &Primitive_Ops;

Public void Make_Primitive(name, proc, arg_count, arg_type1, 
			  arg_type2, arg_type3)
	String name;
	void (*proc)();
	Integer arg_count;
	Scheme_Type arg_type1, arg_type2, arg_type3;
{
	Object Interned_Symbol;

	Value_Register = Allocate(Primitive_Size(arg_count));
	Get_Primitive_Name(Value_Register) = name;
	Get_Primitive_Procedure(Value_Register) = proc;
	Get_Primitive_Numargs(Value_Register) = arg_count;
	Get_Primitive_Tracing(Value_Register) = FALSE;

	switch (arg_count)
	{
	case 3:
		Get_Primitive_Argtypes(Value_Register,2) = arg_type3;
	case 2:
		Get_Primitive_Argtypes(Value_Register,1) = arg_type2;
	case 1:
	case VARYING:
		Get_Primitive_Argtypes(Value_Register,0) = arg_type1;
		break;
	case 0:
		break;
	default:
		Panic("I thought all primitives had fewer than four types");
	}

	Set_Result_Type(Primitive_Type);
	Push( Value_Register );  /* Save Primitive */

	/* Put the name in our hash table. */
	Interned_Symbol = Intern_Name(name);

	Value_Register = Top(1);
	Pop(1);		    /* Restore Primitive from stack */
	/* Put the thing we just made in the global environment. */
	Get_Global_Binding( Interned_Symbol ) = Value_Register ;
}

Public	Integer	Primitive_Print( o , m )

	Object	o;
	Integer	m;
{
	Integer this_type;

	Output("< primitive: ");

	Output(Get_Primitive_Name(o));
	Output(" ");
	if ( Get_Primitive_Numargs(o) == VARYING )
	{
		Print_Type(Get_Primitive_Argtypes(o,0));
		Output( "... " );
	}
	else
	{
		for (this_type=0; this_type<Get_Primitive_Numargs(o); 
			this_type++)
		{
			Print_Type(Get_Primitive_Argtypes(o,this_type));
			Output(" ");
		}
	}

	Output(">");
	return( 0 );
}

Public Object Primitive_GC(old)

	Object old;
{
	Object new = Move_Object(old, Primitive_Size(Get_Primitive_Numargs(old)));

	return new;
}

/* Continuation. */

Op_Vector Continuation_Ops = {
	Self_Eval, Self_Compile,
        Continuation_Print, Continuation_Print, Continuation_Print,
        Continuation_GC};

Scheme_Type Continuation_Type = &Continuation_Ops;

Public void Make_Continuation()
{
	Integer this_element;
	Value_Register = Allocate(Continuation_Size(Arg_Stack_Ptr));

	Get_Continuation_State(Value_Register) = State_Register;
	Get_Continuation_Stacksize(Value_Register) = Arg_Stack_Ptr;

	for (this_element = 0; this_element < Arg_Stack_Ptr; this_element++)
	{
		Get_Continuation_Stack_Elem(Value_Register,this_element) =
		    Arg_Stack[this_element];
	}

	Set_Result_Type(Continuation_Type);
}

Public	Integer	Continuation_Print( o , m )

	Object	o;
	Integer	m;
{
	Output("<continuation>");
	return( m + 14 );
}

Public Object Continuation_GC(old)

	Object old;
{
	Integer this_element;
	Object new = Move_Object(old, 
	    Continuation_Size(Get_Continuation_Stacksize(old)));

	Relocate(&Get_Continuation_State(new));
	for (this_element = 0; this_element < Get_Continuation_Stacksize(new);
	    this_element++)
	{
		Relocate(&Get_Continuation_Stack_Elem(new,this_element));
	}

	return new;
}

/* Port. */

Op_Vector Port_Ops = { Self_Eval, Self_Compile, 
			Port_Print, Port_Print, Port_Print, Port_GC};

Scheme_Type Port_Type = &Port_Ops;

Public Object Current_Input_Port, Current_Output_Port, Current_Error_Port,
              The_Transcript_Port;

Private void Init_Port()
{
	The_Standard_Input = stdin;
	The_Standard_Output = stdout;
	The_Standard_Error = stderr;

	Make_Port(TRUE, The_Standard_Input, "stdin");
	Current_Input_Port = Value_Register;

	Make_Port(FALSE, The_Standard_Output, "stdout");
	Current_Output_Port = Value_Register;

	Make_Port(FALSE, The_Standard_Error, "stdout");
	Current_Error_Port = Value_Register;

	The_Transcript_Port = Nil;
}

Public void Make_Port(is_input, file, filename)

	Boolean is_input;
	FILE *file;
	String filename;
{
	Value_Register = Allocate(Port_Size);
	Is_Input_Port(Value_Register) = is_input;
	Get_Port_File(Value_Register) = file;
	Get_Port_Name(Value_Register) = Copy_String(filename);

	Set_Result_Type(Port_Type);
}

Public	Integer	Port_Print( o, m )

	Object	o;
	Integer	m;
{
	if (Is_Input_Port(o))
	{
		Output("<input port connected to `"); m += 26;
	}
	else
	{
		Output("<output port connected to `"); m += 27;
	}

	if (Get_Port_Name(o) != NULL)
	{
		Output( Get_Port_Name(o) ); m += strlen( Get_Port_Name(o) );
	}

	Output("'>"); m += 2;
	return( m );
}

Public Object Port_GC(old)

	Object old;
{
	Object new = Move_Object(old, Port_Size);

	return new;
}

/* End-of-file. */

Op_Vector Eof_Ops = {
	Self_Eval, Self_Compile, Eof_Display, Eof_Write, Eof_Display,  Eof_GC};

Scheme_Type Eof_Type = &Eof_Ops;

Public Object The_Eof_Object;

Private void Init_Eof()
{
	The_Eof_Object = Allocate(Eof_Size);

	Get_Type(The_Eof_Object) = Eof_Type;
	Get_Type_Name(The_Eof_Object) = "Eof_Type";
}

Public	Integer	Eof_Write( o, m )

	Object	o;
	Integer	m;
{
	Output_Char('\004');
	return( m + 1 );
}

Public	Integer	Eof_Display( o , m )

	Object	o;
	Integer	m;
{
	Output("<eof>");
	return( m + 5 );
}

Public	Object	Eof_GC( old )

	Object old;
{
	Object new = Move_Object( old, Eof_Size );

	return new;
}

/* Variable  (lexically addressed by frame and displacement) */

Op_Vector Variable_Ops = {
	Variable_Eval, Self_Compile, Variable_Print,
        Variable_Print, Variable_Print, Variable_GC};

Scheme_Type Variable_Type = &Variable_Ops;

Public void Make_Local_Variable(symbol, frame, displacement)

	Object	symbol;
	Integer frame;
	Integer displacement;
{
	Push( symbol );

	Value_Register = Allocate(Variable_Size);

	if (! Is_Symbol( Top(1) ) )
		Panic("Expected a symbol in Make_Variable_Local");

	Is_Local_Variable(Value_Register) = TRUE;
	Get_Variable_Frame_Number(Value_Register) = frame;
	Get_Variable_Displacement(Value_Register) = displacement;
	Get_Variable_Symbol(Value_Register) = Top(1) ;
	Pop(1);

	Set_Result_Type(Variable_Type);
}



Public void Make_Global_Variable( symbol )

	Object	symbol;
{
	Push( symbol );

	Value_Register = Allocate(Variable_Size);

	if (! Is_Symbol( Top(1) ) )
		Panic("Expected a symbol in Make_Variable_Global");

	Is_Local_Variable(Value_Register) = FALSE;
	Get_Variable_Symbol(Value_Register) = Top(1);
	Pop(1);

	Set_Result_Type(Variable_Type);
}

Public	Integer	Variable_Print( o, m )

	Object	o;
	Integer	m;
{
	return( Symbol_Print( Get_Variable_Symbol(o), m ) );
}

Public Object Variable_GC(old)

	Object old;
{
	Object new = Move_Object(old, Variable_Size);

	Relocate(&Get_Variable_Symbol(new));

	return new;
}

/* Apply. */

Op_Vector Apply_Ops = {
	Apply_Eval, Self_Compile, Apply_Print, Apply_Print,
	Apply_Print,  Apply_GC};

Scheme_Type Apply_Type = &Apply_Ops;

Public void Make_Apply()
{
	Value_Register = Allocate(Apply_Size);
	Get_Apply_Numargs(Value_Register) = Length(Top(1));
	Get_Apply_Arguments(Value_Register) = Top(1);
	Get_Apply_Operator(Value_Register) = Top(2);

	Set_Result_Type(Apply_Type);
	Pop(2);
}

Public	Integer	Apply_Print( o , m )

	Object	o;
	Integer	m;
{
	Output("("); m += 1;

	m = Write_Object( Get_Apply_Operator(o), m );

	o = Get_Apply_Arguments(o);

	while ( Is_Pair( o ) )
	{
		Output( " " ); m += 1;
		m = Write_Object( Get_Pair_Car( o ) , m );
		o = Get_Pair_Cdr( o );
	}
	Output( ")" ); m += 1;

	return( m );
}

Public Object Apply_GC(old)

	Object old;
{
	Object new = Move_Object(old, Apply_Size);

	Relocate(&Get_Apply_Operator(new));
	Relocate(&Get_Apply_Arguments(new));

	return new;
}

/* Lambda  */

Op_Vector Lambda_Ops = {
	Lambda_Eval, Self_Compile, Lambda_Print,
        Lambda_Print, Lambda_Print,  Lambda_GC};

Scheme_Type Lambda_Type = &Lambda_Ops;

#define Save_Lambda_Objects() \
	{Push(formals); Push(body); Push(rest); Push(frame);}

#define Restore_Lambda_Objects() \
	{ frame = Top(1); rest = Top(2); body = Top(3); formals = Top(4); \
          Pop(4); }


Public void Make_Lambda()
{
	Object frame;
	Object body;

	Value_Register = Allocate(Lambda_Size);
	frame = Top(2);
	body = Top(1);
	Get_Lambda_Numargs(Value_Register) = 
		Get_Environment_Frame_Size(frame) -
			(Get_Environment_Frame_Has_Rest(frame) ? 1 : 0);
	Get_Lambda_Has_Rest(Value_Register) =
		Get_Environment_Frame_Has_Rest(frame);
	Get_Lambda_Frame(Value_Register) = frame;
	Get_Lambda_Body(Value_Register) = body;

	Set_Result_Type(Lambda_Type);
	Pop(2);
}

Public	Integer	Lambda_Print( o, m )

	Object	o;
	Integer	m;
{
	Integer in_m = m;

	Output( "(lambda " ); m +=  8;
	m = Print_As_Formals( Get_Lambda_Frame(o) , m );

	m = Write_Object( Get_Lambda_Body(o) , in_m ); /* Prints closing `)' */

	return( m );
}

Public Object Lambda_GC(old)

	Object old;
{
	Object new = Move_Object(old, Lambda_Size);

	Relocate(&Get_Lambda_Frame(new));
	Relocate(&Get_Lambda_Body(new));

	return new;
}


/* Conditional. */

Op_Vector Conditional_Ops = {
	Conditional_Eval, Self_Compile,
        Conditional_Print, Conditional_Print, Conditional_Print,
        Conditional_GC};

Scheme_Type Conditional_Type = &Conditional_Ops;

/* make-conditional test consequent alternate. */
Public void Make_Conditional()
{
	Value_Register = Allocate(Conditional_Size);
	Get_Conditional_Test(Value_Register) = Top(3);
	Get_Conditional_Consequent(Value_Register) = Top(2);
	Get_Conditional_Alternate(Value_Register) = Top(1);

	Set_Result_Type(Conditional_Type);
	Pop(3);
}

Public	Integer	Conditional_Print( o , m )

	Object	o;
	Integer	m;
{
	Integer	in_m = m;

	Output("(if "); m += 4;

	m = Write_Object( Get_Conditional_Test(o) , m );

	m = New_Left_Margin( in_m + INDENT );
	m = Write_Object( Get_Conditional_Consequent(o), m );

	m = New_Left_Margin( in_m + INDENT );
	m = Write_Object( Get_Conditional_Alternate(o) , m );

	Output(")"); m += 1;
	
	return( m );
}

Public Object Conditional_GC(old)

	Object old;
{
	Object new = Move_Object(old, Conditional_Size);

	Relocate(&Get_Conditional_Test(new));
	Relocate(&Get_Conditional_Consequent(new));
	Relocate(&Get_Conditional_Alternate(new));

	return new;
}


/* Assignment */

Op_Vector Assignment_Ops = {
	Assignment_Eval, Self_Compile,
        Assignment_Print, Assignment_Print, Assignment_Print,
        Assignment_GC};

Scheme_Type Assignment_Type = &Assignment_Ops;

Public void Make_Assignment()
{
	Value_Register = Allocate(Assignment_Size);
	Get_Assignment_Lvalue(Value_Register) = Top(2);
	Get_Assignment_Rvalue(Value_Register) = Top(1);

	Set_Result_Type(Assignment_Type);
	Pop(2);
}

Public	Integer	Assignment_Print( o, m )

	Object	o;
	Integer	m;
{
	Output( "(set! " ); m += 6;

	m = Write_Object( Get_Assignment_Lvalue(o), m );

	Output(" "); m += 1;

	m = Write_Object( Get_Assignment_Rvalue(o) , m );

	Output( ")" ); m += 1;
	return( m );
}

Public Object Assignment_GC(old)

	Object old;
{
	Object new = Move_Object(old, Assignment_Size);

	Relocate(&Get_Assignment_Lvalue(new));
	Relocate(&Get_Assignment_Rvalue(new));

	return new;
}


/* Definition  */

Op_Vector Definition_Ops = {
	Definition_Eval, Self_Compile,
        Definition_Print, Definition_Print, Definition_Print,
        Definition_GC};

Scheme_Type Definition_Type = &Definition_Ops;

Public void Make_Definition()
{
	Value_Register = Allocate(Definition_Size);
	Get_Definition_Lvalue(Value_Register) = Top(2);
	Get_Definition_Rvalue(Value_Register) = Top(1);

	Set_Result_Type(Definition_Type);
	Pop(2);
}

Public	Integer	Definition_Print( o , m )

	Object	o;
	Integer	m;
{
	Output( "(define " ); m += 8;

	m = Write_Object( Get_Definition_Lvalue( o ) , m );

	Output( " " ); m += 1;

	m = Write_Object( Get_Definition_Rvalue( o ) , m );

	Output( ")" ); m += 1;
	return( m );
}

Public Object Definition_GC(old)

	Object old;
{
	Object new = Move_Object(old, Definition_Size);

	Relocate(&Get_Definition_Lvalue(new));
	Relocate(&Get_Definition_Rvalue(new));

	return new;
}

/* Defmacro Definition Forms */

Op_Vector Defmacro_Ops = {
	Self_Eval, Self_Compile, Defmacro_Print, Defmacro_Print, Defmacro_Show,
        Defmacro_GC};

Scheme_Type Defmacro_Type = &Defmacro_Ops;

Public void Make_Defmacro()
{
	Value_Register = Allocate(Defmacro_Size);
	Get_Defmacro_Keyword(Value_Register) = Top(2);
	Get_Defmacro_Transformer(Value_Register) = Top(1);

	Set_Result_Type(Defmacro_Type);
	Pop(2);
}

Public	Integer	Defmacro_Print( o , m )

	Object	o;
	Integer	m;
{
	Integer	in_m = m;

	Output( "(defmacro " ); m += 7;

	m = Write_Object( Get_Defmacro_Keyword( o ) , m );

	if ( m >= 20 )
	{ 
		m = New_Left_Margin( in_m + INDENT );
	}
	else
	{
		Output( " " ); m += 1;
	}

	m = Write_Object( Get_Defmacro_Transformer( o ) , m );


	Output( ")" ); m += 1;
	return( m );
}

Public	Integer	Defmacro_Show( o , m )

	Object	o;
	Integer	m;
{
	Output( "(defmacro " ); m += 7;

	m = Write_Object( Get_Defmacro_Keyword( o ) , m );

	Output( " ...)" ); m += 5;

	return( m );
}


Public Object Defmacro_GC(old)

	Object old;
{
	Object new = Move_Object(old, Defmacro_Size);

	Relocate(&Get_Defmacro_Keyword(new));
	Relocate(&Get_Defmacro_Transformer(new));
	return new;
}

/* Defmacro Call Forms. */

Op_Vector Defmacro_Call_Ops = {
	Defmacro_Call_Eval, Self_Compile,
        Defmacro_Call_Print, Defmacro_Call_Print, Defmacro_Call_Print,
        Defmacro_Call_GC};

Scheme_Type Defmacro_Call_Type = &Defmacro_Call_Ops;

Public void Make_Defmacro_Call()
{
	Value_Register = Allocate(Defmacro_Call_Size);
	Get_Defmacro_Call_Original(Value_Register) = Top(2);
	Get_Defmacro_Call_Expansion(Value_Register) = Top(1);

	Set_Result_Type(Defmacro_Call_Type);
	Pop(2);
}

Public	Integer	Defmacro_Call_Print( o , m )

	Object	o;
	Integer m;
{
	Integer	in_m = m;

	m = Write_Object( Get_Defmacro_Call_Original( o ) , m );

	m = New_Left_Margin( in_m );
	return( m );
}

Public Object Defmacro_Call_GC(old)

	Object old;
{
	Object new = Move_Object(old, Defmacro_Call_Size);

	Relocate(&Get_Defmacro_Call_Original(new));
	Relocate(&Get_Defmacro_Call_Expansion(new));

	return new;
}


/* Sequence. */

Op_Vector Sequence_Ops = {
	Sequence_Eval, Self_Compile, Sequence_Print,
        Sequence_Print, Sequence_Print, Sequence_GC};

Scheme_Type Sequence_Type = &Sequence_Ops;

Public void Make_Sequence(from_begin)

	Boolean from_begin;

{
	Value_Register = Allocate(Sequence_Size);
	Get_Sequence_Clauses(Value_Register) = Top(1);
	Get_Sequence_From_Begin(Value_Register) = from_begin;

	Set_Result_Type(Sequence_Type);
	Pop(1);
}

Public	Integer	Sequence_Print( o, m )

	Object	o;
	Integer m;
{
	Integer	in_m = m;

	if ( Get_Sequence_From_Begin( o ) )
	{
		Output("(begin ");  m += 6;
	} 

	o = Get_Sequence_Clauses( o );
	while ( o != Nil )
	{
		m = New_Left_Margin( in_m + INDENT );
		m = Write_Object( Get_Pair_Car( o ) , m );
		o = Get_Pair_Cdr( o );
	}
	Output( " )" ); m += 2;
	return( m );
}

Public Object Sequence_GC( old )

	Object old;
{
	Object new = Move_Object(old, Sequence_Size);

	Relocate(&Get_Sequence_Clauses(new));

	return new;
}

/* Delay. */

Op_Vector Delay_Ops = {
	Delay_Eval, Self_Compile, Delay_Print,
        Delay_Print, Delay_Print,  Delay_GC};

Scheme_Type Delay_Type = &Delay_Ops;

Public void Make_Delay()
{
	Value_Register = Allocate(Delay_Size);
	Get_Delay_Expression(Value_Register) = Top(1);

	Set_Result_Type(Delay_Type);
	Pop(1);
}

Public	Integer	Delay_Print( o, m )

	Object	o;
	Integer	m;
{
	Integer in_m = m;

	Output("(delay ");
	m = New_Left_Margin( in_m + INDENT );

	m = Write_Object( Get_Delay_Expression( o ) , m );

	Output(")"); m += 1;

	return( m );
}

Public Object Delay_GC(old)

	Object old;
{
	Object new = Move_Object(old, Delay_Size);

	Relocate(&Get_Delay_Expression(new));

	return new;
}

/* Promise. */

Op_Vector Promise_Ops = {
	Self_Eval, Self_Compile, Promise_Print,
        Promise_Print, Promise_Show,  Promise_GC};

Scheme_Type Promise_Type = &Promise_Ops;

Public void Make_Promise()
{
	Value_Register = Allocate(Promise_Size);
	Get_Promise_Expression(Value_Register) = Top(2);
	Get_Promise_Environment(Value_Register) = Top(1);
	Get_Promise_Forced(Value_Register) = FALSE;
	
	Set_Result_Type(Promise_Type);
	Pop(2);
}

Public	Integer	Promise_Print( o, m )

	Object	o;
	Integer	m;
{
	Integer	in_m = m;

	Output( "(PROMISE " ); 
	m = New_Left_Margin( in_m + INDENT );

	m = Write_Object( Get_Promise_Expression( o ) , m );
	Output( ")" ); m +=1;
	return( m );
}

Public	Integer	Promise_Show( o, m )

	Object	o;
	Integer	m;
{
	Output( "<PROMISE>" ); 
	return( m + 9 );
}

Public Object Promise_GC(old)

	Object old;
{
	Object new = Move_Object(old, Promise_Size);

	Relocate(&Get_Promise_Expression(new));
	Relocate(&Get_Promise_Environment(new));

	return new;
}


/* Error forms. */

Op_Vector Error_Ops = {
	Self_Eval, Self_Compile, Error_Print,
        Error_Print, Error_Print, Error_GC};

Scheme_Type Error_Type = &Error_Ops;

Public void Make_Error(message)

	String message;
{
	Value_Register = Allocate(Error_Size);
	Get_Error_Message(Value_Register) = Copy_String(message);

	Set_Result_Type(Error_Type);
}

Public	Integer	Error_Print( o, m )

	Object	o;
	Integer	m;
{
	Output( "(Error " ); m += 7;
	Output( Get_Error_Message( o ) ); m += strlen( Get_Error_Message( o ) );
	Output( ")" ); m += 1;
	return( m );
}

Public Object Error_GC(old)

	Object old;
{
	Object new = Move_Object(old, Error_Size);

	return new;
}


/* Environment Frame. */

Op_Vector Environment_Frame_Ops = {
	Self_Eval, Self_Compile,
        Environment_Frame_Show,
        Environment_Frame_Show,
	Environment_Frame_Show,
        Environment_Frame_GC};

Scheme_Type Environment_Frame_Type = &Environment_Frame_Ops;

Public Object The_Global_Environment;

Private void Init_Environment_Frame()
{
	Make_Environment_Frame(0, FALSE); 
	The_Global_Environment = Value_Register;
}

Public void Make_Environment_Frame(size, has_rest)
        Integer size;
        Boolean has_rest;
{
        Integer newsize = ( size ? size : 1);
        Integer i;

        Value_Register = Allocate(Environment_Frame_Size(newsize));
        Get_Environment_Frame_Previous(Value_Register) = Nil;
        Get_Environment_Frame_Size(Value_Register) = size;
        Get_Environment_Frame_Has_Rest(Value_Register) = has_rest;

        for ( i = 0; i < newsize; i++ )
	  {
                Get_Environment_Frame_Binding_Symbol(Value_Register,i) = Nil;
                Get_Environment_Frame_Binding_Value(Value_Register,i) = Nil;
                Get_Environment_Frame_Binding_How(Value_Register,i) = Nil;
          }
        Set_Result_Type(Environment_Frame_Type);
}

Public void Make_Symbol_Frame()
{
	Object formals, frame;
	Object rest = Last_Cdr(Top(1));
	Integer numargs = Length(Top(1));
	Integer this_arg;
	Integer frame_size;

	frame_size  = numargs + ((rest == Nil) ? 0 : 1);

	Make_Environment_Frame(frame_size, rest != Nil);
	frame = Value_Register;
	formals = Top(1);

	for (this_arg = 0; this_arg < numargs; this_arg++)
	{
		Get_Environment_Frame_Binding_Symbol(frame,this_arg) = 
			First(formals);
		Get_Environment_Frame_Binding_Value(frame,this_arg) = 
			The_Undefined_Symbol;
		Get_Environment_Frame_Binding_How(frame,this_arg) = 
			An_Argument;
		formals = Rest(formals);
	}
	if (formals != Nil)
	{
		Get_Environment_Frame_Binding_Symbol(frame,numargs) = formals;
		Get_Environment_Frame_Binding_Value(frame,numargs) = 
			The_Undefined_Symbol;
		Get_Environment_Frame_Binding_How(frame,numargs) = 
			An_Argument;
	}
	Get_Environment_Frame_Has_Rest(frame) = (rest != Nil);
	Pop(1);
}

Public	Integer	Print_As_Formals( o, m )

	Object	o;
	Integer	m;
{
	Integer this_arg;

	Output("("); m += 1;

	for (this_arg = 0; this_arg < Get_Environment_Frame_Size(o)-1; 
	     this_arg++)
	{
		m = Write_Object( 
			Get_Environment_Frame_Binding_Symbol(o,this_arg) , m );
		Output( " " ); m += 1;
	}

	if (Get_Environment_Frame_Has_Rest(o))
	{
		Output( ". " ); m += 2;
	}

	if (Get_Environment_Frame_Size(o) > 0)
	{
		m = Write_Object(
			Get_Environment_Frame_Binding_Symbol(o,
		    		Get_Environment_Frame_Size(o)-1) , m );
	}

	Output( ")" );  m += 1;
	return( m );
}

Private	Integer Show_One_Frame( o, m )

	Object	o;
	Integer	m;
{
	Integer	in_m = m;
	Integer binding;

	for (binding = 0; binding <= Get_Environment_Frame_Size(o)-1; 
	     binding++)
	{
		m = Write_Object( 
			Get_Environment_Frame_Binding_Symbol(o,binding) , m );
		Output( "\t= " ); m += 3;
		
		m = Write_Object(
			Get_Environment_Frame_Binding_Value(o,binding) , m );
		m = New_Left_Margin( in_m );
	}
	return( m );
}
Public Integer Environment_Frame_Show( frame , in_m )

       Object frame;
       Integer in_m;
{
       Integer m;

       while ( frame != The_Global_Environment )
       {
	       m  = Show_One_Frame( frame , in_m );
	       frame = Get_Environment_Frame_Previous( frame );
	       Output( "\n" );
       }
       if ( frame == The_Global_Environment )
       {
	       Output( "<The Global Environment>\n" );
       }	
       return( in_m );
}

Public Object Environment_Frame_GC(old)

	Object old;
{
	Object new = Move_Object(old, Environment_Frame_Size(
	    Get_Environment_Frame_Size(old)));
	Integer binding;

	Relocate(&Get_Environment_Frame_Previous(new));
	for (binding = 0; binding < Get_Environment_Frame_Size(new); binding++)
	{
		Relocate(&Get_Environment_Frame_Binding_Symbol( new, binding ));
		Relocate(&Get_Environment_Frame_Binding_Value( new, binding ));
		Relocate(&Get_Environment_Frame_Binding_How( new , binding ));
	}

	return new;
}

/* State Frame. */

Op_Vector State_Frame_Ops = {
	State_Frame_Eval, Self_Compile,
	State_Frame_Print, State_Frame_Print, State_Frame_Print,
	State_Frame_GC};

Scheme_Type State_Frame_Type = &State_Frame_Ops;

Public void Make_State_Frame()
{
	Value_Register = Allocate(State_Frame_Size);

	Set_Result_Type(State_Frame_Type);
}

Public	Integer	State_Frame_Print( o, m )

	Object	o;
	Integer	m;
{
	Output( "<state frame>" );
	return( m + 13 );
}

Public Object State_Frame_GC(old)

	Object old;
{
	Object new = Move_Object(old, State_Frame_Size);

	Relocate(&Get_State_Frame_Expression(new));
	Relocate(&Get_State_Frame_Environment(new));
	Relocate(&Get_State_Frame_Function(new));
	Relocate(&Get_State_Frame_Arguments(new));
	Relocate(&Get_State_Frame_State(new));

	return new;
}

/* Eclectics. */

Op_Vector Eclectic_Ops = {
	Self_Eval, Self_Compile,
        Eclectic_Print, Eclectic_Print, Eclectic_Print,
        Eclectic_GC};
Scheme_Type Eclectic_Type = &Eclectic_Ops;

Public Object The_Rparen_Object, The_Dot_Object;

Private void Init_Eclectic()
{
	The_Rparen_Object = Allocate(Eclectic_Size);
	The_Dot_Object = Allocate(Eclectic_Size);

	Get_Type(The_Rparen_Object) = Eclectic_Type;
	Get_Type_Name(The_Rparen_Object) = "Eclectic_Type/)";
	Get_Type(The_Dot_Object) = Eclectic_Type;
	Get_Type_Name(The_Dot_Object) = "Eclectic_Type/.";
}

Public	Integer	Eclectic_Print( o , m )

	Object	o;
	Integer	m;
{
	if (o == The_Rparen_Object)
	{
		Output( "<An extra `)'.>" );
		return( m + 15 );
	}
	else if (o == The_Dot_Object)
	{
		Output(" <An extra `.'.>" );
		return( m + 15 );
	}
	else
	{
		/* Although we don't do typechecking in any of the other
                output routines, we may as well do it here, since it's not
                costing us anything. */
		Panic("Eclectic_Print called on a non-junk");
		return( m );
	}
}     

Public Object Eclectic_GC(old)

	Object old;
{
	Object new = Move_Object(old, Eclectic_Size);

	return new;
}


/* Nothing should have this type. */
Public Scheme_Type The_Undefined_Type = (Scheme_Type)-1;

/* This type matches anything. */
Public Scheme_Type Any_Type = (Scheme_Type) -2;



/* StObj. Smalltalk Object */

Op_Vector StObj_Ops = {
	Self_Eval, Self_Compile, StObj_Display,
        StObj_Write, StObj_Show,  StObj_GC};

Scheme_Type StObj_Type = &StObj_Ops;

Public void Make_StObj(length)

	Integer length;
{
	Value_Register = Allocate(StObj_Size(length));
	Get_StObj_Length(Value_Register) = length;

	while (length--)
	{
		Get_StObj_Elem(Value_Register, length) = The_Undefined_Symbol;
	}

	Set_Result_Type(StObj_Type);
}

Public	Integer	StObj_Display( o, m )

	Object	o;
	Integer	m;
{
	Integer this_element;

	Output("#("); m += 3;

	for (this_element = 0; this_element < Get_StObj_Length(o); 
	     this_element++)
	{
		if ( this_element )
		{
			Output(" "); m += 1;
		}
		m = Display_Object( Get_StObj_Elem(o, this_element) , m );
	}

	Output(")"); m += 1;
	return( m );
}

Public	Integer	StObj_Write( o, m )

	Object	o;
	Integer	m;
{
	Integer this_element;

	Output("#("); m += 3;

	for (this_element = 0; this_element < Get_StObj_Length(o); 
	     this_element++)
	{
		if ( this_element )
		{
			Output(" "); m += 1;
		}
		m = Write_Object( Get_StObj_Elem(o, this_element) , m );
	}

	Output(")"); m += 1;
	return( m );
}


Public	Integer	StObj_Show( o, m )

	Object	o;
	Integer	m;
{
	Integer this_element;
	Integer length = Get_StObj_Length(o);

	Output("#("); m += 2;

	if  (length > 3)
	{
		m = Write_Object( Get_StObj_Elem(o,0) , m ) + 1; Output( " " );
		m = Write_Object( Get_StObj_Elem(o,1) , m ) + 1; Output( " " );
		m = Write_Object( Get_StObj_Elem(o,2) , m ) + 1; Output( " " );
		Output( "...)" ); m += 4;
	}
	else
	{
		for (this_element = 0; this_element < length; this_element++)
		{
			m = Write_Object(Get_StObj_Elem(o,this_element),m) + 1;
			Output( this_element == (length-1) ? ")" : " " );
		}
	}

	return( m );
}


Public Object StObj_GC(old)

	Object old;
{
	Object new = Move_Object(old, StObj_Size(Get_StObj_Length(old)));
	Integer this_element;

	for (this_element = 0; this_element < Get_StObj_Length(old); 
	     this_element++)
	{
		Relocate(&Get_StObj_Elem(new,this_element));
	}

	return new;
}


/* Called once at boot time. */

Public void Initialize_Object()
{
	Init_Boolean();
	Init_Eclectic();
	Init_Empty_List();
	Init_Symbol();
	Init_Port();
	Init_Eof();
	Init_Environment_Frame();
}
