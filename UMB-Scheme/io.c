/* io.c -- UMB Scheme, I/O package.

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
#include "steering.h"
#include "primitive.h"
#include "io.h"
#include "number.h"

Public FILE *The_Standard_Input, *The_Standard_Output, *The_Standard_Error;


/* A static variable printing keeps track of whether we want to print or not.*/

Private Boolean printing = TRUE;

Public void Set_Printing(turn_it_on)
	Boolean turn_it_on;
{
	printing = turn_it_on;
}

Public Boolean Get_Printing_State()
{
	return printing;
}


/* The routines that actually print somewhere. |Output| assumes that its 
argument will not have a null. */

Public void Output(s)

	String s;
{
	fprintf(Get_Port_File(Current_Output_Port), "%s", s);

	if ( The_Transcript_Port != Nil )
	{ 
		fprintf(Get_Port_File(The_Transcript_Port), "%s", s);
	}
}


Public void Error_Output(s)

	String s;
{
	fprintf( The_Standard_Error, "%s", s);

	if ( The_Transcript_Port != Nil )
	{ 
		fprintf(Get_Port_File(The_Transcript_Port), "%s", s);
	}
}

/* |Output_Char| should perhaps do something about control characters. 
When printing to the terminal, we certainly don't want to send control 
characters, for example. */

Public void Output_Char(c)

	Character c;
{
	fprintf(Get_Port_File(Current_Output_Port), "%c", c);

	if ( The_Transcript_Port != Nil )
	{ 
		fprintf(Get_Port_File(The_Transcript_Port), "%c", c);
	}
}

Public	Integer	New_Left_Margin( margin )
	
	Integer	margin;
{
	Integer	in_margin = margin;
	Output( "\n" );
	while ( margin-- > 0 )
		Output( " " );
	return( in_margin );
}

Public void Print_Type(t)
	Scheme_Type t;
{
	if (Boolean_Type == t) 
	{
		Output( "Boolean" ); 
	}
	if (Eclectic_Type == t) 
	{ 
		Output( "Eclectic" ); 
	}
	if (Pair_Type == t) 
	{ 
		Output( "Pair" ); 
	}
	if (Empty_List_Type == t) 
	{ 
		Output( "Empty_List" ); 
	}
	if (Symbol_Type == t) 
	{ 
		Output( "Symbol" ); 
	}
	if (Number_Type == t) 
	{ 
		Output( "Number" ); 
	}
	if (Character_Type == t) 
	{ 
		Output( "Character" ); 
	}
	if (String_Type == t) 
	{ 
		Output( "String" ); 
	}
	if (Vector_Type == t) 
	{ 
		Output( "Vector" ); 
	}
	if (Procedure_Type == t) 
	{ 
		Output( "Procedure" ); 
	}
	if (Primitive_Type == t) 
	{ 
		Output( "Primitive" ); 
	}
	if (Continuation_Type == t) 
	{ 
		Output( "Continuation" ); 
	}
	if (Port_Type == t) 
	{ 
		Output( "Port" ); 
	}
	if (Eof_Type == t) 
	{ 
		Output( "Eof" ); 
	}
	if (Variable_Type == t) 
	{ 
		Output( "Variable" ); 
	}
	if (Apply_Type == t) 
	{ 
		Output( "Apply" ); 
	}
	if (Lambda_Type == t) 
	{ 
		Output( "Lambda" ); 
	}
	if (Conditional_Type == t) 
	{ 
		Output( "Conditional" ); 
	}
	if (Assignment_Type == t) 
	{ 
		Output( "Assignment" ); 
	}
	if (Definition_Type == t) 
	{ 
		Output( "Definition" ); 
	}
	if (Defmacro_Type == t) 
	{ 
		Output( "Defmacro" ); 
	}
	if (Defmacro_Call_Type == t) 
	{ 
		Output( "Defmacro_Call" ); 
	}
	if (Sequence_Type == t) 
	{ 
		Output( "Sequence" ); 
	}
	if (Delay_Type == t) 
	{ 
		Output( "Delay" ); 
	}
	if (Promise_Type == t) 
	{ 
		Output( "Promise" ); 
	}
	if (Error_Type == t) 
	{ 
		Output( "Error" ); 
	}
	if (Environment_Frame_Type == t) 
	{ 
		Output( "Environment_Frame" ); 
	}
	if (State_Frame_Type == t) 
	{ 
		Output( "State_Frame" ); 
	}
	if (Any_Type == t) 
	{ 
		Output( "Any" ); 
	}
}

/* Reading. */

#define	MAX_TOKEN_SIZE 1000

typedef enum
{
	Lparen_Token,	Rparen_Token,	Quote_Token,	Backquote_Token,
	Dot_Token,	Comma_Token,	Open_Vec_Token,	True_Token,
	False_Token,	String_Token,	Number_Token,	Character_Token,
	Symbol_Token,	Error_Token,	Comma_At_Token,	Eof_Token
} 
Token ;

Private	Token	The_Token ;
Private	String Token_String ;
Private	Character Token_Buffer[ MAX_TOKEN_SIZE ] ;
Private	Integer	Token_Index ;
Private	Boolean Transcripting = FALSE;

#define Is_Control_Char iscntrl
#define	Is_White_Space	isspace 

#define Scan_Char(f)	(Transcripting?Tscan(f):getc(f))

Private	void Read_Number() ;
Private	void Read_Symbol() ;
Private int  Force_Lower() ;
Private void Read_Token();
Private void Read_List();


/* Auxiliary input routines. */

Private	int	Tscan( f )

	FILE *	f;
{
	int c; 
	if ( (c = getc(f)) != EOF ) putc(c , Get_Port_File(The_Transcript_Port));
	return( c );
}

	

Private Boolean Is_Delimiter(c)

	int	c;
{
    return( Is_White_Space(c) || c == '(' || c == ')' || c == '"' || 
	    c == ';' || c == EOF);
}


/* Force uppercase letters (and only letters) to lowercase. */
Private	int Force_Lower( Ch )
	int	Ch ;
{
	return( isupper( Ch ) ? (Ch - 'A' + 'a') : Ch ) ;
}


/* Implement the ANSI routine `toint'. */

Public Integer toint(c)
	int	c;
{
	if (isxdigit(c))
	{
		c = Force_Lower(c);
		if (c >= 'a')
			return c - 'a' + 10;
		else
			return c - '0';
	}
	else 
	{
		Panic( "Non-hex digit passed to toint" );
		return 0;
	}
}

/* Read a Scheme object from |Input_File|; leave it in Value_Register. */

Public	void Read( Input_File )

	FILE*	Input_File ;
{
	Transcripting = The_Transcript_Port != Nil 
				&&  Input_File == The_Standard_Input; 

	Read_Token( Input_File ) ;

	switch( The_Token )
	{
	case Symbol_Token :
		Value_Register = Intern_Name( Token_String ) ;
		break ;

	case Lparen_Token :
		Read_List( Input_File ) ;
		break ;

	case Number_Token :
		Cstring_To_Number( Token_String , 0 ) ;
		break ;

	case String_Token :
		/* We want to allow nulls in string constants. Hence
                   |memcpy| instead of |strcpy|. */
		Make_String( Token_Index );
		memcpy( Get_String_Value(Value_Register), Token_Buffer, 
		    Token_Index );
		Get_String_Value( Value_Register ) [ Token_Index ] = '\0';
		break ;

	case Character_Token :
		Make_Character( *Token_String ) ;
		break ;

	case True_Token :
		Value_Register = The_True_Object ;
		break ;

	case False_Token :
		Value_Register = The_False_Object ;
		break ;
		
	    case Open_Vec_Token :
		Read_List( Input_File ) ;
		Push( Value_Register ) ;
		List_To_Vector() ;
		Pop( 1 ) ;
		break ;

	case Dot_Token :
		Value_Register = The_Dot_Object ;
		break ;

	case Rparen_Token :
		Value_Register = The_Rparen_Object ;
		break ;

	case Quote_Token :
		Push( Intern_Name( "quote" ) ) ;
		Read( Input_File ) ;
		Push( Value_Register ) ;
		Push( Nil ) ;
		Make_Pair() ;
		Push( Value_Register ) ;
		Make_Pair() ;
		break ;

	case Backquote_Token :
		Push( Intern_Name( "quasiquote" ) ) ;
		Read( Input_File ) ;
		Push( Value_Register ) ;
		Push( Nil ) ;
		Make_Pair() ;
		Push( Value_Register ) ;
		Make_Pair() ;
		break ;

	case Comma_Token :
		Push( Intern_Name( "unquote" ) ) ;
		Read( Input_File ) ;
		Push( Value_Register ) ;
		Push( Nil ) ;
		Make_Pair() ;
		Push( Value_Register ) ;
		Make_Pair() ;
		break ;

	case Comma_At_Token :
		Push( Intern_Name( "unquote-splicing" ) ) ;
		Read( Input_File ) ;
		Push( Value_Register ) ;
		Push( Nil ) ;
		Make_Pair() ;
		Push( Value_Register ) ;
		Make_Pair() ;
		break ;

	case Error_Token :
		Make_Error( Token_String ) ;
		break ;

	case Eof_Token :
		Value_Register = The_Eof_Object ;
		break ;

	default :
		Panic( "Unidentified token" ) ;
		break ;
	}
}


/* Read list from Input_File and leave it in Value_Register. This allows 
the input `( . x )' (it treats it as equivalent to x), which is not strictly 
legal according to the manual. */

Private	void Read_List( Input_File )

	FILE*	Input_File ;
{
	Read( Input_File ) ;

	if ( Value_Register == The_Rparen_Object )
	{
		Value_Register = Nil ;
	}
	else if ( Value_Register == The_Dot_Object )
	{
		Read( Input_File ) ;
		if ( Value_Register == The_Rparen_Object || 
		    Value_Register == The_Dot_Object ||
		    Value_Register == The_Eof_Object )
		{
			Make_Error( "Bad syntax involving dot operator" ) ;
		}
		else
		{
			Push( Value_Register ) ;
			Read( Input_File ) ;
			if ( Value_Register != The_Rparen_Object )
			{
				Make_Error("No right parenthesis after dot") ;
			}
			else
			{
				Value_Register = Top( 1 ) ;
			}
			Pop( 1 ) ;
		}
	}
	else if ( Value_Register == The_Eof_Object )
	{
		Make_Error( "Unexpected EOF" ) ;
	}
	else
	{
		Push( Value_Register ) ;
		Read_List( Input_File ) ;
		if ( Is_Error( Top( 1 ) ) )
		{
			Value_Register = Top( 1 ); /* Propagate first error. */
			Pop( 1 ) ;
		}
		else if ( Is_Error( Value_Register ) )
		{
			Pop( 1 ) ;
		}
		else
		{
			Push( Value_Register ) ;
			Make_Pair() ;
		}
	}
}


/* Read a single token from |Input_File|. Leave the result in |The_Token|, 
   and the string matched in |Token_Buffer|. (|Token_Index| will be the 
   length of the string matched. */

Private	void Read_Token( Input_File )

	FILE*	Input_File ;
{
	int	ch ; 	/* Hold input characters */

READ_A_TOKEN:

	while ( Is_White_Space( ch = Scan_Char( Input_File ) ) );

	switch ( ch )
	{
	case '(' :
		The_Token = Lparen_Token;
		break;

	case ')' :
		The_Token = Rparen_Token;
		break;

	case '\'' :
		The_Token = Quote_Token;
		break;

	case '`' :
		The_Token = Backquote_Token;
		break;

	case '.' :
		if ( isdigit( Peek_Char( Input_File ) ) )
		{
			Read_Number( '.' , Input_File );
		}
		else if ( Peek_Char( Input_File ) == '.' )
	        {
	                Read_Symbol( '.' , Input_File );
		}
		else
		{
			The_Token = Dot_Token;
		}
		break;

	case ',' :
		if ( Peek_Char( Input_File ) == '@' )
		{
			ch = Scan_Char( Input_File );
			The_Token = Comma_At_Token ;
		}
		else
		{
			The_Token = Comma_Token ;
		}
		break ;
		
	    case '#' :
		switch ( Force_Lower( Peek_Char( Input_File ) ) )
		{
		case '(' :
			ch = Scan_Char( Input_File );
			The_Token = Open_Vec_Token ;
			break ;

		case 't' :
			ch = Scan_Char( Input_File );
			The_Token = True_Token ;
			break ;

		case 'f' :
			ch = Scan_Char( Input_File );
			The_Token = False_Token ;
			break ;

		case '\\' :
			ch = Scan_Char( Input_File );
			The_Token = Character_Token ;

			/* Scan character or character name */

			Token_Index = 0 ;
			Token_Buffer[Token_Index++] = ch = 
				        Scan_Char( Input_File ) ;
			if ( isalpha( ch ) )
			{
          			while (!Is_Delimiter( Peek_Char( Input_File )))
			        {
				        Token_Buffer[Token_Index++] = 
					        Scan_Char( Input_File );
			        }
			}
			Token_Buffer[Token_Index] = '\0' ;

			Token_String =
			    Token_Index == 1 ? Token_Buffer :
			    Eq_Strs( Token_Buffer , "space" )   ? " " :
			    Eq_Strs( Token_Buffer , "tab" )     ? "\t" :
			    Eq_Strs( Token_Buffer , "newline" ) ? "\n" :
			    Eq_Strs( Token_Buffer , "newpage" ) ? "\f" :
			    ( The_Token = Error_Token,
			    "Unrecognized character name" ) ;
			break ;

		case 'i' : 
		case 'e' : 
		case 's' : 
		case 'l' :
		case 'b' : 
		case 'o' : 
		case 'd' : 
		case 'x' :

			/* All legal prefixes to numbers */
			Read_Number( '#' , Input_File ) ;
			break ;

		default :
			/* Call it a symbol (not exactly legal) */
			Read_Symbol( '#' , Input_File ) ;
		}
		break ;
		
	    case '"' :
		The_Token = String_Token ;
		Token_Index = 0 ;
		while ( (ch = Scan_Char( Input_File )) != '\"' ) 
		{
			if ( Token_Index >= MAX_TOKEN_SIZE )
			{
				Error( "Missing closed quote on a string" );
			}

			if ( ch == '\\' )
			{
				/* \ is an escape character */
				ch = Scan_Char( Input_File ) ;
			}
			Token_Buffer[ Token_Index++ ] = ch ;
		}
		Token_Buffer[Token_Index] = '\0' ;
		Token_String = Token_Buffer ;
		break ;

	case '+' :
	case '-' :
		if ( (Is_Delimiter( Peek_Char( Input_File ) )) )
		{
			/* The symbol + or -. */
			Read_Symbol( ch , Input_File ) ;
		}
		else if ( Peek_Char( Input_File ) == '>' )
		{
		        /* A fix */
		        Read_Symbol( ch , Input_File ) ;
		}
		else
		{
			/* Otherwise the + or - must start a number. */
			Read_Number( ch , Input_File ) ;
		}
		break ;

	case ';' :
		/* ; announces a comment which extends to the newline */
		Token_String = fgets(Token_Buffer, MAX_TOKEN_SIZE, Input_File);
		goto READ_A_TOKEN ;
		break ;

	case EOF :
		The_Token = Eof_Token ;
		break ;

	default :
		if ( isdigit( ch ) )
		{
			Read_Number( ch , Input_File ) ;
		}
		else
		{
			Read_Symbol( ch , Input_File ) ;
		}
	}
}


/* We do a naive scan, scanning up to a delimiter.  The external rep is
   actually parsed when it's converted to a number in Read. */

Private	void Read_Number( Ch , Input_File )

	Character	Ch ;		/* Leading Character of Number */
	FILE*		Input_File ;	/* Containing remaining chars */
{

	Token_Buffer[0] = Ch ;
	Token_Index = 1 ;
	while ( !Is_Delimiter( Force_Lower( Peek_Char( Input_File ) ) ) )
	{
		Token_Buffer[Token_Index++] = 
			Force_Lower( Scan_Char( Input_File ) );
	}
	Token_Buffer[Token_Index] = '\0' ;
	The_Token = Number_Token ;
	Token_String = Token_Buffer ;
}


/* We do a naive scan, scanning up to a delimiter.  This allows for more than
   is described in the formal syntax. We do eliminate control characters. */

Private	void Read_Symbol( Ch , Input_File )

	Character	Ch ;		/* Leading character of symbol. */
	FILE*		Input_File ;	/* Containing remaining chars. */
{
	Token_Index = 0 ;
	if (! Is_Control_Char(Ch))
	{
		Token_Buffer[Token_Index++] = Force_Lower( Ch ) ;
	}

	while ( !Is_Delimiter( Peek_Char( Input_File ) ) )
	{
		Ch = Scan_Char( Input_File );
		if (! Is_Control_Char(Ch))
		{
			Token_Buffer[Token_Index++] = Force_Lower( Ch ) ;
		}
	}; 

	if (Token_Index > 0)
	{
		Token_Buffer[Token_Index] = '\0' ;
		The_Token = Symbol_Token ;
		Token_String = Token_Buffer ;
	} 
	else
	{
		The_Token = Error_Token;
		Token_String = "Null symbol name";
	}
}
