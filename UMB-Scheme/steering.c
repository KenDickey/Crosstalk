/* steering.c -- UMB Scheme, steering routines

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

#include  <setjmp.h>
#include  <signal.h>
#include  <string.h>

#include "portable.h"
#include "eval.h"
#include "object.h"
#include "primitive.h"
#include "steering.h"
#include "debug.h"
#include "architecture.h"
#include "io.h"
#include "number.h"

/* Public variables. */
Public	jmp_buf	Top_Level_Loop;
Public	jmp_buf	Debugging_Loop;
Public	String	Prompt;

/* Internal routines. */

Private void Initializations();
Private void Steering();
Private	void Load_File();
Private Boolean File_Exists();


#define TOP_LEVEL_PROMPT "\n==> "

#define STANDARD_PRELUDE_PATHNAME strcat(getenv("$HOME"), "/Scheme/UMB-Scheme/prelude.scheme")

Private String OPENING  =
"Welcome to UMB Scheme, version      Copyright (c) 1988,1996 William R Campbell.\n\
UMB Scheme comes with ABSOLUTELY NO WARRANTY. This is free software and\n\
you are free to redistribute it under certain conditions.\n\
See the UMB Scheme Release Notes for details. \n\
Type `(exit)` or Control-d to exit.\n\n";

Private String Rev = "$Revision: 3.2 $";

Private	int	Argc;
Private	char	**Argv;

Public void main( argc , argv )
	int	argc;
	char	*argv[];
{
	Argc = argc;
	Argv = argv;
	Steering();
}

Private	Boolean Init_File_Complete = FALSE;
Public	Boolean Prelude_Started = FALSE;
Public	Boolean	Prelude_Complete = FALSE;

Private void Steering()
{
	Character Opening[400];
	String	Init_Filename = getenv ("SCHEME_INIT");
	Character Dot_Scheme_Filename [256];
	sprintf (Opening, "%s", OPENING);
	sprintf (Dot_Scheme_Filename, "%s/.scheme", getenv ("HOME"));

	Initializations();


	Opening[31] = Rev[11];
	Opening[32] = Rev[12];
	Opening[33] = Rev[13];
	Opening[34] = Rev[14];


	Output( Opening );

	signal( SIGINT  , Handler );
	signal( SIGFPE  , Handler );
	signal( SIGILL  , Handler );
	signal( SIGSEGV , Handler );
	signal( SIGTERM , Handler );

	setjmp( Top_Level_Loop );	/* Return here upon Reset(). */

	clearerr( The_Standard_Input );

	Set_Printing( TRUE );

	Environment_Register = The_Global_Environment;
	State_Register = Nil;
	Value_Register = Nil;
	Expression_Register = Nil;
	Function_Register = Nil;
	Arguments_Register = Nil;
	Reset_Stack( 0 );

	State_Debugged = Nil;
	Value_Debugged = Nil;

	Control_C = FALSE;
	Evaluating = FALSE;
	Evaluation_Broken = FALSE;
	Go_Processed = FALSE;

	if ( ! Prelude_Started )
	{
		Prelude_Started = TRUE;
		Load_File(STANDARD_PRELUDE_PATHNAME);
	}
	Prelude_Complete = TRUE;
	
	if ( ! Init_File_Complete )
	{
		Init_File_Complete = TRUE;

		if (Init_Filename != NULL)
		{
			Load_File (Init_Filename);
		}
		else if (File_Exists (Dot_Scheme_Filename))
		{
			Load_File (Dot_Scheme_Filename);
		}
	}

	while (--Argc > 0)
	{
		Load_File(*++Argv);
	}

	Prompt = TOP_LEVEL_PROMPT;
	Read_Eval_Print( The_Standard_Input );

	if (Arg_Stack_Ptr != 0)
	{
		Panic( "Non-zero argstack pointer on exit" );
		Arg_Stack_Ptr = 0;
	}
}


Private Boolean File_Exists(Filename)

	String      Filename;
{
	FILE * fp;
	
	fp = fopen (Filename, "r");
	if (fp != NULL)
	{
		(void) fclose(fp);
		return( TRUE );
	}
	return( FALSE );
}

Private	void	Load_File(Filename)

	String      Filename;
{
	Make_Constant_String(Filename);
	Push(Value_Register);
	Save(); Load(); Restore();
	Pop(1);
}

Public void Read_Eval_Print( input )
	FILE*	input;	/* C file from which expressions are Read() */
{
	while ( ! Go_Processed )
	{
		if (Get_Printing_State()) Output( Prompt );

		Read( input );

		if (Value_Register == The_Eof_Object) break;

		Push( Value_Register );
		Compile_Object( Top( 1 ));

		Debugger_Activated =  ! Debugging && Debugger_Switched_On;
		Eval( Value_Register, Environment_Register );
		Debugger_Activated = FALSE;

		if (Get_Printing_State()) 
		{
			Output( "\n" );
			(void) Write_Object( Value_Register , 0 );
		}
	}
}


#define ERROR_PREFIX "\nError: "
#define PANIC_PREFIX "\nFatal Error: "


Public void Error( message )
	String	message;
{
	Error_Output( ERROR_PREFIX );
	Error_Output( message );
	Error_Output( ".\n" );
	Break();
}

Public void Error1(message, name)
	String message, name;
{
	Character error_string[256];

	sprintf( error_string, message, name );

	Error_Output( ERROR_PREFIX );
	Error_Output( error_string );
	Error_Output( ".\n" );
	Break();
}

Public void Display_Error(message, object)
	String message;
	Object object;
{
	Error_Output( ERROR_PREFIX );
	Error_Output( message ); 
	Push( Current_Output_Port );
	Current_Output_Port = Current_Error_Port;
	(void) Write_Object( object , 0 );
	Current_Output_Port = Top(1); Pop(1);
	Error_Output( "\n" );
	Break();
}


Public void Panic( message )

	String	message;
{
	Error_Output( PANIC_PREFIX );
	Error_Output( message );
	Error_Output( ".\n" );
	Reset();
}

Public	void Break()
{
	Import	jmp_buf	Eval_Loop;

	if ( Debugger_Activated )
	{
		Debugger_Activated = FALSE;

		if ( Evaluating )
		{
			Evaluation_Broken = TRUE;
			longjmp( Eval_Loop , 1 );
		}
		else
		{
			Reset();
		}
	}
	else if ( Debugging )
	{
		longjmp( Debugging_Loop , 1 );
	}
	else
	{
		Reset();
	}
}


Public	void Reset()
{
	Debugger_Activated = FALSE;
	Error_Output( "\nReset (Type `(exit)` or Control-d to quit UMB Scheme)" );
	longjmp( Top_Level_Loop , 1 );
}


Public	void Handler( sig )

	Integer	sig ;
{
	switch ( sig )
	{
	    case SIGINT:
		/* Control-D */

		if ( Allocating )
		{
		   Control_C = TRUE;
		   break;
		}
		else
		{
		   Break();
		}
	
	    case SIGFPE:
		Error( "Floating Point Exception" );

	    case SIGILL:
		Panic( "Illegal Instruction" );

	    case SIGSEGV:
		Panic( "Segmentation Violation" );

	    case SIGTERM:
		Error( "Terminated" );

	    default:
		Panic( "Unhandled Signal" );
	}
}

Private void Initializations()
{
	/* The order of these does matter. */

	Initialize_Architecture();
	Initialize_Object();
	Initialize_Number();
	Initialize_Primitive();
	Initialize_Debug();
}

