
/* debug.c -- UMB Scheme, debugging routines.

UMB Scheme Interpreter  				$Revision: 3.2 $
Copyright (C) 1988, 1991 William R Campbell

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
Long Nguyen, Susan Quina, Jeyashree Sivasubram, Bela Sohoni and Thang Quoc Tran.

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

#include "portable.h"
#include "eval.h"
#include "object.h"
#include "primitive.h"
#include "steering.h"
#include "debug.h"
#include "architecture.h"
#include "io.h"
#include "number.h"

/* Public Variables */

Public	Boolean	Control_C = FALSE;
Public	Boolean Debugger_Activated = FALSE;
Public	Boolean	Debugger_Switched_On = FALSE;
Public	Boolean	Debugging = FALSE;
Public	Boolean	Go_Processed = FALSE;
Public	Boolean Evaluating = FALSE;
Public	Boolean	Evaluation_Broken = FALSE;
Public	Boolean	At_Top_Level = TRUE;
Public	Boolean	Tracing = FALSE;
Public	Boolean	Tracing_All = FALSE;
Public	Integer	Stepping = 0;
Public	Integer	Stepper = 0;

Public	Integer Trace_Margin = 0;
Public	Object	Traced_Procedures;

#define DEBUGGING_PROMPT "\ndebug> "

Public	void Steer_Debugging()
{
	String	saved_prompt = Prompt;
	Import	jmp_buf	Debugging_Loop;

	Debugger_Activated = FALSE;

	Value_Debugged = Value_Register;
	Save();
	State_Debugged = State_Register;

	setjmp( Debugging_Loop );
	Debugging = TRUE;

	clearerr( The_Standard_Input );

	State_Register = Nil;
	Expression_Register = Nil;
	Function_Register = Nil;
	Arguments_Register = Nil;
	Environment_Register = Get_State_Frame_Environment( State_Debugged );
	Reset_Stack( Get_State_Frame_Top( State_Debugged ) );

	Prompt = DEBUGGING_PROMPT;
	Read_Eval_Print( The_Standard_Input );
	Prompt = saved_prompt;
	clearerr( The_Standard_Input );

	State_Register = State_Debugged;
	Restore();
	Value_Register = Value_Debugged;

	Debugging = FALSE;
	Debugger_Activated = TRUE;
}

/* Debugging Primitives */

Private	void	Debug()		/* (debug) */
{
	Debugger_Switched_On = TRUE;
	Value_Register = Nil;
}

Private	void	Debug_Off()	/* (debug-off) */
{
	Debugger_Switched_On = FALSE ;
	Reset();
	Value_Register = Nil;
}


Private	void	Step()		/* (step n) */
{
	Stepping = Stepper =  Number_To_Integer( Top(1) );
	Value_Register = Top(1);

	if ( Evaluating )
	{
		/* State was saved in Eval() -- get right out */
		Restore();
	}
}


Private	void	Trace()		/* (trace)
				   (trace proc...) */
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );

        if ( arg_count == 0 )
	{
		Tracing_All = TRUE;
	}
	else while ( arg_count )
	{
		if ( Is_Procedure( Top( arg_count ) ) )
		{
			Get_Procedure_Tracing( Top( arg_count ) ) = TRUE;
		}
		else if ( Is_Primitive( Top( arg_count ) ) )
		{
			Get_Primitive_Tracing( Top( arg_count ) ) = TRUE;
		}
		else
		{
		    Display_Error( "Attempt to trace a non-procedure object: ",
					Top( arg_count ) );
		}
		Push( Top( arg_count ) );
		Push( Traced_Procedures );
		Make_Pair();
		Traced_Procedures = Value_Register;
		arg_count -- ;
	}

	Tracing = TRUE;
	Value_Register = Nil;

	if ( Evaluating )

	{
		/* State was saved in Eval() -- get right out */
		Restore();
	}
}

Private	void	Untrace() 	/* (untrace)
				   (untrace proc...) */
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
	
	if ( arg_count == 0 )
	{
		Tracing = Tracing_All = FALSE;
		
		while ( Traced_Procedures != Nil )
		{
			Object proc = First( Traced_Procedures );

			if ( Is_Procedure( proc ))
			{
				Get_Procedure_Tracing( proc ) = FALSE;
			}
			else if ( Is_Primitive( proc ))
			{
				Get_Primitive_Tracing( proc ) = FALSE;
			}
			else
			{
				Display_Error( 
				   "Attempt to trace a non-procedure object: ",
				   proc );
			}
			Traced_Procedures = Rest( Traced_Procedures );
		}
	}
	else while ( arg_count )
	{
		if ( Is_Procedure( Top( arg_count ) ) )
		{
			Get_Procedure_Tracing( Top( arg_count ) ) = FALSE;
		}
		else if ( Is_Primitive( Top( arg_count ) ) )
		{
			Get_Primitive_Tracing( Top( arg_count ) ) = FALSE;
		}
		else
		{
		    Display_Error( "Attempt to trace a non-procedure object: ",
					Top( arg_count ) );
		}
		arg_count--;
	}

	Value_Register = Nil;

	if ( Evaluating )

	{
		/* State was saved in Eval() -- get right out */
		Restore();
	}
}



Private void	GoN()		/*  (#_go k obj)  */
{
	Integer	k = Number_To_Integer( Top(2) ); /* State Frames to descend */
	Object	state = State_Debugged;
	Object	last = Nil;

	while ( k-- && state != Nil )
	{
		if ( Get_State_Frame_Expression( state ) != last )
		{
			last = Get_State_Frame_Expression( state );
		}
		state = Get_State_Frame_State( state );
	}
	if ( Debugging )
	{
		if ( state != Nil )
		{
			State_Debugged = state;
			Value_Debugged = Top( 1 );
			Go_Processed = TRUE;
		}
		else
		{
			Error( "k too large in (go# k obj)" );
		}
	}
	else
	{
		Error( "(go k obj) executed outside of debugging mode" );
	}
	Value_Register = Nil;
}


Private	void	Show_Proc_Env()	/* (show-proc-env proc) */
{
	Object	frame = Get_Procedure_Environment( Top(1) );
	Integer	dummy;

	Output( "\n" );
	dummy = Environment_Frame_Show( frame , 0 );

	Value_Register = Nil;
}



Private void	Show_Global_Binding( Symaddr )

	Object	*Symaddr;
{
	Object	Sym = * Symaddr;
	if ( Get_Symbol_User_Defined( Sym ) )
	{
		Integer m = 0;
		Output( "\n" );
		m = Show_Object( Sym , 0 ); 
		Output( "\t= " ); m = 12;
		m = Show_Object( Get_Global_Binding( Sym ) , m );
	}
}


Private	void	Show_Global_Env()
{
	Output( "\nUser-defined Global Symbols:\n" );
	Symbol_Hash_Iterate( Show_Global_Binding ); 
	Value_Register = Nil;
	
}


Private	void	Show_Env()	/* (show-env k) -- we ignore k */
{
	Object	frame = Get_State_Frame_Environment( State_Debugged );
	Integer	dummy;

	Output( "\n" );
	dummy = Environment_Frame_Show( frame , 0 );

	Value_Register = Nil;
}

Private	void	Where()		/* (where k) */
{
	Integer	k; 	/* expressions to show */
	Integer	counter = 0;
	Object	state = State_Debugged;
	Object	last = Nil;
	Character countstr[20];
	k = Number_To_Integer( Top( 1 ) );

	while ( k-- && state != Nil )
	{
		if ( Get_State_Frame_Expression( state ) != last )
		{
			last = Get_State_Frame_Expression( state );

			sprintf( countstr , "\n\n%2d>  " , counter++ );
			Output( countstr );
			(void) Write_Object( last , 5 );
		}
		state = Get_State_Frame_State( state );
	}
	Value_Register = Nil;
}


Private	void	How()	/*  (#_how symbol)  */
{
	Object	env = Debugging ? Get_State_Frame_Environment( State_Debugged )
				: The_Global_Environment;
	Object	sym = Top( 1 );
	Integer	displacement;
	
	while ( env != The_Global_Environment )
	{
		for ( displacement = 0; 
		      displacement < Get_Environment_Frame_Size( env );
		      displacement++ )
		{
			if (Get_Environment_Frame_Binding_Symbol( env,
							displacement ) == sym )
			{
			    Show_Object(
			     Get_Environment_Frame_Binding_How(env,displacement), 					0 );
			    Value_Register = Nil;
			    return;
			}
		}
		env = Get_Environment_Frame_Previous( env );
	}
	Show_Object( Get_Symbol_How( sym ) , 0 );
	Value_Register = Nil;
}




Public void Initialize_Debug()
{
	Traced_Procedures = Nil;

	Make_Primitive("debug", Debug , 0, The_Undefined_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("debug-off", Debug_Off , 0, The_Undefined_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("trace", Trace , VARYING, Any_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("untrace", Untrace, VARYING, Any_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("#_go", GoN, 2, Number_Type, Any_Type, 
	    The_Undefined_Type);

	Make_Primitive("step", Step , 1 , Number_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("show-globals", Show_Global_Env,0,The_Undefined_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("show-proc-env", Show_Proc_Env,1,Procedure_Type, 
	    The_Undefined_Type, The_Undefined_Type);

	Make_Primitive("#_show-env", Show_Env,1,Number_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("#_where", Where , 1, Number_Type, The_Undefined_Type,
	    The_Undefined_Type);

	Make_Primitive("#_how", How , 1, Symbol_Type, The_Undefined_Type,
	    The_Undefined_Type);

}
