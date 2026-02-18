/* eval.c -- UMB Scheme, explicit control evaluator.

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

#include <setjmp.h>
#include "portable.h"
#include "eval.h"
#include "object.h"
#include "architecture.h"
#include "steering.h"
#include "debug.h"
#include "io.h"


#define	Goto(x)	PC_Register = (x)

Public	jmp_buf	Eval_Loop;

Private void Call_Primitive();
Private void Restore_Continuation_State();


Public void Self_Eval( Expression )

Object	Expression ;	/* Evalauates to itself */
{
	Value_Register = Expression ;
	Restore();
}



Public void Eval( Expression , Environment )

	Object	Expression ;	/* To be evaluated */
	Object	Environment;
{
	Boolean	saved_evaluating = Evaluating;

	Expression_Register = Expression ;
	Value_Register = Nil ;
	Function_Register = Nil ;
	Environment_Register = Environment;
	PC_Register = RETURN ;

	Save() ;

	PC_Register = EVAL_EXPRESSION ;

	if ( Debugger_Activated )
	{
		Evaluating = TRUE;
		setjmp( Eval_Loop ); 

		if ( Evaluation_Broken )
		{
			Evaluation_Broken = FALSE;
			Output( "\nBreak:\t" );
			(void) Show_Object( Expression_Register , 9 );

			Steer_Debugging();
			if ( Go_Processed )
			{
				Restore();
				Go_Processed = FALSE;
			}
			else
			{
				Reset();
			}
		}
	}

	while ( PC_Register != RETURN )
	{
		switch( PC_Register )
		{
		case EVAL_EXPRESSION:
			if ( Debugger_Activated && Stepping )
			{
				if ( (--Stepper) == 0 )
				{
					Debugger_Activated = FALSE;
					Output( "\nStep:\t" );
					(void) Show_Object( Expression_Register,
								9 );
					Steer_Debugging();
					Debugger_Activated = TRUE;
					Stepper = Stepping;
					if ( Go_Processed )
					{
						Restore();
						Go_Processed = FALSE;
						break;
					}
				}
			}
			Eval_Object( Expression_Register ) ;
			break;

		case EVAL_APPLY:
			Function_Register = Value_Register;
			Arguments_Register = Get_Apply_Arguments( 
			Expression_Register );

		case EVAL_ARGUMENTS:
		    LABEL_ARGUMENTS:
			if ( Arguments_Register != Nil )
			{
				if ( !Is_Pair( Arguments_Register ) )
				{
					Display_Error( 
					"Arguments must be a list:",
				         Arguments_Register );
				}

				PC_Register = STACK_ARGUMENT;
				Save();

				Expression_Register = First( 
					    Arguments_Register );
				Goto( EVAL_EXPRESSION );
				break;
			}

			/* Otherwise, fall through */

		case PERFORM_APPLICATION:

			if ( Evaluating )
			{
			    if (Tracing && ! Debugging && (Tracing_All || 
				Traced( Function_Register ) ) )
			    {
				Integer left;
				Integer arg = Get_Apply_Numargs( 
						Expression_Register );
				Save();
				
				left = New_Left_Margin( Trace_Margin );
				Trace_Right();
				Output( "Trace: (" ); left += 8;
				Output( Name_For( Function_Register ) );
				left += strlen( Name_For( Function_Register ) );
				while (arg)
				{
					Output( " " ); left++;
					left = Show_Object( Top(arg) , left );
					arg--;
				}
				Output( ")" ); left++;
			    
				Restore();

				Steer_Debugging();
				if ( Go_Processed )
				{
					Restore();
					Go_Processed = FALSE;
					break;
				}
			    }

			    PC_Register = APPLICATION_COMPLETE;
			    Save();
			}

			if ( Is_Primitive( Function_Register ) )
			{
				Call_Primitive( Function_Register );
			}
			else if ( Is_Procedure( Function_Register ) )
			{ 
				Extend_Environment(Get_Apply_Numargs(
						 Expression_Register));
				Expression_Register = Get_Procedure_Body(
					    Function_Register);
				Goto( EVAL_EXPRESSION );
			}
			else if ( Is_Continuation( Function_Register ) )
			{
				if ( Get_Apply_Numargs(Expression_Register)!= 1)
				{
					Display_Error( 

					"Continuation requires one argument:",
					Expression_Register);
				}

				Value_Register = Top(1);
				Restore_Continuation_State( Function_Register );
			}
			else
			{
				Display_Error( "Bad function object:",
				    Function_Register );
			}
			break;

		case APPLICATION_COMPLETE:

			if (Tracing && ! Debugging && (Tracing_All || 
				Traced( Function_Register ) ) )
			{
				Integer left;
				Trace_Left();
				left = New_Left_Margin( Trace_Margin ); 
				Output( "Trace: Value = " ); left += 15;
				left = Show_Object( Value_Register , left );

				Steer_Debugging();
				Go_Processed = FALSE;
			}
			Restore();
			break;

		case STACK_ARGUMENT:
			Push( Value_Register );
			Arguments_Register = Rest( Arguments_Register );
			goto LABEL_ARGUMENTS;

		case EVAL_DEFINITION:
			Define( Get_Definition_Lvalue( Expression_Register ),
			    Value_Register,
			    Environment_Register );
			Restore();
			break;

		case EVAL_CONDITIONAL:
			Expression_Register =
			    Is_False( Value_Register )
			    ? Get_Conditional_Alternate( Expression_Register )
			    : Get_Conditional_Consequent( Expression_Register );
			Goto( EVAL_EXPRESSION );
			break;


		case EVAL_SEQUENCE:
			Expression_Register = First( Arguments_Register ) ;
			Arguments_Register = Rest( Arguments_Register );
			if (  Is_Pair( Arguments_Register ) )
			{
				/* More clauses after this.  Must save state. */
				PC_Register = EVAL_SEQUENCE ;
				Save();
			}
			Goto( EVAL_EXPRESSION );
			break;

		case EVAL_ASSIGNMENT:
			Assign( Get_Assignment_Lvalue( Expression_Register ),
			    Value_Register,
			    Environment_Register );
			Restore();
			break;
			
	        case OVERWRITE_PROMISE:
			Get_Promise_Expression( Expression_Register ) =
			    Value_Register;
			Get_Promise_Forced( Expression_Register ) = TRUE;
			Restore();
			break;


			
		default:
			Panic( "Bad Evaluation Label in Evaluate()" );
		}
	}
	if ( Debugger_Activated )
	{
		Evaluating = saved_evaluating;
	}
}


Public void Apply_Eval()
{
	PC_Register = EVAL_APPLY ;
	Save();

	/* Firstly, evaluate operator */

	Expression_Register = Get_Apply_Operator( Expression_Register );
	Goto( EVAL_EXPRESSION );
}


Public void Lambda_Eval()
{
	/* Lambdas evaluate to procedures, closed in the current env */

	Push( Expression_Register );
	Make_Procedure();

	Restore();
}


Public void Conditional_Eval()
{
	PC_Register = EVAL_CONDITIONAL;
	Save();

	/* Firstly, evaluate test predicate */

	Expression_Register = Get_Conditional_Test( Expression_Register );
	Goto( EVAL_EXPRESSION );
}



Public void Assignment_Eval()
{
	PC_Register = EVAL_ASSIGNMENT;
	Save();

	/* Firstly, evaluate value to be assigned (rhs) */

	Expression_Register = Get_Assignment_Rvalue( Expression_Register );
	Goto( EVAL_EXPRESSION );
}



Public void Definition_Eval()
{
	PC_Register = EVAL_DEFINITION;
	Save();

	/* Firstly, evaluate defining expression */

	Expression_Register = Get_Definition_Rvalue( Expression_Register );
	Goto( EVAL_EXPRESSION );
}



Public void Defmacro_Call_Eval()
{
	/* Evaluate the expanded form, of course */

	Expression_Register = Get_Defmacro_Call_Expansion( Expression_Register );
	Goto( EVAL_EXPRESSION );
}



Public void Sequence_Eval()
{
	Arguments_Register = Get_Sequence_Clauses( Expression_Register );

	/* Special case the empty sequence */

	if ( Arguments_Register == Nil )
	{
		Value_Register = Nil;
		Restore();
	}
	else
	{
		if (!Is_Pair(Arguments_Register))
		{
			Display_Error( "Body sequence must be a list:",
			    Arguments_Register );
		}

		Goto( EVAL_SEQUENCE );
	}
}



Public void Delay_Eval()
{
	Push( Get_Delay_Expression( Expression_Register ) );
	Push( Environment_Register );
	Make_Promise();
	Restore();
}



Public void Variable_Eval()
{
	if ( Is_Local_Variable( Expression_Register ) )
	{
		Object	env =	Environment_Register;
		Integer frame;

		for ( frame = 0; 
		      frame < Get_Variable_Frame_Number( Expression_Register );
		      frame++ ) env = Get_Environment_Frame_Previous(env);
		Value_Register = 
		    Get_Environment_Frame_Binding_Value( env,
			Get_Variable_Displacement( Expression_Register ));
	}
	else
	{
		Value_Register =
		    Get_Global_Binding( Get_Variable_Symbol(
						Expression_Register ) );
	}

	if ( Value_Register == The_Undefined_Symbol )
	{
		Error1( "`%s' is undefined" , 
		    Get_Symbol_Name(Get_Variable_Symbol(Expression_Register)));
	}
	Restore();  /* !!!!!! */
}



Public void State_Frame_Eval()
{
	Panic( "Attempt to evaluate a state frame" );
}



Private void Restore_Continuation_State( c )

	Object	c;
{
	Integer this_elem;

	State_Register = Get_Continuation_State( c ); /* State to be restored */
	Restore();

	for (this_elem = 0; this_elem < Arg_Stack_Ptr; this_elem++)
	{
		Arg_Stack[this_elem] = Get_Continuation_Stack_Elem(c,this_elem);	
	}
}



Private void Call_Primitive( f )

	Object	f ;	/* The Primitive (function) Object */
{
	Integer actual_arg_count = Get_Apply_Numargs( Expression_Register );
	Integer formal_arg_count = Get_Primitive_Numargs( f );
	Integer counter = 0; /* for type checking */

	/* Check number of arguments */

	if ( actual_arg_count!=formal_arg_count && formal_arg_count!=VARYING )
	{
		Display_Error( "Incorrect number of arguments to primitive: ",
		    Expression_Register );
	}


	/* Check types of the individual arguments (on stack) */

	if ( formal_arg_count == VARYING )
	{
	    formal_arg_count = actual_arg_count;
	    while ( formal_arg_count )
	    {
		if ( (Get_Type( Top( formal_arg_count ) ) !=
		    Get_Primitive_Argtypes(f,0)) &&
		    (Get_Primitive_Argtypes(f,0) != Any_Type) )
		{
			Display_Error( "Bad argument type to primitive in: ",
			    Expression_Register );
		}
		formal_arg_count-- ;
	    }
	}
	else
	{
	    while ( formal_arg_count )
	    {
		if ( (Get_Type( Top( formal_arg_count ) ) !=
		    Get_Primitive_Argtypes(f,counter)) &&
		    (Get_Primitive_Argtypes(f,counter) != Any_Type) )
		{
			Display_Error( "Bad argument type to primitive in: ",
			    Expression_Register );
		}
		counter++ ;
		formal_arg_count-- ;
	    }
	}

	(*Get_Primitive_Procedure(f))() ;	/* Invoke the C routine */

	Pop( actual_arg_count );
	Restore();
}



