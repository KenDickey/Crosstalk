/* compiler.c -- UMB Scheme, compiles Scheme expressions to abstract graphs.

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


#include	"portable.h"
#include	"eval.h"
#include	"object.h"
#include	"architecture.h"
#include	"compiler.h"
#include 	"steering.h"
#include 	"debug.h"

/*

In general, the compilation routines take their input expressions from atop
the stack and leave the target graphs in the Value_Register.

*/

#define Extend_Compiler_Environment( frame ) \
	{	Get_Environment_Frame_Previous(frame) = Environment_Register;\
		Environment_Register = frame; }

#define Restore_Compiler_Environment() \
		Environment_Register = \
			Get_Environment_Frame_Previous(Environment_Register);

Private void	Compile_Arguments();
Private	void	Lookup_Address();
Private Object	Scanned_Internal_Defns();

	

Public void Self_Compile()
{
	Value_Register = Top( 1 ) ;
	Pop( 1 ) ;
}


Public void Compile_The_Empty_Object()
{
	Error( "Unquoted ()" ); 
}


Public void Compile_Form()
{
	/* The expression to be compiled is a list of the form

		(operator ...)
	*/

	Object	form = Top( 1 ) ;
	Object	operator = First( form ) ;

	if ( Is_Symbol( operator ) )
	{
		if ( operator == QUOTE_Symbol )
		{
			/* form = (quote expr) */

			if ( Length( form ) != 2 )
			{
			    Display_Error("Bad syntax to quote in: ", form);
			}

			Value_Register = Second( form ) ;
		}
		else if ( operator == DEFINE_Symbol )
		{
		    Object name ;

		    /* form = (define name expr) */

		    if ( Length( form ) <  2 )
		    {
			Display_Error("Bad syntax to define in: ", form);
		    }

		    name = Second( form );

		    if ( Is_Pair( name ) )
		    {
			Object formals = Rest( name );

			/* Transform: (define (name . formals) . body)
			    => (define name (lambda formals . body))
			*/

			name = First( name );

			Push( DEFINE_Symbol );
			Push( name );
			Push( LAMBDA_Symbol );
			Push( formals );
			Push( Rest( Rest( form )));

			Make_Pair();
			Push( Value_Register );	/* (formals . body ) */
			Make_Pair();
			Push( Value_Register );	/* (lambda formals . body) */
			Push( Nil );
			Make_Pair();
			Push( Value_Register );	/* ((lambda formals .body)) */
			Make_Pair();
			Push( Value_Register );	/* (name (lambda ...)) */
			Make_Pair();
			Push( Value_Register );	/* (define name (lambda...)) */

			Compile_Object( Top(1) ); /* Now, compile THAT! */
		    }
		    else
		    {
			/* Basic form: (define name expr) */

			Object	expr ; 

			if ( Length( form ) != 3 )
			{
			    Display_Error("Bad syntax to define in: ", form);
			}

			expr = Third( form );

			if ( !Is_Symbol( name ) )
			{
			    Display_Error("Bad syntax to define in: ", form);
			}

			if (Get_Global_Binding(name) == The_Syntactic_Keyword)
			{
			    Error1( "`%s' cannot be used as a variable.",
					Get_Symbol_Name(name) );
			}

			if ( Debugging && 
			     Environment_Register == 	
				Get_State_Frame_Environment( State_Debugged ) )
			{
				Lookup_Address( name , The_Global_Environment );
			}
			else
			{
			    Lookup_Address( name , Environment_Register );
			    if ( Is_Local_Variable( Value_Register ) )
			    {
				if (Get_Variable_Frame_Number(Value_Register)!=0)
			    	Display_Error( "Bad internal definition: ",
						form );
			    }
			    else if ((Environment_Register!=
						The_Global_Environment))
			    {
			    	Display_Error( "Bad internal definition: ",form);
			    }
			}

			Push( Value_Register );

			Push( expr );
			Compile_Object( Top( 1 ));
			Push( Value_Register );

			Make_Definition();
		    }
		}
		else if ( operator == SET_Symbol )
		{
			/* form = (set! name expr) */

			if ( Length( form ) != 3 || !Is_Symbol(Second(form)) )
			{
			    Display_Error("Bad syntax to set! in: ", form);
			}

			if (Get_Global_Binding(Second(form)) ==
				The_Syntactic_Keyword)
			{
			    Error1( "`%s' cannot be used as a variable.",
					Get_Symbol_Name(Second(form)) );
			}


			Lookup_Address( Second( form ) , Environment_Register );
			Push( Value_Register );

			Push( Third( Top( 2 ) ) ); /* expr */
			Compile_Object( Top( 1 ));
			Push( Value_Register ); 

			Make_Assignment();
		}
		else if ( operator == IF_Symbol )
		{
			/* form = (if test consequent alternative)
			     or   (if test consequent)
			*/

			if ( Length( form ) < 3 || Length( form ) > 4 )
			{
			    Display_Error("Bad syntax to if in: ", form);
			}

			Push( Second( form )); /* form now = Top(2) */
			Compile_Object( Top( 1 ));
			Push( Value_Register );	/* test on stack */

			Push( Third( Top(2)) ); /* form now = Top(3) */
			Compile_Object( Top( 1 ));
			Push( Value_Register );	/* consequent on stack */

			if (Length( Top(3) ) == 4) 
			{
				/* alternative supplied in form */

				Push( Fourth( Top(3) )); 
				Compile_Object( Top( 1 ));
				Push( Value_Register );
			}
			else
			{
				/* no alternative in form; use () instead */

				Push( Nil );
			}			/* alternative on stack */

			Make_Conditional();
		}
		else if ( operator == DEFMACRO_Symbol )
		{
		        /* form = (defmacro keyword formals . clauses) */

		        if ( Length( form ) < 3 || !Is_Symbol(Second(form)))
			{
			  Display_Error("Bad syntax to defmacro in: ", form);
			}
			else
			{
			  Object keyword = Second( form );
			  Object formals;
			  Object clauses;

			  Make_Global_Variable( keyword ); /* defined keyword */
			  Push( Value_Register );

			  /* The form might be affected by GC. */

			  form = Top( 2 );
			  keyword = Second( form );
			  formals = Third( form );
			  clauses = Rest( Rest( Rest( form ) ) );

			  /* The Defmacro Object */

			  Push( keyword ); /* keyword on stack */

			  Push( LAMBDA_Symbol );  /* the transformer */
			  Push( formals );
			  Push( clauses );
			  Make_Pair();
			  Push( Value_Register ); /* (formals . body) */
			  Make_Pair();
			  Push( Value_Register ); /* (lambda formals . body) */
			
			  Compile_Object( Top ( 1 ));
			  Push( Value_Register ); /* transformer on stack */
			  Make_Defmacro();

			  Push( Value_Register );  /* the defmacro */
			  Make_Definition();
			}
		}
		else if ( operator == BEGIN_Symbol )
		{
			/* form = (begin . expr-sequence) */

			Push( Rest( form ));
			Compile_Arguments();
			Push( Value_Register );	/* expr-sequence on stack */
			Make_Sequence(TRUE);
		}
		else if ( operator == DELAY_Symbol )
		{
			/* form = (delay expr) */

			if ( Length( form ) != 2 )
			{
			    Display_Error("Bad syntax to delay in: ", form);
			}

			Push( Second( form ));
			Compile_Object( Top( 1 ));
			Push( Value_Register ); /* expr on stack */
			Make_Delay();
		}
		else if ( operator == LAMBDA_Symbol )
		{
			/* form = (lambda formals . body ) */

			Object 	formals = Second( form );
			Object	formal_check;
			Boolean	internal_definitions = FALSE;
                        
                        formal_check = formals;
                        while (Is_Pair(formal_check))
                        {
                           if (! Is_Symbol(First(formal_check)))
                           {
				Display_Error("Formals must be symbols", 
	                                    First(formal_check));
                           }
			   else if ( Member( First(formal_check),
					     Rest(formal_check) ))
			   {
				Display_Error( "Name duplicated in formals: ",
						First( formal_check ) );
			   }
			   formal_check = Rest( formal_check );

			}
                        if (! Is_Symbol(formal_check) && formal_check != Nil)
                        {
                           Display_Error("Bad syntax for formal arguments",
                           		 formals);
                        }
                           
			Push( formals );	/* formals */
			Make_Symbol_Frame();
			Extend_Compiler_Environment( Value_Register );
			Push( Value_Register );


			Push( Rest( Rest( Top(2) ))); 	/* body */
			formals = Scanned_Internal_Defns( Top(1) );
			if ( formals != Nil )
			{
				internal_definitions = TRUE;
				Push( formals );
				Make_Symbol_Frame();
				Extend_Compiler_Environment( Value_Register );
			}

			Compile_Arguments(); 	/* ie the body -- clause list */
			Push( Value_Register );	

			if ( internal_definitions )
			{
				/* The body contains internal definitions; we
				   transform it to a new body as follows:

				   If body =

					(   (define x1 e1)
					    (define x2 e2)
					    ...
					    (define xn en) ...)

				   then we transform it to the new body:

					( ( (lambda (x1 x2 ... xn) body)
					    	?1 ?2 ... ?n) . () )

				   Currently,

					body is atop the stack, ie Top(1)
					formals = (xn ... x2 x1) 
				*/

				Integer numargs;
				Integer args;

				formals = Environment_Register;
				numargs = Get_Environment_Frame_Size( formals );
				Restore_Compiler_Environment();

				Push( formals );	/* (x1 ... xn) */
				Push( Top(2) );		/* orig body */
				Make_Sequence(FALSE);
				Push( Value_Register );
				Make_Lambda();
				Push( Value_Register );	/* (lambda (x1..xn)..)*/

				args = numargs;
				while (args--) Push( The_Undefined_Symbol );
				Push( Nil );
				args = numargs;
				while (args--)
				{
					Make_Pair();
					Push( Value_Register );
				}			/* (?1 ... ?n) */

				Make_Apply();
				Push( Value_Register );
				Push( Nil );
				Make_Pair();		/* the new body */
				Top(1) = Value_Register;/* replaces orig body */
			}

			Restore_Compiler_Environment();
			Make_Sequence(FALSE);	/* make the body a sequence */
			Push( Value_Register );

			Make_Lambda();
		}
		else /* operator is not special */
		{
			/* (operator . arguments) */

			Value_Register = Get_Global_Binding( operator );
			if ( Is_Defmacro( Value_Register ))
			{
				/* NB: Defmacros are declared only in the global
				       environment.
				*/

				/* Expand the defmacro call; compile result */

				Boolean save_activation = Debugger_Activated;

				Push( form ); /* Original code */

				Push( Get_Defmacro_Transformer( Value_Register ) );
				Push( Rest( form ) );
				Make_Apply();  /* (transformer Rest(form)) */

				Push( Value_Register );
				Save(); /* !!!! */
				Debugger_Activated = FALSE;
				Eval( Top( 1 ), The_Global_Environment );
				Debugger_Activated = save_activation;
				Restore(); /* !!!! */
				Pop( 1 );
				Push( Value_Register );	/* Expansion on stack */

				Compile_Object(Top(1)); /* Now, compile THAT! */
				Push( Value_Register );
				Make_Defmacro_Call();  /* from orig. & expansion */
			}
			else  /* An application */
			{
				Push( operator );
				Compile_Object( Top( 1 ));
				Push( Value_Register );	/* operator on stack */

				Push( Rest( Top( 2 ) ));
				Compile_Arguments();
				Push( Value_Register ); /* arguments on stack */

				Make_Apply();
			}
		}
	}
	else /* operator is not a symbol -- treat as an application */
	{
		/* (operator . arguments) */

		Push( operator );
		Compile_Object( Top( 1 ));
		Push( Value_Register );	/* operator on stack */

		Push( Rest( Top( 2 ) ));
		Compile_Arguments();
		Push( Value_Register ); /* arguments on stack */

		Make_Apply();
	}
	Pop(1);	/* Original form */
}




Private void Compile_Arguments()
{
	/* Compile the list of arguments (or clauses) that are atop stack;
	   leave the resulting (compiled) list in Value_Register.
	*/

	if ( !Is_List( Top( 1 )) )
	{
		Display_Error( "Syntax : list expected by compiler: ", Top(1) );
	}
	else if ( Top( 1 ) == Nil )
	{
		Value_Register = Nil;
	}
	else  /* A non-empty list */
	{
		Push( First( Top( 1 )));
		Compile_Object( Top( 1 ));
		Push( Value_Register );	/* First (compiled) element on stack */

		Push( Rest( Top( 2 )));	
		Compile_Arguments();
		Push( Value_Register ); /* Rest of (compiled) list on stack */

		Make_Pair();		/* Compiled list in Value_Register */
	}
	Pop( 1 );  /* Original form */
}



Public void Compile_Symbol()
{
	Lookup_Address( Top(1), Environment_Register );
	Pop(1);
}



Private void Lookup_Address(symbol, env)

	Object symbol, env;
{
	Integer	frame , displacement;

	if (! Is_Symbol(symbol) )
	{
		Panic("Lookup_Address called with bad symbol argument");
	}
	else if ( ! Is_Environment_Frame(env) )
	{
		Panic( "Lookup_Address called with bad environment argument" );
	}

	frame = 0;
	while (env != The_Global_Environment)
	{
		for (displacement = 0; displacement < 
		            Get_Environment_Frame_Size(env);  displacement++)
		{
			if (Get_Environment_Frame_Binding_Symbol
			                (env,displacement) == symbol)
			{
				Make_Local_Variable(symbol,frame,displacement);
				return;
			}
		}
		env = Get_Environment_Frame_Previous( env );
		frame++;
	}

	Make_Global_Variable( symbol );
}

Private Object Scanned_Internal_Defns( body )

	Object body; /* not yet compiled */
{
	if ( Is_Pair(body) )
	{

		Object clause = First( body );

		if  ( Is_Pair( clause ) && First( clause ) == DEFINE_Symbol )
		{
			clause = Second( clause );
			Push( Is_Symbol( clause ) ? clause :
			      Is_Pair( clause ) && Is_Symbol( First(clause) ) 
				? First( clause ): The_Undefined_Symbol );
			Push( Scanned_Internal_Defns( Rest( body ) ) );
			Make_Pair();
			return ( Value_Register );
		}
		/* more lenient than the language allows ... */
                else return ( Scanned_Internal_Defns( Rest( body ) )  );
	}
	return ( Nil );
}
