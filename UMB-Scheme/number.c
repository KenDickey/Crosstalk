/* number.c - UMB Scheme, numbers package 

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
#include "number.h"
#include "fixnum.h"
#include "bignum.h"
#include "rational.h"
#include "real.h"
#include "complex.h"
#include "steering.h"
#include "io.h"
#include <math.h>

/* Conversions used in promotion */

Private void Coerce_Args();
Private void Coerce_Relational_Args();

/* Conversions used in demotion */

Private void Demote_Complex_To_Real();
Private void Demote_Rational_To_Integer();
Private void Demote_Bignum_To_Fixnum();

/* All the number operations. */

typedef void (*Procedure_Pointer)();
typedef Procedure_Pointer Procedure_Array[ TOWER_LEVEL_COUNT ];

typedef Boolean (*Boolean_Function_Pointer)();
typedef Boolean_Function_Pointer Boolean_Function_Array[ TOWER_LEVEL_COUNT ];

Private struct 
{
					/* Predicates */
	Boolean_Function_Array Is_Number_Zero;
	Boolean_Function_Array Is_Number_Positive;
	Boolean_Function_Array Is_Number_Negative;
	Boolean_Function_Array Is_Number_Even;
	Boolean_Function_Array Is_Number_Odd;
	Boolean_Function_Array Is_Number_Exact;
	Boolean_Function_Array Is_Number_Inexact;

					/* Comparisons */
	Boolean_Function_Array Number_Equal;
	Boolean_Function_Array Number_Less_Than;
	Boolean_Function_Array Number_Greater_Than;
	Boolean_Function_Array Number_Less_Than_Or_Equal;
	Boolean_Function_Array Number_Greater_Than_Or_Equal;

					/* Arithmetic. */
	Procedure_Array Number_Add;
	Procedure_Array Number_Subtract;
	Procedure_Array Number_Multiply;
	Procedure_Array Number_Divide;
	Procedure_Array Number_Quotient;
	Procedure_Array Number_Remainder;
	Procedure_Array Number_Modulo;
	Procedure_Array Number_Negate;
	Procedure_Array Number_Abs;
	Procedure_Array Number_Numerator;
	Procedure_Array Number_Denominator;
	Procedure_Array Number_Rationalize;

					/* Others. */
	Procedure_Array Number_Max;
	Procedure_Array Number_Min;
	Procedure_Array Number_GCD;
	Procedure_Array Number_LCM;


	Procedure_Array Number_Floor;
	Procedure_Array Number_Ceiling;
	Procedure_Array Number_Truncate;
	Procedure_Array Number_Round;


	Procedure_Array Number_Sqrt;
	Procedure_Array Number_Exp;
	Procedure_Array Number_Log;
	Procedure_Array Number_Expt;


	Procedure_Array Number_Sin;
	Procedure_Array Number_Cos;
	Procedure_Array Number_Tan;
	Procedure_Array Number_Asin;
	Procedure_Array Number_Acos;
	Procedure_Array Number_Atan;
	Procedure_Array Number_Atan2;

	Procedure_Array Number_Exact_To_Inexact;
	Procedure_Array Number_Inexact_To_Exact;
	Procedure_Array Number_To_String;

	Procedure_Array Number_Make_Rectangular;
	Procedure_Array Number_Make_Polar;
	Procedure_Array Number_Real_Part;
	Procedure_Array Number_Imaginary_Part;
	Procedure_Array Number_Magnitude;
	Procedure_Array Number_Angle;

} Num_Ops;

/* Basic Predicates on Numbers */

/* (number? object) */

Private void Number_Predicate()
{
   Value_Register = Is_Number(Top(1)) ? The_True_Object
                                      : The_False_Object;
}


/* (integer? object) */

Private void Integer_Predicate()
{
	if ( Is_Number( Top(1) ) )
	{
		Integer p1 = Get_Number_Tower_Position( Top(1) );

		if ( p1 <= BIGNUM_LEVEL )
			Value_Register = The_True_Object;
		else if ( p1 == REAL_LEVEL )
		{
			Push( Top(1) );
			Number_Round();
			Replace( 1 , Value_Register );
			Number_Equal(); Pop(1); /* just the rounded one */
		}
		else Value_Register = The_False_Object;
	}
	else Value_Register = The_False_Object;
}


/* (rational? object) */

Private void Rational_Predicate()
{
	if ( Is_Number( Top(1) ) )
	{
		Integer p1 = Get_Number_Tower_Position( Top(1) );

		if ( p1 <= RATIONAL_LEVEL )
			Value_Register = The_True_Object;
		else if ( p1 == REAL_LEVEL )
		{
			Push( Top(1) );
			Number_Round();
			Replace( 1 , Value_Register );
			Number_Equal(); Pop(1); /* just the rounded one */
		}
		else Value_Register = The_False_Object;
	}
	else Value_Register = The_False_Object;
}

/* (real? object) */

Private void Real_Predicate()
{
   Value_Register = Is_Number(Top(1)) &&
                    Get_Number_Tower_Position(Top(1)) <= REAL_LEVEL
                    ? The_True_Object
                    : The_False_Object;
}

/* (complex? object) */

Private void Complex_Predicate()
{
   Value_Register = Is_Number(Top(1)) &&
                    Get_Number_Tower_Position(Top(1)) <= COMPLEX_LEVEL
                    ? The_True_Object
                    : The_False_Object;
}




/* Generic Number Procedures - invoke more specific procedures via Num_Ops */


Public void Is_Number_Zero()
{
    Value_Register =
	(*(Num_Ops.Is_Number_Zero[Get_Number_Tower_Position( Top(1) )]))()
	    	? The_True_Object
	    	: The_False_Object;
}

					


Public void Is_Number_Positive()
{
    	
    Value_Register =
	(*(Num_Ops.Is_Number_Positive[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					


Public void Is_Number_Negative()
{
    Value_Register =
	(*(Num_Ops.Is_Number_Negative[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					


Public void Is_Number_Odd()
{
    Value_Register =
	(*(Num_Ops.Is_Number_Odd[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					


Public void Is_Number_Even()
{
    Value_Register =
	(*(Num_Ops.Is_Number_Even[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					


Public void Is_Number_Exact()
{
    Value_Register =
	(*(Num_Ops.Is_Number_Exact[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					


Public void Is_Number_Inexact()
{
    Value_Register =
	(*(Num_Ops.Is_Number_Inexact[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

/* Relations of the form (rel obj obj obj ...) */

Private Object Iterate_Over_Relations( Relation_Tower )
        
	Boolean_Function_Array Relation_Tower;
{
	/* In (rel obj obj ...) apply rel to successive obj pairs;
	   thus eg (> x y z) is the same as (and (> x y) (> y z)). */

	Integer arg_count = Get_Apply_Numargs( Expression_Register );

	if (arg_count < 2 )
	{
		Display_Error( "Fewer than 2 arguments to a relation: " ,
			       Expression_Register );
	}
	
	while ( arg_count > 1 )
	{
       	       Push( Top( arg_count ) );
	       Push( Top( arg_count ) );
	       Coerce_Relational_Args();
	       
	       if ( (*(Relation_Tower[Get_Number_Tower_Position(Top(1))]))() )
	       {
		       Pop( 2 );
		       arg_count--;
	       }
	       else
	       {
		       Pop( 2 );
		       return( The_False_Object );
	       }
       }
	return( The_True_Object );
}



Private void Varying_Number_Equal()
{
    Value_Register = Iterate_Over_Relations( Num_Ops.Number_Equal );
}



Private void Varying_Number_Greater_Than()
{
    Value_Register = Iterate_Over_Relations( Num_Ops.Number_Greater_Than );
}



Private void Varying_Number_Less_Than()
{
    Value_Register = Iterate_Over_Relations( Num_Ops.Number_Less_Than );
}



Private void Varying_Number_Greater_Than_Or_Equal()
{
    Value_Register =
	Iterate_Over_Relations( Num_Ops.Number_Greater_Than_Or_Equal );
}



Private void Varying_Number_Less_Than_Or_Equal()
{
    Value_Register =
	Iterate_Over_Relations( Num_Ops.Number_Less_Than_Or_Equal );
}




Public void Number_Equal()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    Value_Register =
	(*(Num_Ops.Number_Equal[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					



Public void Number_Less_Than()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    Value_Register =
	(*(Num_Ops.Number_Less_Than[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					



Public void Number_Greater_Than()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    Value_Register =
	(*(Num_Ops.Number_Greater_Than[Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					



Public void Number_Less_Than_Or_Equal()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    Value_Register =
	(*(Num_Ops.Number_Less_Than_Or_Equal[
				Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

					



Public void Number_Greater_Than_Or_Equal()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    Value_Register =
	(*(Num_Ops.Number_Greater_Than_Or_Equal[
				Get_Number_Tower_Position( Top(1) )]))()
	    ? The_True_Object
	    : The_False_Object;
}

/* Numerical operations with varying numbers of arguments */


Private void Iterate_Over_Operands( Arg_Count , Op_Tower )

	Integer Arg_Count;
	Procedure_Array Op_Tower;
{
	while ( Arg_Count > 1 )
	{
		Push( Value_Register );
		Push( Top( Arg_Count ) );
		Coerce_Args();
		(*(Op_Tower[Get_Number_Tower_Position( Top(1) )]))();
		Pop(2);
		Arg_Count--;
	}
	Demote();
}



Private void Varying_Number_Add()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count, Num_Ops.Number_Add );
	}
	else if ( arg_count == 1 )
		Value_Register = Top( 1 );
	else
		Make_Fixnum_Number( 0 );
}



Private void Varying_Number_Subtract()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count , Num_Ops.Number_Subtract);
	}
	else if (arg_count == 1 )
	{
		Make_Fixnum_Number( 0 );  /* treat as (- 0 arg) */
		Iterate_Over_Operands( 2 , Num_Ops.Number_Subtract);
	}
	else 
		Display_Error( "At least 1 argument required in: " ,
			       Expression_Register );
}



Private void Varying_Number_Multiply()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count, Num_Ops.Number_Multiply );
	}
	else if ( arg_count == 1 )
		Value_Register = Top( 1 );
	else
		Make_Fixnum_Number( 1 );
}



Private void Varying_Number_Divide()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count , Num_Ops.Number_Divide );
	}
	else if (arg_count == 1 )
	{
		Make_Fixnum_Number( 1 );  /* treat as (/ 1 arg) */
		Iterate_Over_Operands( 2 , Num_Ops.Number_Divide );
	}
	else 
		Display_Error( "At least 1 argument required in: " ,
			       Expression_Register );
}



Private void Varying_Number_Min()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count , Num_Ops.Number_Min );
	}
	else if (arg_count == 1 )
	{
		Value_Register = Top(arg_count);
	}
	else 
		Display_Error( "At least 1 argument required in: " ,
			       Expression_Register );
}



Private void Varying_Number_Max()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count , Num_Ops.Number_Max );
	}
	else if (arg_count == 1 )
	{
		Value_Register = Top(1);
	}
	else 
		Display_Error( "At least 1 argument required in: " ,
			       Expression_Register );
}



Private void Varying_Number_GCD()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count, Num_Ops.Number_GCD );
	}
	else if ( arg_count == 1 )
	{
		Make_Fixnum_Number( 0 );
		Iterate_Over_Operands( 2 , Num_Ops.Number_GCD );
	}
	else
		Make_Fixnum_Number( 0 );
}



Private void Varying_Number_LCM()
{
	Integer arg_count = Get_Apply_Numargs( Expression_Register );
    
	if ( arg_count >= 2 )
	{
		Value_Register = Top(arg_count);
		Iterate_Over_Operands( arg_count, Num_Ops.Number_LCM );
	}
	else if ( arg_count == 1 )
	{
		Make_Fixnum_Number( 0 );
		Iterate_Over_Operands( 2 , Num_Ops.Number_LCM );
	}
	else
		Make_Fixnum_Number( 1 );
}



Public void Number_Add()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Add[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Subtract()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Subtract[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Multiply()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Multiply[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Divide()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Divide[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Quotient()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Quotient[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Remainder()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Remainder[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Modulo()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Modulo[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}



Public void Number_Negate()
{
    (*(Num_Ops.Number_Negate[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Abs()
{
    (*(Num_Ops.Number_Abs[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Numerator()
{
    (*(Num_Ops.Number_Numerator[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Denominator()
{
    (*(Num_Ops.Number_Denominator[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}


/*
Private void SS( label )

	String	label;
{
	Integer	i;
	Output( "\n" );
	Output( label );
	Output( ": " );
	for ( i = 1 ; i <= 10 && i <= Arg_Stack_Ptr; i++ )
	{
		Show_Object( Top(i) , 12 );
		Output( ", " );
	}
}
*/



Private void Real_To_Integer( real )

	Double real;
{
	Double  quotient;
	Integer remainder;
	quotient = floor( real / RADIX );
	remainder = real - (quotient * RADIX);

	Integer_To_Number( remainder );

	if ( quotient > 0.0 )
	{
		Push( Value_Register );
		Real_To_Integer( quotient );
		Push( Value_Register );
		Integer_To_Number( RADIX );
		Push( Value_Register );
		Number_Multiply(); 
		Pop( 2 );
		Push( Value_Register );
		Number_Add(); 
		Pop( 2 );
	}
}
	

Public void SS( label )
	String	label;
{
	Integer i;
	Output( "\n" ); Output( label ); Output( ": " );
	for ( i = 1; i <= 8 && i <= Arg_Stack_Ptr; i++ );
	{
		Show_Object( Top(i), 1 ); 
		if ( i < 8 && i < Arg_Stack_Ptr ) Output( ", " );
	}

}


Public void Number_Rationalize()	/* (rationalize x eps) */
{
	Boolean negative_input = FALSE;
	Boolean exact = Is_Exact_Number( Top(1) ) && Is_Exact_Number( Top(2) );

	/* Check Domains of Inputs */

	if  ( Is_Complex(Top(1)) )
	{
		Display_Error( "Complex argument to rationalize: " , Top(1) );
	}
	else if  ( Is_Complex(Top(2)) )
	{
		Display_Error( "Complex argument to rationalize: " , Top(2) );
	}

	/* Work with absolute values */

	Push( Top( 1 ) );
	Number_Abs(); Pop(1);
	Replace( 1 , Value_Register );	/* |eps| */
	Push( Top(2) );
	Is_Number_Negative(); Pop(1);
	negative_input = (Value_Register == The_True_Object);
	Push( Top( 2 ) );
	Number_Abs(); Pop(1);
	Replace( 2 , Value_Register );	/* |x| */

	/* Construct the continued fraction */

	Push( Value_Register );		/* y = x */
	Push( Value_Register );
	Number_Truncate(); Pop(1);
	if ( Is_Real( Value_Register ) )
	{
		Real_To_Integer( Get_Number_Real_Value( Value_Register ) );
	}
	Push( Value_Register );		/* a = truncate( |x| */
	Push( Value_Register );		/* num = truncate( |x| ) */
	Integer_To_Number( 1 );
	Push( Value_Register );		/* den = 1 */
	Push( Value_Register );		/* oldnum = 1 */
	Integer_To_Number( 0 );	
	Push( Value_Register );		/* oldden = 0 */

	/* Stack =  oldden, oldnum, den, num, a, y, |epsilon|, |x| */

	while ( TRUE )
	{
		/* Rational found when |x - num/den| <= |epsilon| */

		Push( Top(3) );
		Is_Number_Zero(); Pop(1);
		if ( Value_Register == The_False_Object )
		{
			/* den != 0  */

			Push( Top(8) );	/* x */
			Push( Top(5) );	/* num */
			Push( Top(5) );	/* den */
			Make_Rational_Number(); Pop(2);
			Push( Value_Register );
			Number_Subtract();  Pop(2);
			Push( Value_Register );
			Number_Abs(); Pop(1);
			Push( Value_Register );	/* |x - num/den| */
			Push( Top(8) );		/* |epsilon| */
			Number_Less_Than_Or_Equal(); Pop(2);

			if ( Value_Register == The_True_Object )
			{
				/* |x - num/den| <= |epsilon| */

				Push( Top(4) );	/* num */
				Push( Top(4) );	/* den */
				Make_Rational_Number();	/* num/den */
				Pop(2);

				/* Negate if necessary */
		
				if ( negative_input )
				{
					Push( Value_Register );
					Number_Negate(); Pop(1);
				}
				Is_Exact_Number( Value_Register ) = exact;
				Pop(6);
				return;
			}
		}

		/* Otherwise, compute the next fraction */

		/* Stack =  oldden, oldnum, den, num, a, y, |epsilon|, |x| */

		Integer_To_Number( 1 );
		Push( Value_Register );	/* 1 */
		Push( Top(7) );		/* y */
		Push( Top(7) );		/* a */
		Number_Subtract(); Pop(2);
		Push( Value_Register );
		Number_Divide(); Pop(2);
		Replace( 6 , Value_Register );	/* y = 1/(y - a)  */

		Push( Value_Register );
		Number_Truncate(); Pop(1);
		if ( Is_Real( Value_Register ) )
		{
		    Real_To_Integer( Get_Number_Real_Value( Value_Register ) );
		}
		Replace( 5 , Value_Register );	/* a = truncate( y )  */

		Push( Top(2) );	/* oldnum */
		Push( Top(6) );	/* a */
		Push( Top(6) ); /* num */
		Number_Multiply(); Pop(2);
		Push( Value_Register );
		Number_Add(); Pop(2);		/* newnum = oldnum + a * num  */

		Replace( 2 , Top(4) );		/* oldnum = num  */
		Replace( 4 , Value_Register );	/* num = newnum  */

		Push( Top(1) );	/* oldden */
		Push( Top(6) );	/* a */
		Push( Top(5) ); /* den */
		Number_Multiply(); Pop(2);
		Push( Value_Register );
		Number_Add(); Pop(2);		/* newden = oldden + a * den  */

		Replace( 1 , Top(3) );		/* oldden = den  */
		Replace( 3 , Value_Register );	/* den = newden  */
	}
}



Public void Number_Max()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Max[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_Min()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Min[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}

					


Public void Number_GCD()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_GCD[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}



Public void Number_LCM()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_LCM[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}




Public void Number_Floor()
{
    (*(Num_Ops.Number_Floor[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Ceiling()
{
    (*(Num_Ops.Number_Ceiling[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Truncate()
{
    (*(Num_Ops.Number_Truncate[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Round()
{
    (*(Num_Ops.Number_Round[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}




Public void Number_Sqrt()
{
    (*(Num_Ops.Number_Sqrt[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Exp()
{
    (*(Num_Ops.Number_Exp[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Log()
{
    (*(Num_Ops.Number_Log[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Expt()
{
    /* Must check one domain restriction here before coercion occurs */

    if(Get_Number_Tower_Position(Top(1)) > BIGNUM_LEVEL)
    {
	    Push(Top(2));
	    Is_Number_Negative();
	    if(Value_Register == The_True_Object)
	    {
	        Pop(1);
		Error("Domain error for expt");
	    }
	    Pop(1);
    }

    /* Don't coerce args.  Pick specific routine based only on exponent */

    (*(Num_Ops.Number_Expt[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Sin()
{
    (*(Num_Ops.Number_Sin[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}




Public void Number_Cos()
{
    (*(Num_Ops.Number_Cos[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Tan()
{
    (*(Num_Ops.Number_Tan[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Asin()
{
    (*(Num_Ops.Number_Asin[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Acos()
{
    (*(Num_Ops.Number_Acos[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Atan()
{
    (*(Num_Ops.Number_Atan[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Atan2()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Atan2[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}



Public void Number_Exact_To_Inexact()
{
    (*(Num_Ops.Number_Exact_To_Inexact[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Inexact_To_Exact()
{
    (*(Num_Ops.Number_Inexact_To_Exact[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_To_String()
{
    (*(Num_Ops.Number_To_String[Get_Number_Tower_Position( Top(2) )]))();
}

					

Public void Number_Make_Rectangular()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Make_Rectangular[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}



Public void Number_Make_Polar()
{
    Coerce_Args();	/* Promote arg of lower tower posn to posn of other. */

    (*(Num_Ops.Number_Make_Polar[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;		/* To lowest possible position (in Value_Register) */
}



Public void Number_Real_Part()
{
    (*(Num_Ops.Number_Real_Part[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Imaginary_Part()
{
    (*(Num_Ops.Number_Imaginary_Part[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Magnitude()
{
    (*(Num_Ops.Number_Magnitude[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public void Number_Angle()
{
    (*(Num_Ops.Number_Angle[Get_Number_Tower_Position( Top(1) )]))();

    Demote() ;	/* To lowest possible position (in Value_Register) */
}



Public Integer Number_Sign(num)
	Object num;

{
	Tower_Position argtype = Get_Number_Tower_Position(num);

	if( argtype == FIXNUM_LEVEL )
	{
		return ((Integer) (Get_Number_Fixnum_Value(num)) );
	}

	else if (argtype == BIGNUM_LEVEL)
	{
		return (Get_Number_Digit((num), Get_Number_Length(num) -1));
	}

	else
	{
		Panic("Incorrect argument for Number_Sign");
		return 0;
	}

}

/* PROMOTION */

Private void Coerce_Args()
{
	/* Coerce Argument of lower tower position to the (higher) position
	   of the other argument.
	*/

	Tower_Position	p1 = Get_Number_Tower_Position( Top(1) ),
			p2 = Get_Number_Tower_Position( Top(2) );
	if (p1 != p2)
	{
		if (p1 < p2)
			Promote( 1 , p2 );
		else
			Promote( 2 , p1 );
	}
}

Private void Coerce_Relational_Args()
{
	/* Coerce Argument of lower tower position to the (higher) position
	   of the other argument.
	*/

	Tower_Position	p1 = Get_Number_Tower_Position( Top(1) ),
			p2 = Get_Number_Tower_Position( Top(2) );

	if (p1 != p2)
	{
		/* If comparing inexacts to exacts then we want to 
		   force the inexacts to be exacts */

		if (p1 == REAL_LEVEL && p2 <= RATIONAL_LEVEL)
		{
			Number_Inexact_To_Exact();
			Replace( 1 , Value_Register );
		}
		else if (p2 == REAL_LEVEL && p1 <= RATIONAL_LEVEL)
		{
			Push( Top(2) );
			Number_Inexact_To_Exact(); Pop(1);
			Replace( 2 , Value_Register );
		}

		p1 = Get_Number_Tower_Position( Top(1) );
	       	p2 = Get_Number_Tower_Position( Top(2) );

		if (p1 < p2)
			Promote( 1 , p2 );
		else if (p2 < p1)
			Promote( 2 , p1 );
	}
}


/* Promote a number to the indicated tower_position |level|. */

Public void Promote( stkpos, level)
	
	Integer	 stkpos;
        Tower_Position level;
        
{

	switch (Get_Number_Tower_Position(Top(stkpos)))                
	{
        	case FIXNUM_LEVEL:
                {
	                if (level == FIXNUM_LEVEL) break;


			else if (level == REAL_LEVEL)
			{
				Short val ;
				val = Get_Number_Fixnum_Value(Top(stkpos));
				Make_Real_Number( (Double) val);
				Replace( stkpos , Value_Register );
				break;
			}
			else
			{
				Make_Bignum_Number(1);
				Get_Number_Digit(Value_Register,0) = 
					Get_Number_Fixnum_Value( Top(stkpos) );
				Replace( stkpos , Value_Register );
			}
                }

        	case BIGNUM_LEVEL:
                {
	                if (level == BIGNUM_LEVEL) break;

			else if (level == REAL_LEVEL)
			{
				Double temp_real = 0;
				Integer this_digit;
				Double	cumulative_radix = 1.0;
				Object	num = Top(stkpos);

				for (this_digit = 0; 
					this_digit < Get_Number_Length(num); 
					this_digit++)
				{
				    temp_real += 
					cumulative_radix *
					  ((Get_Number_Digit(num,this_digit)>=0)
		    			   ?  Get_Number_Digit(num,this_digit)
		    			   : -Get_Number_Digit(num,this_digit));
				    cumulative_radix *= RADIX;
				}

				if (Number_Sign(num) < 0)
					temp_real = - temp_real;

				Make_Real_Number(temp_real);
				Replace( stkpos , Value_Register );
				break;
			}
			else
			{
				Push( Top(stkpos) );
				Make_Small_Bignum( 1 );
				Push( Value_Register );
				Make_Rational_Number();
				Pop( 2 );
				Replace( stkpos , Value_Register );
			}
                }

		case RATIONAL_LEVEL:
                {
	                if (level == RATIONAL_LEVEL) break;

			Push( Get_Number_Rational_Numerator( Top(stkpos) ) );
			Push( Get_Number_Rational_Denominator( Top(stkpos+1) ) );
			Promote( 1 , REAL_LEVEL );
			Promote( 2 , REAL_LEVEL );
			Real_Divide();
			Pop( 2 );
			Replace( stkpos , Value_Register );
                }

        	case REAL_LEVEL:
                {
	                if (level == REAL_LEVEL) break;

			Make_Complex_Number(Get_Number_Real_Value(Top(stkpos)),
				            0.0 );
			Replace( stkpos , Value_Register );
                }
        	case COMPLEX_LEVEL:
                {
	                if (level == COMPLEX_LEVEL) break;

        		Panic("Unknown level to promote in promoting routine.");
                }

		default: 
			Panic("Unknown tower position in promoting routine");
                
	}
        
}        


/* DEMOTION */

/* Demote a number to the lowest tower position such that its value
does not lose anything, i.e., that if you promoted it again to the same level,
it would be the same value. */

Public void Demote()
{

	switch (Get_Number_Tower_Position( Value_Register ))
        {
		case COMPLEX_LEVEL:         	
                        Demote_Complex_To_Real();
			break;

		case REAL_LEVEL:         	
			break;	/* Retaining inexactness */

		case RATIONAL_LEVEL:
                        Demote_Rational_To_Integer();
			break;

        	case BIGNUM_LEVEL:
                        Demote_Bignum_To_Fixnum();
			break;

                case FIXNUM_LEVEL: 
			break;

                default: Panic("Unknown tower position in demote routine");
        }
}




Private void Demote_Complex_To_Real()
{

	if (Get_Number_Complex_Imaginary_Part(Value_Register) == 0.0)
	{
		Make_Real_Number(Get_Number_Complex_Real_Part(Value_Register));
	}
}        




Private void Demote_Rational_To_Integer()
{
	Object	denominator = Get_Number_Rational_Denominator( Value_Register );
	
	if ( Get_Number_Tower_Position( denominator ) == FIXNUM_LEVEL
		&& Get_Number_Fixnum_Value( denominator ) == 1 )
	{
		Value_Register = Get_Number_Rational_Numerator(Value_Register);
	}
}



Private void Demote_Bignum_To_Fixnum()
{
	if (Get_Number_Length(Value_Register) == 1)
	{
		Make_Fixnum_Number(
			(Short) Get_Number_Digit(Value_Register, 0));
	}
}



/* MISC TRANSFER FUNCTIONS */

Public void Integer_To_Number(n)

	Integer n;
{
	if ( abs( n ) >= RADIX )
		Integer_To_Bignum( n );
	else 
		Make_Fixnum_Number( n );
}



Public Integer Number_To_Integer(num)

   Object num;

{
	if ( Get_Number_Tower_Position( num ) == FIXNUM_LEVEL )
	{
		return( (Integer) Get_Number_Fixnum_Value( num ));
	}
	else if ( Get_Number_Tower_Position( num ) == BIGNUM_LEVEL )
	{
		return Bignum_To_Integer(num);
	}
	else
	{
		Display_Error("Integer wanted where non-integer supplied:",num);
		return( 0 );
	}
}   



Public	String	Integer_To_Cstring( N , S , Radix , Zero_Padding )

	Integer	N;
	String	S;
	Integer	Radix;
	Integer	Zero_Padding;
{
	Boolean	negative = ( N < 0 );
	Integer	integral = (N < 0) ? -N : N;
	Integer	quotient = 0;
	Integer	digit;
	Integer digits = 0;
	Integer	index = MAX_CHARS_PER_INT;

	S[--index] = '\0';

	do
	{
		quotient = integral / Radix ;
		digit = integral - (quotient * Radix);
		integral = quotient;
		S[--index] = (Character) (digit<=9 ? digit+'0' : digit-10+'a');
		digits++;
	} while ( integral );
	if ( negative ) S[--index] = '-';

	if ( Zero_Padding )
	{
		/* We want to pad the number to the left with 0's  */

		while ( digits++ < Zero_Padding )
			S[--index] = '0';
	}

	return( &S[index] );
}

/* STRING-TO-NUMBER CONVERSION */

Private	String	String_To_Complex();
Private	String	String_To_Ureal();
Private	String	String_To_Uinteger();
Private	String	String_To_Ufractional();
Private Boolean	Legal_Digit();
Private	Integer	Digit_Value();

typedef	enum
{
	EXACT, INEXACT, UNDEFINED
} Exacttype;


Private void String_To_Number()
{
	String	S = Get_String_Value( Top(2) );
	Integer	Radix = Number_To_Integer( Top(1) );

	Cstring_To_Number( S , Radix );
}


Public	void Cstring_To_Number( S , Radix )

	String	S;
	Integer Radix;
{
	/* <num R>    ->  <prefix R> <complex R> */

	String	original;
	Integer precision = 0;
	Exacttype exactness = UNDEFINED;
	original = S;

	/* Look for <prefix> */

	while ( *S == '#' )
	{
		S++;
		switch( *S++ )
		{
		case  'b':
			if ( ! Radix ) Radix = 2;
			else Error1( "Inconsistent radix in: %s", original );
			break;

		case  'o':
			if ( ! Radix ) Radix = 8;
			else Error1( "Inconsistent radix in: %s", original );
			break;

		case  'd':
			if ( ! Radix ) Radix = 10;
			else Error1( "Inconsistent radix in: %s", original );
			break;

		case  'x':
			if ( ! Radix ) Radix = 16;
			else Error1( "Inconsistent radix in: %s", original );
			break;

		case  'i':
			if ( exactness  != UNDEFINED ) exactness = INEXACT;
			else Error1( "Inconsistent exactness in: %s", original );
			break;

		case  'e':
			if ( exactness != UNDEFINED ) exactness = EXACT;
			else Error1( "Inconsistent exactness in: %s", original );
			break;

		case  's':
			if ( ! precision ) precision = 1;
			else Error1( "Inconsistent precision in: %s", original );
			break;

		case  'l':
			if ( ! precision ) precision = 2;
			else Error1( "Inconsistent precision in: %s", original );
			break;

		default:
			Error1( "Unknown prefix in: %s", original );
			break;
		}
	}
	if ( ! Radix ) Radix = 10;	/* by default */

	S = String_To_Complex( original , S , Radix );
	Demote();

	if ( *S )
	{
		Error1( "Ill-formed numeric constant: <%s>", original );
	}

	/* Finally, take account of any #e/#i prefix */

	if ( exactness == INEXACT && Is_Exact_Number( Value_Register ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
	else if ( exactness == EXACT && !Is_Exact_Number( Value_Register ) )
	{
		Push( Value_Register );
		Number_Inexact_To_Exact(); Pop(1);
	}
}



Private	String String_To_Complex( Original ,  S , Radix )

	String	Original;
	String	S;
	Integer	Radix;
{
	/*  <complex R>	->  +i  |  -i  |  <real R>  |  <real R> @ <real R>   
			|   <real R> [+|-] <ureal R> i  |  <real R> [+|-] i
			|   <real R> i

	    <real R>	->  {+|-} <ureal R>
	*/

	Boolean	negative1 = FALSE;
	Boolean negative2 = FALSE;

	/* Possible sign */

	if ( *S == '+' ) 
	{
		S++ ;
		/* Simple imaginary? */
		if ( *S == 'i' )
		{
			S++;
			Make_Complex_Number( 0.0 , 1.0 );
			return( S );
		}
	}
	else if ( *S == '-' )
	{
		S++;
		negative1 = TRUE;
		/* Simple imaginary? */
		if ( *S == 'i' )
		{
			S++;
			Make_Complex_Number( 0.0 , -1.0 );
			return( S );
		}
	}

	S = String_To_Ureal( Original , S , Radix );

	if ( negative1 )
	{
		Push( Value_Register );
		Number_Negate(); Pop(1);
	}

	if ( *S )
	{
		Push( Value_Register );
		switch( *S++ )
		{
		case  '@':
			/* Possible sign */

			if ( *S == '+' ) 
				S++ ;
			else if ( *S == '-' )
			{
				S++;
				negative2 = TRUE;
			}

			S = String_To_Ureal( Original , S , Radix );
			if ( negative2 )
			{
				Push( Value_Register );
				Number_Negate(); Pop(1);
			}
			Push( Value_Register );
			Promote( 1 , REAL_LEVEL );
			Promote( 2 , REAL_LEVEL );
			Make_Complex_Number(
				cos( Get_Number_Real_Value( Top(1) ) )
				* Get_Number_Real_Value( Top(2) ),
				sin( Get_Number_Real_Value( Top(1) ) )
				* Get_Number_Real_Value( Top(2) ) );
			Pop(2) ;
			return( S );
		case  '-':
			negative2 = TRUE;
		case  '+':
			if ( *S == 'i' )
			{
				Make_Real_Number( negative2 ? -1.0 : 1.0 );
				Push( Value_Register );
			}
			else
			{
				S = String_To_Ureal( Original , S , Radix );
				if ( negative2 )
				{
					Push( Value_Register );
					Number_Negate(); Pop(1);
				}
				Push( Value_Register );
			}
			Promote( 1 , REAL_LEVEL );
			Promote( 2 , REAL_LEVEL );
			Make_Complex_Number( Get_Number_Real_Value( Top(2) ),
					     Get_Number_Real_Value( Top(1) ) );
			Pop(2);
			if ( *S++ != 'i') 
			{
				Error1( "Ill-formed numeric constant: %s" , 
					Original );
			}
			return( S );

		case 'i':
			Promote( 1 , REAL_LEVEL );
			Make_Complex_Number(0.0, Get_Number_Real_Value(Top(1)));
			Pop(1);
			return( S );

		default:
			Error1( "Ill-formed constant: <%s>", Original );
		}
	}
	return( S );
}


Private	String	String_To_Ureal( Original , S , Radix )

	String	Original;
	String	S;
	Integer	Radix;
{
	/*  <ureal R>	->  <uinteger R>  |  <uinteger R> / <uinteger R>  
			|   <float R>

	    <float R>	->  <uinteger R> <suffix>  |  . <ufractional R> <suffix>
			|   <uinteger R> . <ufractional R> <suffix>
			|   . #* <suffix>

	    <suffix>	->  <empty>  |  <exp mark> {+|-} <digit R>+

	    <exp mark>	->  e  |  s  |  f  |  d  |  l
	*/

	/* Scan first part of number */

	if ( *S  == '.' )
	{
		Make_Real_Number( 0.0 );
	}
	else
	{
		S = String_To_Uinteger( Original , S , Radix );
	}

	/* Is there more ? */

	if ( *S == '.' )
	{
		Push( Value_Register );
		if ( Radix != 10 )
		{
			Error1( "Non-decimal real: %s" , Original );
		}
		S++;
		S = String_To_Ufractional( Original , S , Radix );
		Push( Value_Register );
		Number_Add(); Pop(2);
	}
	else if ( *S == '/' )
	{
		Push( Value_Register );
		S++;
		S = String_To_Uinteger( Original , S , Radix );
		Push( Value_Register );
		Make_Rational_Number(); Pop(2);
		return( S );
	}

	if ( *S=='e' || *S=='s' || *S=='f' || *S=='d' || *S=='l' )
	{
		/* all exponent marks map to e in UMB Scheme */

		Boolean negative = FALSE;

		Push( Value_Register );
		Promote( 1 , REAL_LEVEL );
		if ( Radix != 10 ) 
		{
			Error1( "Non-decimal real: %s" , Original );
		}

		S++;
		Integer_To_Number( Radix );
		Push( Value_Register );			/* Radix */

		/* Possible sign */

		if ( *S == '+' ) 
			S++ ;
		else if ( *S == '-' )
		{
			S++;
			negative = TRUE;
		}
		S = String_To_Uinteger( Original , S , Radix );
		Push( Value_Register );
		if ( negative )
		{
			Number_Negate(); Pop(1);
			Push( Value_Register );
		}					/* Exponent */
		
		Number_Expt(); Pop(2);
		Push( Value_Register );
		Number_Multiply(); Pop(2);
	}
	return( S );
}




Private	String	String_To_Uinteger( Original , S , Radix )

	String	Original;
	String	S;
	Integer	Radix;
{
	/*   <uinteger R>  ->  <digit R>+ #*   */

	if ( ! Legal_Digit( *S , Radix ) )
	{
		Error1( "Ill-formed numeric constant: <%s>" , Original );
	}

	Integer_To_Number( Digit_Value( *S++ ) );
	while ( Legal_Digit( *S , Radix ) )
	{
		Push( Value_Register );
		Integer_To_Number( Radix );
		Push( Value_Register );
		Number_Multiply(); Pop(2);
		Push( Value_Register );
		Integer_To_Number( Digit_Value( *S ) );
		Push( Value_Register );
		Number_Add(); Pop(2);
		S++;
	}
	while ( *S == '#' )
	{
		Push( Value_Register );
		Make_Real_Number( (Double) Radix );
		Push( Value_Register );
		Number_Multiply(); Pop(2);
		S++;
	}
	return( S );
}



Private	String	String_To_Ufractional( Original , S , Radix )

	String	Original;
	String	S;
	Integer	Radix;
{
	Double	fraction = 0.0;
	Double	factor = 1.0;

	while ( Legal_Digit( *S , Radix ) )
	{
		factor = factor / Radix;
		fraction += factor * Digit_Value( *S );
		S++;
	}
	while ( *S == '#' ) S++;
	Make_Real_Number( fraction );
	return( S );
}



Private	Boolean	Legal_Digit( Char , Radix )

	Character Char;
	Integer	  Radix;
{
	Integer	val = Digit_Value( Char );
	return( val >= 0 && val < Radix );
}


Private	Boolean	Digit_Value( Char )

	Character Char;
{
	return(	isdigit( Char ) ? Char - '0' : Char - 'a' + 10 );
}

Public void Initialize_Number()
{

   Num_Ops.Is_Number_Zero[FIXNUM_LEVEL] = Is_Fixnum_Zero;
   Num_Ops.Is_Number_Zero[BIGNUM_LEVEL] = Is_Bignum_Zero;
   Num_Ops.Is_Number_Zero[RATIONAL_LEVEL] = Is_Rational_Zero;
   Num_Ops.Is_Number_Zero[REAL_LEVEL] = Is_Real_Zero;
   Num_Ops.Is_Number_Zero[COMPLEX_LEVEL] = Is_Complex_Zero;
   
   Num_Ops.Is_Number_Positive[FIXNUM_LEVEL] = Is_Fixnum_Positive;
   Num_Ops.Is_Number_Positive[BIGNUM_LEVEL] = Is_Bignum_Positive;
   Num_Ops.Is_Number_Positive[RATIONAL_LEVEL] = Is_Rational_Positive;
   Num_Ops.Is_Number_Positive[REAL_LEVEL] = Is_Real_Positive;
   Num_Ops.Is_Number_Positive[COMPLEX_LEVEL] = Is_Complex_Positive;
   
   Num_Ops.Is_Number_Negative[FIXNUM_LEVEL] = Is_Fixnum_Negative;
   Num_Ops.Is_Number_Negative[BIGNUM_LEVEL] = Is_Bignum_Negative;
   Num_Ops.Is_Number_Negative[RATIONAL_LEVEL] = Is_Rational_Negative;
   Num_Ops.Is_Number_Negative[REAL_LEVEL] = Is_Real_Negative;
   Num_Ops.Is_Number_Negative[COMPLEX_LEVEL] = Is_Complex_Negative;
   
   Num_Ops.Is_Number_Odd[FIXNUM_LEVEL] = Is_Fixnum_Odd;
   Num_Ops.Is_Number_Odd[BIGNUM_LEVEL] = Is_Bignum_Odd;
   Num_Ops.Is_Number_Odd[RATIONAL_LEVEL] = Is_Rational_Odd;
   Num_Ops.Is_Number_Odd[REAL_LEVEL] = Is_Real_Odd;
   Num_Ops.Is_Number_Odd[COMPLEX_LEVEL] = Is_Complex_Odd;
   
   Num_Ops.Is_Number_Even[FIXNUM_LEVEL] = Is_Fixnum_Even;
   Num_Ops.Is_Number_Even[BIGNUM_LEVEL] = Is_Bignum_Even;
   Num_Ops.Is_Number_Even[RATIONAL_LEVEL] = Is_Rational_Even;
   Num_Ops.Is_Number_Even[REAL_LEVEL] = Is_Real_Even;
   Num_Ops.Is_Number_Even[COMPLEX_LEVEL] = Is_Complex_Even;
   
   Num_Ops.Is_Number_Exact[FIXNUM_LEVEL] = Is_Fixnum_Exact;
   Num_Ops.Is_Number_Exact[BIGNUM_LEVEL] = Is_Bignum_Exact;
   Num_Ops.Is_Number_Exact[RATIONAL_LEVEL] = Is_Rational_Exact;
   Num_Ops.Is_Number_Exact[REAL_LEVEL] = Is_Real_Exact;
   Num_Ops.Is_Number_Exact[COMPLEX_LEVEL] = Is_Complex_Exact;
   
   Num_Ops.Is_Number_Inexact[FIXNUM_LEVEL] = Is_Fixnum_Inexact;
   Num_Ops.Is_Number_Inexact[BIGNUM_LEVEL] = Is_Bignum_Inexact;
   Num_Ops.Is_Number_Inexact[RATIONAL_LEVEL] = Is_Rational_Inexact;
   Num_Ops.Is_Number_Inexact[REAL_LEVEL] = Is_Real_Inexact;
   Num_Ops.Is_Number_Inexact[COMPLEX_LEVEL] = Is_Complex_Inexact;
   
   Num_Ops.Number_Equal[FIXNUM_LEVEL] = Fixnum_Equal;
   Num_Ops.Number_Equal[BIGNUM_LEVEL] = Bignum_Equal;
   Num_Ops.Number_Equal[RATIONAL_LEVEL] = Rational_Equal;
   Num_Ops.Number_Equal[REAL_LEVEL] = Real_Equal;
   Num_Ops.Number_Equal[COMPLEX_LEVEL] = Complex_Equal;
   
   Num_Ops.Number_Less_Than[FIXNUM_LEVEL] = Fixnum_Less_Than;
   Num_Ops.Number_Less_Than[BIGNUM_LEVEL] = Bignum_Less_Than;
   Num_Ops.Number_Less_Than[RATIONAL_LEVEL] = Rational_Less_Than;
   Num_Ops.Number_Less_Than[REAL_LEVEL] = Real_Less_Than;
   Num_Ops.Number_Less_Than[COMPLEX_LEVEL] = Complex_Less_Than;
   
   Num_Ops.Number_Greater_Than[FIXNUM_LEVEL] = Fixnum_Greater_Than;
   Num_Ops.Number_Greater_Than[BIGNUM_LEVEL] = Bignum_Greater_Than;
   Num_Ops.Number_Greater_Than[RATIONAL_LEVEL] = Rational_Greater_Than;
   Num_Ops.Number_Greater_Than[REAL_LEVEL] = Real_Greater_Than;
   Num_Ops.Number_Greater_Than[COMPLEX_LEVEL] = Complex_Greater_Than;
   
   Num_Ops.Number_Less_Than_Or_Equal[FIXNUM_LEVEL] = Fixnum_Less_Than_Or_Equal;
   Num_Ops.Number_Less_Than_Or_Equal[BIGNUM_LEVEL] = Bignum_Less_Than_Or_Equal;
   Num_Ops.Number_Less_Than_Or_Equal[RATIONAL_LEVEL] = Rational_Less_Than_Or_Equal;
   Num_Ops.Number_Less_Than_Or_Equal[REAL_LEVEL] = Real_Less_Than_Or_Equal;
   Num_Ops.Number_Less_Than_Or_Equal[COMPLEX_LEVEL] = Complex_Less_Than_Or_Equal;
   
   Num_Ops.Number_Greater_Than_Or_Equal[FIXNUM_LEVEL] = Fixnum_Greater_Than_Or_Equal;
   Num_Ops.Number_Greater_Than_Or_Equal[BIGNUM_LEVEL] = Bignum_Greater_Than_Or_Equal;
   Num_Ops.Number_Greater_Than_Or_Equal[RATIONAL_LEVEL] = Rational_Greater_Than_Or_Equal;
   Num_Ops.Number_Greater_Than_Or_Equal[REAL_LEVEL] = Real_Greater_Than_Or_Equal;
   Num_Ops.Number_Greater_Than_Or_Equal[COMPLEX_LEVEL] = Complex_Greater_Than_Or_Equal;
   
   Num_Ops.Number_Max[FIXNUM_LEVEL] = Fixnum_Max;
   Num_Ops.Number_Max[BIGNUM_LEVEL] = Bignum_Max;
   Num_Ops.Number_Max[RATIONAL_LEVEL] = Rational_Max;
   Num_Ops.Number_Max[REAL_LEVEL] = Real_Max;
   Num_Ops.Number_Max[COMPLEX_LEVEL] = Complex_Max;
   
   Num_Ops.Number_Min[FIXNUM_LEVEL] = Fixnum_Min;
   Num_Ops.Number_Min[BIGNUM_LEVEL] = Bignum_Min;
   Num_Ops.Number_Min[RATIONAL_LEVEL] = Rational_Min;
   Num_Ops.Number_Min[REAL_LEVEL] = Real_Min;
   Num_Ops.Number_Min[COMPLEX_LEVEL] = Complex_Min;
   
   Num_Ops.Number_Add[FIXNUM_LEVEL] = Fixnum_Add;
   Num_Ops.Number_Add[BIGNUM_LEVEL] = Bignum_Add;
   Num_Ops.Number_Add[RATIONAL_LEVEL] = Rational_Add;
   Num_Ops.Number_Add[REAL_LEVEL] = Real_Add;
   Num_Ops.Number_Add[COMPLEX_LEVEL] = Complex_Add;
   
   Num_Ops.Number_Subtract[FIXNUM_LEVEL] = Fixnum_Subtract;
   Num_Ops.Number_Subtract[BIGNUM_LEVEL] = Bignum_Subtract;
   Num_Ops.Number_Subtract[RATIONAL_LEVEL] = Rational_Subtract;
   Num_Ops.Number_Subtract[REAL_LEVEL] = Real_Subtract;
   Num_Ops.Number_Subtract[COMPLEX_LEVEL] = Complex_Subtract;
   
   Num_Ops.Number_Multiply[FIXNUM_LEVEL] = Fixnum_Multiply;
   Num_Ops.Number_Multiply[BIGNUM_LEVEL] = Bignum_Multiply;
   Num_Ops.Number_Multiply[RATIONAL_LEVEL] = Rational_Multiply;
   Num_Ops.Number_Multiply[REAL_LEVEL] = Real_Multiply;
   Num_Ops.Number_Multiply[COMPLEX_LEVEL] = Complex_Multiply;
   
   Num_Ops.Number_Divide[FIXNUM_LEVEL] = Fixnum_Divide;
   Num_Ops.Number_Divide[BIGNUM_LEVEL] = Bignum_Divide;
   Num_Ops.Number_Divide[RATIONAL_LEVEL] = Rational_Divide;
   Num_Ops.Number_Divide[REAL_LEVEL] = Real_Divide;
   Num_Ops.Number_Divide[COMPLEX_LEVEL] = Complex_Divide;
   
   Num_Ops.Number_Quotient[FIXNUM_LEVEL] = Fixnum_Quotient;
   Num_Ops.Number_Quotient[BIGNUM_LEVEL] = Bignum_Quotient;
   Num_Ops.Number_Quotient[RATIONAL_LEVEL] = Rational_Quotient;
   Num_Ops.Number_Quotient[REAL_LEVEL] = Real_Quotient;
   Num_Ops.Number_Quotient[COMPLEX_LEVEL] = Complex_Quotient;
   
   Num_Ops.Number_Remainder[FIXNUM_LEVEL] = Fixnum_Remainder;
   Num_Ops.Number_Remainder[BIGNUM_LEVEL] = Bignum_Remainder;
   Num_Ops.Number_Remainder[RATIONAL_LEVEL] = Rational_Remainder;
   Num_Ops.Number_Remainder[REAL_LEVEL] = Real_Remainder;
   Num_Ops.Number_Remainder[COMPLEX_LEVEL] = Complex_Remainder;
   
   Num_Ops.Number_Modulo[FIXNUM_LEVEL] = Fixnum_Modulo;
   Num_Ops.Number_Modulo[BIGNUM_LEVEL] = Bignum_Modulo;
   Num_Ops.Number_Modulo[RATIONAL_LEVEL] = Rational_Modulo;
   Num_Ops.Number_Modulo[REAL_LEVEL] = Real_Modulo;
   Num_Ops.Number_Modulo[COMPLEX_LEVEL] = Complex_Modulo;

   Num_Ops.Number_Negate[FIXNUM_LEVEL] = Fixnum_Negate;
   Num_Ops.Number_Negate[BIGNUM_LEVEL] = Bignum_Negate;
   Num_Ops.Number_Negate[RATIONAL_LEVEL] = Rational_Negate;  
   Num_Ops.Number_Negate[REAL_LEVEL] = Real_Negate;
   Num_Ops.Number_Negate[COMPLEX_LEVEL] = Complex_Negate; 

   Num_Ops.Number_Abs[FIXNUM_LEVEL] = Fixnum_Abs;
   Num_Ops.Number_Abs[BIGNUM_LEVEL] = Bignum_Abs;
   Num_Ops.Number_Abs[RATIONAL_LEVEL] = Rational_Abs;
   Num_Ops.Number_Abs[REAL_LEVEL] = Real_Abs;
   Num_Ops.Number_Abs[COMPLEX_LEVEL] = Complex_Abs;
   
   Num_Ops.Number_Numerator[FIXNUM_LEVEL] = Fixnum_Numerator;
   Num_Ops.Number_Numerator[BIGNUM_LEVEL] = Bignum_Numerator;
   Num_Ops.Number_Numerator[RATIONAL_LEVEL] = Rational_Numerator;
   Num_Ops.Number_Numerator[REAL_LEVEL] = Real_Numerator;
   Num_Ops.Number_Numerator[COMPLEX_LEVEL] = Complex_Numerator;

   Num_Ops.Number_Denominator[FIXNUM_LEVEL] = Fixnum_Denominator;
   Num_Ops.Number_Denominator[BIGNUM_LEVEL] = Bignum_Denominator;
   Num_Ops.Number_Denominator[RATIONAL_LEVEL] = Rational_Denominator;
   Num_Ops.Number_Denominator[REAL_LEVEL] = Real_Denominator;
   Num_Ops.Number_Denominator[COMPLEX_LEVEL] = Complex_Denominator;

   Num_Ops.Number_Rationalize[FIXNUM_LEVEL] = Fixnum_Rationalize;
   Num_Ops.Number_Rationalize[BIGNUM_LEVEL] = Bignum_Rationalize;
   Num_Ops.Number_Rationalize[RATIONAL_LEVEL] = Rational_Rationalize;
   Num_Ops.Number_Rationalize[REAL_LEVEL] = Real_Rationalize; 
   Num_Ops.Number_Rationalize[COMPLEX_LEVEL] = Complex_Rationalize;

   Num_Ops.Number_GCD[FIXNUM_LEVEL] = Fixnum_GCD;
   Num_Ops.Number_GCD[BIGNUM_LEVEL] = Bignum_GCD;
   Num_Ops.Number_GCD[RATIONAL_LEVEL] = Rational_GCD;
   Num_Ops.Number_GCD[REAL_LEVEL] = Real_GCD;
   Num_Ops.Number_GCD[COMPLEX_LEVEL] = Complex_GCD;

   Num_Ops.Number_LCM[FIXNUM_LEVEL] = Fixnum_LCM;
   Num_Ops.Number_LCM[BIGNUM_LEVEL] = Bignum_LCM;
   Num_Ops.Number_LCM[RATIONAL_LEVEL] = Rational_LCM;
   Num_Ops.Number_LCM[REAL_LEVEL] = Real_LCM;
   Num_Ops.Number_LCM[COMPLEX_LEVEL] = Complex_LCM;

   Num_Ops.Number_Floor[FIXNUM_LEVEL] = Fixnum_Floor;
   Num_Ops.Number_Floor[BIGNUM_LEVEL] = Bignum_Floor;
   Num_Ops.Number_Floor[RATIONAL_LEVEL] = Rational_Floor;
   Num_Ops.Number_Floor[REAL_LEVEL] = Real_Floor;
   Num_Ops.Number_Floor[COMPLEX_LEVEL] = Complex_Floor;

   Num_Ops.Number_Ceiling[FIXNUM_LEVEL] = Fixnum_Ceiling;
   Num_Ops.Number_Ceiling[BIGNUM_LEVEL] = Bignum_Ceiling;
   Num_Ops.Number_Ceiling[RATIONAL_LEVEL] = Rational_Ceiling;
   Num_Ops.Number_Ceiling[REAL_LEVEL] = Real_Ceiling;
   Num_Ops.Number_Ceiling[COMPLEX_LEVEL] = Complex_Ceiling;

   Num_Ops.Number_Truncate[FIXNUM_LEVEL] = Fixnum_Truncate;
   Num_Ops.Number_Truncate[BIGNUM_LEVEL] = Bignum_Truncate;
   Num_Ops.Number_Truncate[RATIONAL_LEVEL] = Rational_Truncate;
   Num_Ops.Number_Truncate[REAL_LEVEL] = Real_Truncate;
   Num_Ops.Number_Truncate[COMPLEX_LEVEL] = Complex_Truncate;

   Num_Ops.Number_Round[FIXNUM_LEVEL] = Fixnum_Round;
   Num_Ops.Number_Round[BIGNUM_LEVEL] = Bignum_Round;
   Num_Ops.Number_Round[RATIONAL_LEVEL] = Rational_Round;
   Num_Ops.Number_Round[REAL_LEVEL] = Real_Round;
   Num_Ops.Number_Round[COMPLEX_LEVEL] = Complex_Round;

   Num_Ops.Number_Sqrt[FIXNUM_LEVEL] = Fixnum_Sqrt;
   Num_Ops.Number_Sqrt[BIGNUM_LEVEL] = Bignum_Sqrt;
   Num_Ops.Number_Sqrt[RATIONAL_LEVEL] = Rational_Sqrt;
   Num_Ops.Number_Sqrt[REAL_LEVEL] = Real_Sqrt;
   Num_Ops.Number_Sqrt[COMPLEX_LEVEL] = Complex_Sqrt;

   Num_Ops.Number_Exp[FIXNUM_LEVEL] = Fixnum_Exp;
   Num_Ops.Number_Exp[BIGNUM_LEVEL] = Bignum_Exp;
   Num_Ops.Number_Exp[RATIONAL_LEVEL] = Rational_Exp;
   Num_Ops.Number_Exp[REAL_LEVEL] = Real_Exp;
   Num_Ops.Number_Exp[COMPLEX_LEVEL] = Complex_Exp;

   Num_Ops.Number_Log[FIXNUM_LEVEL] = Fixnum_Log;
   Num_Ops.Number_Log[BIGNUM_LEVEL] = Bignum_Log;
   Num_Ops.Number_Log[RATIONAL_LEVEL] = Rational_Log;
   Num_Ops.Number_Log[REAL_LEVEL] = Real_Log;
   Num_Ops.Number_Log[COMPLEX_LEVEL] = Complex_Log;

   Num_Ops.Number_Expt[FIXNUM_LEVEL] = Fixnum_Expt;
   Num_Ops.Number_Expt[BIGNUM_LEVEL] = Bignum_Expt;
   Num_Ops.Number_Expt[RATIONAL_LEVEL] = Rational_Expt;
   Num_Ops.Number_Expt[REAL_LEVEL] = Real_Expt;
   Num_Ops.Number_Expt[COMPLEX_LEVEL] = Complex_Expt;

   Num_Ops.Number_Sin[FIXNUM_LEVEL] = Fixnum_Sin;
   Num_Ops.Number_Sin[BIGNUM_LEVEL] = Bignum_Sin;
   Num_Ops.Number_Sin[RATIONAL_LEVEL] = Rational_Sin;
   Num_Ops.Number_Sin[REAL_LEVEL] = Real_Sin;
   Num_Ops.Number_Sin[COMPLEX_LEVEL] = Complex_Sin;

   Num_Ops.Number_Cos[FIXNUM_LEVEL] = Fixnum_Cos;
   Num_Ops.Number_Cos[BIGNUM_LEVEL] = Bignum_Cos;
   Num_Ops.Number_Cos[RATIONAL_LEVEL] = Rational_Cos;
   Num_Ops.Number_Cos[REAL_LEVEL] = Real_Cos;
   Num_Ops.Number_Cos[COMPLEX_LEVEL] = Complex_Cos;

   Num_Ops.Number_Tan[FIXNUM_LEVEL] = Fixnum_Tan;
   Num_Ops.Number_Tan[BIGNUM_LEVEL] = Bignum_Tan;
   Num_Ops.Number_Tan[RATIONAL_LEVEL] = Rational_Tan;
   Num_Ops.Number_Tan[REAL_LEVEL] = Real_Tan;
   Num_Ops.Number_Tan[COMPLEX_LEVEL] = Complex_Tan;

   Num_Ops.Number_Asin[FIXNUM_LEVEL] = Fixnum_Asin;
   Num_Ops.Number_Asin[BIGNUM_LEVEL] = Bignum_Asin;
   Num_Ops.Number_Asin[RATIONAL_LEVEL] = Rational_Asin;
   Num_Ops.Number_Asin[REAL_LEVEL] = Real_Asin;
   Num_Ops.Number_Asin[COMPLEX_LEVEL] = Complex_Asin;

   Num_Ops.Number_Acos[FIXNUM_LEVEL] = Fixnum_Acos;
   Num_Ops.Number_Acos[BIGNUM_LEVEL] = Bignum_Acos;
   Num_Ops.Number_Acos[RATIONAL_LEVEL] = Rational_Acos;
   Num_Ops.Number_Acos[REAL_LEVEL] = Real_Acos;
   Num_Ops.Number_Acos[COMPLEX_LEVEL] = Complex_Acos;

   Num_Ops.Number_Atan[FIXNUM_LEVEL] = Fixnum_Atan;
   Num_Ops.Number_Atan[BIGNUM_LEVEL] = Bignum_Atan;
   Num_Ops.Number_Atan[RATIONAL_LEVEL] = Rational_Atan;
   Num_Ops.Number_Atan[REAL_LEVEL] = Real_Atan;
   Num_Ops.Number_Atan[COMPLEX_LEVEL] = Complex_Atan;

   Num_Ops.Number_Atan2[FIXNUM_LEVEL] = Fixnum_Atan2;
   Num_Ops.Number_Atan2[BIGNUM_LEVEL] = Bignum_Atan2;
   Num_Ops.Number_Atan2[RATIONAL_LEVEL] = Rational_Atan2;
   Num_Ops.Number_Atan2[REAL_LEVEL] = Real_Atan2;
   Num_Ops.Number_Atan2[COMPLEX_LEVEL] = Complex_Atan2;

   Num_Ops.Number_Exact_To_Inexact[FIXNUM_LEVEL] = Fixnum_Exact_To_Inexact;
   Num_Ops.Number_Exact_To_Inexact[BIGNUM_LEVEL] = Bignum_Exact_To_Inexact;
   Num_Ops.Number_Exact_To_Inexact[RATIONAL_LEVEL] = Rational_Exact_To_Inexact;
   Num_Ops.Number_Exact_To_Inexact[REAL_LEVEL] = Real_Exact_To_Inexact;
   Num_Ops.Number_Exact_To_Inexact[COMPLEX_LEVEL] = Complex_Exact_To_Inexact;

   Num_Ops.Number_Inexact_To_Exact[FIXNUM_LEVEL] = Fixnum_Inexact_To_Exact;
   Num_Ops.Number_Inexact_To_Exact[BIGNUM_LEVEL] = Bignum_Inexact_To_Exact;
   Num_Ops.Number_Inexact_To_Exact[RATIONAL_LEVEL] = Rational_Inexact_To_Exact;
   Num_Ops.Number_Inexact_To_Exact[REAL_LEVEL] = Real_Inexact_To_Exact;
   Num_Ops.Number_Inexact_To_Exact[COMPLEX_LEVEL] = Complex_Inexact_To_Exact;

   Num_Ops.Number_To_String[FIXNUM_LEVEL] = Fixnum_To_String;
   Num_Ops.Number_To_String[BIGNUM_LEVEL] = Bignum_To_String;
   Num_Ops.Number_To_String[RATIONAL_LEVEL] = Rational_To_String;
   Num_Ops.Number_To_String[REAL_LEVEL] = Real_To_String;
   Num_Ops.Number_To_String[COMPLEX_LEVEL] = Complex_To_String;
   
   Num_Ops.Number_Make_Rectangular[FIXNUM_LEVEL] = Fixnum_Make_Rectangular;
   Num_Ops.Number_Make_Rectangular[BIGNUM_LEVEL] = Bignum_Make_Rectangular;
   Num_Ops.Number_Make_Rectangular[RATIONAL_LEVEL] = Rational_Make_Rectangular;
   Num_Ops.Number_Make_Rectangular[REAL_LEVEL] = Real_Make_Rectangular;
   Num_Ops.Number_Make_Rectangular[COMPLEX_LEVEL] = Complex_Make_Rectangular;

   Num_Ops.Number_Make_Polar[FIXNUM_LEVEL] = Fixnum_Make_Polar;
   Num_Ops.Number_Make_Polar[BIGNUM_LEVEL] = Bignum_Make_Polar;
   Num_Ops.Number_Make_Polar[RATIONAL_LEVEL] = Rational_Make_Polar;
   Num_Ops.Number_Make_Polar[REAL_LEVEL] = Real_Make_Polar;
   Num_Ops.Number_Make_Polar[COMPLEX_LEVEL] = Complex_Make_Polar;

   Num_Ops.Number_Real_Part[FIXNUM_LEVEL] = Fixnum_Real_Part;
   Num_Ops.Number_Real_Part[BIGNUM_LEVEL] = Bignum_Real_Part;
   Num_Ops.Number_Real_Part[RATIONAL_LEVEL] = Rational_Real_Part;
   Num_Ops.Number_Real_Part[REAL_LEVEL] = Real_Real_Part;
   Num_Ops.Number_Real_Part[COMPLEX_LEVEL] = Complex_Real_Part;

   Num_Ops.Number_Imaginary_Part[FIXNUM_LEVEL] = Fixnum_Imaginary_Part;
   Num_Ops.Number_Imaginary_Part[BIGNUM_LEVEL] = Bignum_Imaginary_Part;
   Num_Ops.Number_Imaginary_Part[RATIONAL_LEVEL] = Rational_Imaginary_Part;
   Num_Ops.Number_Imaginary_Part[REAL_LEVEL] = Real_Imaginary_Part;
   Num_Ops.Number_Imaginary_Part[COMPLEX_LEVEL] = Complex_Imaginary_Part;

   Num_Ops.Number_Magnitude[FIXNUM_LEVEL] = Fixnum_Magnitude;
   Num_Ops.Number_Magnitude[BIGNUM_LEVEL] = Bignum_Magnitude;
   Num_Ops.Number_Magnitude[RATIONAL_LEVEL] = Rational_Magnitude;
   Num_Ops.Number_Magnitude[REAL_LEVEL] = Real_Magnitude;
   Num_Ops.Number_Magnitude[COMPLEX_LEVEL] = Complex_Magnitude;

   Num_Ops.Number_Angle[FIXNUM_LEVEL] = Fixnum_Angle;
   Num_Ops.Number_Angle[BIGNUM_LEVEL] = Bignum_Angle;
   Num_Ops.Number_Angle[RATIONAL_LEVEL] = Rational_Angle;
   Num_Ops.Number_Angle[REAL_LEVEL] = Real_Angle;
   Num_Ops.Number_Angle[COMPLEX_LEVEL] = Complex_Angle;

   Make_Primitive("number?", Number_Predicate, 1, Any_Type, The_Undefined_Type,
                                                        The_Undefined_Type);

   Make_Primitive("integer?", Integer_Predicate, 1, Any_Type, The_Undefined_Type,
                                                        The_Undefined_Type);

   Make_Primitive("rational?", Rational_Predicate, 1, Any_Type, The_Undefined_Type,
                                                        The_Undefined_Type);

   Make_Primitive("real?", Real_Predicate, 1, Any_Type, The_Undefined_Type,
                                                        The_Undefined_Type);

   Make_Primitive("complex?", Complex_Predicate, 1, Any_Type, The_Undefined_Type,
                                                        The_Undefined_Type);

   Make_Primitive("zero?", Is_Number_Zero, 1, Number_Type, The_Undefined_Type,
						   The_Undefined_Type);
   Make_Primitive("positive?", Is_Number_Positive, 1, Number_Type,
				       The_Undefined_Type, The_Undefined_Type);
   Make_Primitive("negative?", Is_Number_Negative, 1, Number_Type,
				       The_Undefined_Type, The_Undefined_Type);
   Make_Primitive("odd?", Is_Number_Odd, 1, Number_Type, The_Undefined_Type, 
						   The_Undefined_Type);
   Make_Primitive("even?", Is_Number_Even, 1, Number_Type, The_Undefined_Type, 
						   The_Undefined_Type);
   Make_Primitive("exact?", Is_Number_Exact, 1, Number_Type, The_Undefined_Type,
						   The_Undefined_Type);
   Make_Primitive("inexact?", Is_Number_Inexact, 1, Number_Type,
	                               The_Undefined_Type, The_Undefined_Type);
 
   Make_Primitive("=", Varying_Number_Equal, VARYING, Number_Type, 
		  The_Undefined_Type , The_Undefined_Type );

   Make_Primitive(">", Varying_Number_Greater_Than, VARYING, Number_Type, 
		  The_Undefined_Type , The_Undefined_Type );

   Make_Primitive("<", Varying_Number_Less_Than, VARYING, Number_Type, 
		  The_Undefined_Type , The_Undefined_Type );

   Make_Primitive(">=", Varying_Number_Greater_Than_Or_Equal, VARYING, 
		  Number_Type, The_Undefined_Type , The_Undefined_Type );

   Make_Primitive("<=", Varying_Number_Less_Than_Or_Equal, VARYING,
		  Number_Type, The_Undefined_Type , The_Undefined_Type );

   Make_Primitive("+", Varying_Number_Add, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("-", Varying_Number_Subtract, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("*", Varying_Number_Multiply, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("/", Varying_Number_Divide, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive( "min", Varying_Number_Min, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive( "max", Varying_Number_Max, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("gcd", Varying_Number_GCD, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("lcm", Varying_Number_LCM, VARYING, Number_Type, 
		  The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("quotient", Number_Quotient, 2, Number_Type, Number_Type,
   					   The_Undefined_Type);
   Make_Primitive("remainder", Number_Remainder, 2, Number_Type, Number_Type, 
 					   The_Undefined_Type);  

   Make_Primitive("modulo", Number_Modulo, 2, Number_Type, Number_Type, 
 					   The_Undefined_Type);  

   Make_Primitive("negate", Number_Negate, 1, Number_Type, The_Undefined_Type,
   					   The_Undefined_Type);

   Make_Primitive("abs", Number_Abs, 1, Number_Type, The_Undefined_Type,
   					   The_Undefined_Type); 

   Make_Primitive("numerator", Number_Numerator, 1, Number_Type, 
   			  		The_Undefined_Type,The_Undefined_Type); 

   Make_Primitive("denominator", Number_Denominator,1, Number_Type,
					The_Undefined_Type, The_Undefined_Type);

   Make_Primitive("rationalize", Number_Rationalize, 2, Number_Type, 
					Number_Type, The_Undefined_Type);

   Make_Primitive("floor", Number_Floor, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );

   Make_Primitive("ceiling", Number_Ceiling, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );

   Make_Primitive("truncate", Number_Truncate, 1, Number_Type,
					The_Undefined_Type,The_Undefined_Type );

   Make_Primitive("round", Number_Round, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );

   Make_Primitive("sqrt", Number_Sqrt, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   
   Make_Primitive("exp", Number_Exp, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );

   Make_Primitive("log", Number_Log, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );

   Make_Primitive("expt", Number_Expt, 2, Number_Type, Number_Type,
						The_Undefined_Type );

   Make_Primitive("sin", Number_Sin, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   Make_Primitive("cos", Number_Cos, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   Make_Primitive("tan", Number_Tan, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );

   Make_Primitive("acos", Number_Acos, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   Make_Primitive("asin", Number_Asin, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   Make_Primitive("#_atan1", Number_Atan, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   Make_Primitive("#_atan2", Number_Atan2, 2, Number_Type, Number_Type,
						The_Undefined_Type );
   Make_Primitive("atan", Number_Atan, 1, Number_Type, The_Undefined_Type,
						The_Undefined_Type );
   Make_Primitive("atan2", Number_Atan2, 2, Number_Type, Number_Type,
						The_Undefined_Type );
   
   Make_Primitive("exact->inexact", Number_Exact_To_Inexact, 1, Number_Type,
		       			The_Undefined_Type, The_Undefined_Type);
   Make_Primitive("inexact->exact", Number_Inexact_To_Exact, 1, Number_Type,
		       			The_Undefined_Type, The_Undefined_Type);

   Make_Primitive("#_number->string", Number_To_String, 2, Number_Type,
					Number_Type , The_Undefined_Type); 

   Make_Primitive("#_string->number", String_To_Number, 2, String_Type,
	    Number_Type, The_Undefined_Type);

   Make_Primitive("make-rectangular", Number_Make_Rectangular, 2, Number_Type,
					Number_Type, The_Undefined_Type );
   
   Make_Primitive("make-polar", Number_Make_Polar, 2, Number_Type,
					Number_Type, The_Undefined_Type );
   
   Make_Primitive("real-part", Number_Real_Part, 1, Number_Type,
				The_Undefined_Type, The_Undefined_Type );
   
   Make_Primitive("imag-part", Number_Imaginary_Part, 1, Number_Type,
				The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("magnitude", Number_Magnitude, 1, Number_Type,
				The_Undefined_Type, The_Undefined_Type );

   Make_Primitive("angle", Number_Angle, 1, Number_Type,
				The_Undefined_Type, The_Undefined_Type );


}
