/* bignum.c -- UMB Scheme, implementation of bignum numbers. 

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
#include <errno.h>

#define Most_Significant_Digit(num) \
			(Get_Number_Digit( (num) , Get_Number_Length(num) - 1 ))


Private Short	Bignum_Compare();
Private	void	Delete_Leading_Zeros();
Public	void	Long_Division();


/* REPRESENTATION */

#define MAX_DIGIT		(RADIX-1)

#define Low_16(long)		(((long) >= 0) ? (Short) ((long) % RADIX) \
					      : (-(Short) (-(long) % RADIX)))

#define High_16(long)	(((long) >= 0) ? (Short) ((long) / RADIX) \
					      : (-(Short) (-(long) / RADIX)))

#define Integer_Digit(n,i)	((Integer) Get_Number_Digit(n,i))


#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))


/* Predicates. */

Public Boolean Is_Bignum_Zero()
{
	return (Number_Sign(Top(1)) == 0);
}


Public Boolean Is_Bignum_Positive()
{
	return (Number_Sign(Top(1)) > 0);
}



Public Boolean Is_Bignum_Negative()
{
	return (Number_Sign(Top(1)) < 0);
}



Public Boolean Is_Bignum_Odd()
{
	return (! Is_Bignum_Even());
}



Public Boolean Is_Bignum_Even()
{
	return ( (Get_Number_Digits(Top(1))[0] % 2) == 0);
}



Public Boolean Is_Bignum_Exact()
{
	return( TRUE ); 
}



Public Boolean Is_Bignum_Inexact()
{
	return( FALSE );
}




/* General comparison. */

Public Boolean Bignum_Equal()
{
	return Bignum_Compare() == 0;
}



Public Boolean Bignum_Less_Than()
{
	return Bignum_Compare() < 0;
}



Public Boolean Bignum_Greater_Than()
{
	return Bignum_Compare() > 0;
}



Public Boolean Bignum_Less_Than_Or_Equal()
{
	return Bignum_Compare() <= 0;
}



Public Boolean Bignum_Greater_Than_Or_Equal()
{
	return Bignum_Compare() >= 0;
}



Private Short Bignum_Compare()
{
	Bignum_Subtract();
	return Number_Sign( Value_Register );
}




/* BIGNUM ADDITION */

Public void Bignum_Add()
{
	Integer	size1 = Get_Number_Length( Top(1) );
	Integer	size2 = Get_Number_Length( Top(2) );
	Integer newsize;
	Integer	accumulator;
	Integer index;
	Short	carry;

	Object	top1, top2;

	/* Compute number of digits in result */

	newsize = size1 > size2 ? size1+1 : size2+1;

	Make_Bignum_Number( newsize );
	top1 = Top(1);
	top2 = Top(2);

	carry = 0;
	for ( index = 0; index < newsize; index++ )
	{
		accumulator = (Integer) carry +
		    (index < size1 ? Integer_Digit( top1 , index ) : 0) +
		    (index < size2 ? Integer_Digit( top2 , index ) : 0);
		Get_Number_Digit(Value_Register, index) = Low_16( accumulator );
		carry = High_16( accumulator );
	}
	Delete_Leading_Zeros( Value_Register );
}

/* BIGNUM SUBTRACTION */


Public void Bignum_Subtract()
{
	Integer	size1 = Get_Number_Length( Top(1) );
	Integer	size2 = Get_Number_Length( Top(2) );
	Integer newsize;
	Integer	accumulator;
	Integer index;
	Short	carry;

	Object	top1, top2;

	/* Compute number of digits in result */

	newsize = size1 > size2 ? size1+1 : size2+1;

	Make_Bignum_Number( newsize );
	top1 = Top(1);
	top2 = Top(2);

	carry = 0;
	for ( index = 0; index < newsize; index++ )
	{
		accumulator = (Integer) carry -
		    (index < size1 ? Integer_Digit( top1 , index ) : 0) +
		    (index < size2 ? Integer_Digit( top2 , index ) : 0);
		Get_Number_Digit(Value_Register, index) = Low_16( accumulator );
		carry = High_16( accumulator );
	}
	Delete_Leading_Zeros( Value_Register );
}

/* BIGNUM MULTIPLICATION  */


Public void Bignum_Multiply()
{
	Integer	size1 = Get_Number_Length( Top(1) );
	Integer	size2 = Get_Number_Length( Top(2) );
	Integer newsize;
	Integer	accumulator;
	Integer index1, index2;
	Short	carry;

	Object	top1, top2;

	/* Compute number of digits in result */

	newsize = size1 + size2;

	Make_Bignum_Number( newsize );
	top1 = Top(1);
	top2 = Top(2);

	for ( index1 = 0; index1 < size1; index1 ++ )
	{
		carry = 0;
		for ( index2 = 0; index2 < size2; index2 ++ )
		{
			accumulator = (Integer) carry +
			    Integer_Digit( top1 , index1 ) *
			    Integer_Digit( top2 , index2 ) +
			    Integer_Digit( Value_Register , index1+index2 );
			Get_Number_Digit(Value_Register , index1+index2) =
			    Low_16( accumulator );
			carry = High_16( accumulator );
		}
		if ( index1 + index2 < newsize )
		{
			Get_Number_Digit(Value_Register, index1+index2) = carry;
		}
	}

	Delete_Leading_Zeros( Value_Register );
}


/* BIGNUM DIVISION. */
      
#define QUOTIENT        Top( 1 )
#define DENOMINATOR     Top( 2 )
#define NUMERATOR       Top( 3 )

Private Boolean Numerator_Less_Than_Denominator()

{
	Boolean result;

                                        /* Stack: Q, D, N, d, n...   */
        Push( Top(3) );                 /* Stack: N, Q, D, N, d, n...   */
        Bignum_Abs();
        Pop(1);                         /* Stack: Q, D, N, d, n...   */
        Push(Value_Register);           /* Stack: |N|, Q, D, N, d, n...   */
        Push( Top(3) );                 /* Stack: D, |N|, Q, D, N, d, n...   */
	result = Bignum_Less_Than(); 	/* compare |numerator| < denominator */
        Pop(2);                         /* Stack: Q, D, N, d, n...   */
	return result;
}

Private void Normalize ()
        
	
        /* First justify remainder (numerator) if it's negative. Then
	justify signs for quotient and remainder based on nsign, and
	dsign. Leave quotient in Value_Register and remainder on top 
	of stack.
	*/

{
	Integer	dsign = Number_Sign( Top(4) );
	Integer	nsign = Number_Sign( Top(5) );
        
	/* Justify values for quotient and remainder
           if numerator < 0,  then 1: quotient has to be one unit less,
                                   2: remainder += denominator
        */


        if (Number_Sign( Top(3) ) < 0)    /* Stack: Q, D, N, d, n...   */
	{
		Make_Small_Bignum( -1 );
                Push( Value_Register );   /* Stack: -1, Q, D, N, d, n...   */
		Bignum_Add(); 
                Pop( 2 );                 /* Stack: D, N, d, n...   */
                Push( Value_Register );   /* Stack: Q-1, D, N, d, n...   */


                Push( Top(3) );           /* Stack: N, Q-1, D, N, d, n...   */
                Push( Top(3) );           /* Stack: D, N, Q-1, D, N, d, n... */
		Bignum_Add(); 
                Pop( 2 );                 /* Stack: Q-1, D, N, d, n...   */
                Top( 3 ) = Value_Register; /* adjusted remainder(numerator) */
	}

	/* now justify the signs for quotient and remainder */

	if ( nsign < 0 && dsign < 0) /* must reverse sign of remainder */
        {
                Make_Small_Bignum( 0 );
                Push( Value_Register );   /* Stack: 0, Q, D, N, d, n...   */
                Push( Top(4) );           /* Stack: N, 0, Q, D, N, d, n... */
		Bignum_Subtract();
                Pop( 2 );                 /* Stack: Q, D, N, d, n... */
                Top( 3 ) = Value_Register;
	}
	else if ( nsign > 0 && dsign < 0 ) /* must reverse sign of quotient */
	{
		Make_Small_Bignum( 0 );
                Push( Value_Register );   /* Stack: 0, Q, D, N, d, n... */
                Push( Top(2) );           /* Stack: Q, 0, Q, D, N, d, n... */
		Bignum_Subtract();
                Pop(3);                   /* Stack: D, N, d, n... */
                Push( Value_Register) ;   /* Stack: Q, D, N, d, n... */
	}
	else if ( nsign < 0 && dsign > 0 ) /* reverse signs for both */
	{
		Make_Small_Bignum( 0 );
                Push( Value_Register );   /* Stack: 0, Q, D, N, d, n...   */
                Push( Top(4) );           /* Stack: N, 0, Q, D, N, d, n... */
		Bignum_Subtract();
                Pop( 1 );                 /* Stack: 0, Q, D, N, d, n... */
                Top( 4 ) = Value_Register;

                Push( Top(2) );           /* Stack: Q, 0, Q, D, N, d, n... */
		Bignum_Subtract();
                Pop(3);                   /* Stack: D, N, d, n... */
                Push( Value_Register) ;   /* Stack: Q, D, N, d, n... */
	}
        Value_Register = Top( 1 );
        Pop( 2 );

}



Public void Bignum_Divide()
{
	Make_Rational_Number();
}



Public	void Long_Division()
        /* Value_Register = Top(1) / Top(2)  */
        /* Push( Top(1) % Top(2) )           */

{
        Integer dsize = Get_Number_Length( Top(1) );
	Integer	nsize = Get_Number_Length( Top(2) );
	Integer	dsign = Number_Sign( Top(1) );
	Integer	nsign = Number_Sign( Top(2) );
        Integer accumulator, first_digit;
        Short   index, start_index, first_denominator_digit, carry;


	
	Push( Copy_Object( Top(2), Bignum_Size(nsize))); /* copied numerator */
	if ( nsign < 0 )  /* if numerator < 0, make it positive */    
	{
		Bignum_Abs(); 
		Pop(1);
		Push( Value_Register );
	}

	Push( Copy_Object( Top(2), Bignum_Size(dsize)));/*copied denominator */
	if ( dsign < 0 ) /* if denominator < 0, make it positive */
	{
		Bignum_Abs();
		Pop(1);
		Push( Value_Register );
	}
		
	Make_Bignum_Number( nsize );
	Push( Value_Register );			/* zero-initialized quotient */

	
        first_denominator_digit = Most_Significant_Digit( DENOMINATOR );
	
	/* Just divide their absolute values, then manipulate signs for
		quotient and remainder later */

	while (TRUE)
	{
		accumulator = 0;
                index = Get_Number_Length(NUMERATOR) - 1;
		while (abs(accumulator) < abs(first_denominator_digit) &&
                                                      index >= 0)
		{
			accumulator *= RADIX;
                        accumulator += Integer_Digit(NUMERATOR,index);
			index--;
		}

		first_digit = accumulator / first_denominator_digit;

		start_index = index + 1 - (dsize - 1);

		if (start_index < 0)
		{
                        Delete_Leading_Zeros( QUOTIENT );
                        Normalize();
			return;
		}

                Get_Number_Digit(QUOTIENT,start_index ) += Low_16(first_digit);
		
		carry = 0;

		for ( index = 0; index < dsize; index++ )
		{
			accumulator = - first_digit *
                            Get_Number_Digit(DENOMINATOR,index) +
			    carry + 
                            Get_Number_Digit(NUMERATOR,index + start_index);

                        Get_Number_Digit(NUMERATOR, index + start_index) =
			    Low_16( accumulator );
			carry = High_16( accumulator );

		}

		if (index + start_index < nsize)
		{
                        Get_Number_Digit(NUMERATOR,index+start_index) += carry;
		}

                Delete_Leading_Zeros( NUMERATOR );

                if ((start_index == 0) && Numerator_Less_Than_Denominator())
		{
                        Delete_Leading_Zeros( QUOTIENT );
                        Normalize();
			return;
		}
	}

}



Private void Delete_Leading_Zeros( num )

	Object num;
{
	/*
	   Leading zeros occur in two ways: an explicit leading digit of zero,
	   and a leading one (or minus one) followed by a digit of opposite
	   sign, ie "-1,5" == "-5".
	*/

	Short	index = Get_Number_Length(num) - 1;
	Integer	accumulator;
	Short	first_digit;

	while (index >= 1)
	{
		if (abs( first_digit = Get_Number_Digit(num,index) ) > 1)
			break;
		if (abs ( accumulator = first_digit * RADIX +
		    Get_Number_Digit(num,index-1) ) > MAX_DIGIT)
			break;
		else
		{
			Get_Number_Length(num) = (Integer) index;
			Get_Number_Digit(num,index-1) = Low_16( accumulator );
			index--;
		}
	}
}




Public void Bignum_Quotient()
{
	Long_Division();
	Pop(1); 	/* popping remainder from stack */
}



Public void Bignum_Remainder()
{
	Long_Division();
	Value_Register = Top(1);
	Pop(1);
}



Public void Bignum_Modulo()
{
	Integer	dsign = Number_Sign( Top(1) ); 
	Integer	nsign = Number_Sign( Top(2) );
	
	Long_Division();
	Value_Register = Top(1);

	if ( (nsign * dsign) < 0 )
        /* == "if ((nsign > 0 && dsign < 0) || (nsign < 0 && dsign > 0))"  */
	{
		Push( Top(2) ); /* denominator */
		Bignum_Add(); /* remainder + denominator */
		Pop( 1 );
	}
	
	Pop(1);
}



Public void Bignum_Negate()
{
	Integer len = Get_Number_Length( Top( 1 ) );
	Integer index;

	Value_Register = Copy_Object( Top( 1 ) , Bignum_Size( len ) );
	for ( index = 0; index < len; index++ )
	{
		Get_Number_Digit( Value_Register , index ) =
			- Get_Number_Digit( Value_Register , index );
	}
}



Public void Bignum_Abs()
{
	if (Is_Bignum_Negative())
	{
		Bignum_Negate();
	}
	else
	{
		Value_Register = Top(1);
	}
}



Public void Bignum_Numerator()
{
	Value_Register = Top(1);
}


 
Public void Bignum_Denominator()
{
	Make_Small_Bignum(1);
}


 
Public void Bignum_Rationalize()
{
	Error("Bignum_Rationalize not yet implemented!");
}


 
Public void Bignum_Max()
{
	Value_Register = Bignum_Greater_Than() ? Top(2) : Top(1);
}



Public void Bignum_Min()
{
	Value_Register = Bignum_Less_Than() ? Top(2) : Top(1);
}



Public void Bignum_GCD()
{
	/* make arguments positive since gcd is positive */

	Push( Top(2) );
	Push( Top(2) );

	Bignum_Abs();
	Replace(1, Value_Register);

	Push(Top(2));
	Bignum_Abs();
	Replace(1, Value_Register);

	while (Number_Sign(Top(1)) != 0)
	{
		Bignum_Remainder();
		Top(2) = Top(1);
		Top(1) = Value_Register;
	}

	Value_Register = Top(2);
	
	Pop(3);
}


 
Public void Bignum_LCM()
{
	/* LCM(a,b) = (a*b)/GCD(a,b)  */
	
	Bignum_Multiply();
	Push(Value_Register);

	Push(Top(3));
	Push(Top(3));

	Bignum_GCD();
	Push(Value_Register);

	Top(2) = Top(4);
	Number_Divide();

	Push(Value_Register);
	Number_Abs();

	Pop(5);
}


 

Public void Bignum_Floor()
{
	Value_Register = Top(1);
}


 
Public void Bignum_Ceiling()
{
	Value_Register = Top(1);
}


 
Public void Bignum_Truncate()
{
	Value_Register = Top(1);
}


 
Public void Bignum_Round()
{
	Value_Register = Top(1);
}




Public void Bignum_Sqrt()
{
	Promote(1, REAL_LEVEL);
	
	if( Get_Number_Real_Value(Top(1)) < 0 )
	{
		Make_Complex_Number( (Double) 0.0 ,
				     sqrt( - Get_Number_Real_Value(Top(1))) );
        }
	else
	{
		Make_Real_Number( sqrt (Get_Number_Real_Value(Top(1))) );
	}
}


 
Public void Bignum_Exp()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( exp (Get_Number_Real_Value(Top(1))) );
}


 
Public void Bignum_Log()
{
	if(Number_Sign(Top(1)) <= 0)
	{
		Error("Argument of log must be positive");
	}


	Promote(1, REAL_LEVEL);

	Make_Real_Number( log (Get_Number_Real_Value(Top(1))) );
}


 
Public void Bignum_Expt()
{
	Push( Top(2) );
	Is_Number_Negative();
	Pop(1);

	if( (Number_Sign(Top(1)) == 0) && (Value_Register == The_True_Object) )
	{

		Error("Domain error for expt");
	}

	else if (Number_Sign(Top(1)) >= 0)
	{
		Make_Small_Bignum( 1 ); /* running total (initially = 1) */

		while ( Number_Sign(Top(1)) > 0 )
		{
			Push( Value_Register );		/* running total */

			Push( Top(2) );
			Make_Small_Bignum( 1 );
			Push( Value_Register );
			Number_Subtract();
			Pop(2);
			Replace(2,Value_Register);	/* exponent -= 1 */

			Push( Top(3) );			/* total *= mantissa */
			Bignum_Multiply();
			Pop( 2 );
		}
	}
	else
	{
		/* Negative Exponent */

		Promote(1, REAL_LEVEL);
		Promote(2, REAL_LEVEL);

		Make_Real_Number( pow ( (Get_Number_Real_Value(Top(2))),
				(Get_Number_Real_Value(Top(1))) ) );
	}
}



 
Public void Bignum_Sin()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( sin (Get_Number_Real_Value(Top(1))) );
}


 
Public void Bignum_Cos()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( cos (Get_Number_Real_Value(Top(1))) );
}


 
Public void Bignum_Tan()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( tan (Get_Number_Real_Value(Top(1))) );
}


 
Public void Bignum_Asin()
{
	Error("Argument of asin must lie between -1 and 1, inclusive");
}


 
Public void Bignum_Acos()
{
	Error("Argument of acos must lie between -1 and 1, inclusive");
}


 
Public void Bignum_Atan()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( atan (Get_Number_Real_Value(Top(1))) );
}


 
Public void Bignum_Atan2()
{
	Promote(1, REAL_LEVEL);
	Promote(2, REAL_LEVEL);

	Make_Real_Number( atan2 (Get_Number_Real_Value(Top(2)), 
					Get_Number_Real_Value(Top(1))) );
}


 
 
/* Transfer functions */


Public void Bignum_Exact_To_Inexact()
{
	Promote( 1 , REAL_LEVEL );
	Value_Register = Top(1);
}


 
Public void Bignum_Inexact_To_Exact()
{
	Value_Register = Top(1);
}

/* BIGNUM TO STRING */

Public void Bignum_To_String()

{
	Import void String_Append();

	Integer	high, low;
	Object	remainder;
	Character partial_string[MAX_CHARS_PER_INT];
	Integer	radix = Number_To_Integer( Top(1) );
	Integer print_radix = 0;
	Object	num = Top(2) ;

	switch ( Get_Number_Length( num ) )
	{
	case 0:
		sprintf( partial_string , "%d" , 0 );
		Make_Constant_String( partial_string );
		return;

	case 1:
		if ( radix == 10 )
		{
			sprintf( partial_string, "%d", Get_Number_Digit(num,0));
			Make_Constant_String( partial_string );
		}
		else
		{
			Make_Constant_String(
			   Integer_To_Cstring( Get_Number_Digit(num,0), 
						partial_string, radix, 0) );
		}
		return;
		
	case 2:
		if ( radix == 10 )
		{
			sprintf( partial_string , "%d" ,
		    		Get_Number_Digit(num,1) * RADIX +
		    		Get_Number_Digit(num,0));
			Make_Constant_String( partial_string );
		}
		else
		{
			Make_Constant_String(
			    Integer_To_Cstring((Integer)
					       Get_Number_Digit(num,1) * RADIX +
					       Get_Number_Digit(num,0), 
						partial_string, radix, 0) );
		}
		return;

	default:
		print_radix =	radix == 2  ? 16 :
				radix == 8  ? 4096 :
				radix == 10 ? 10000 :
				radix == 16 ? 65536 : 10000;

		Make_Constant_String( "" );
		Push( Value_Register );
		Push( Top( 3 ) ); 
		do {
			Integer_To_Bignum( print_radix );
			Push( Value_Register );
			Long_Division();
			remainder = Top(1);
			Pop(3); /* remainder, print_radix and dividend */
			high = Get_Number_Length( remainder ) == 2
			    ? Get_Number_Digit( remainder , 1)
			    : 0;
			low = Get_Number_Digit( remainder , 0 );

			Push( Value_Register );	/* new dividend = quotient */
			
			Make_Constant_String( 
				Integer_To_Cstring( abs(high * RADIX + low),
						    partial_string, radix, 4) );
			Push( Value_Register );
			Push( Top( 3 ) );
			String_Append(); Pop( 2 );
			Replace( 2 , Value_Register );	
		} while ( Get_Number_Length( Top( 1 ) ) > 2);
		
		Integer_To_Number( radix );
		Push( Value_Register );
		Number_To_String(); Pop( 2 );
		Push( Value_Register );
		Push( Top( 2 ) );
		String_Append(); Pop( 3 );
	}
}

Public void Bignum_Make_Rectangular()
{
	Promote(2, REAL_LEVEL);
	Promote(1, REAL_LEVEL);

	Make_Complex_Number( Get_Number_Real_Value(Top(2)),
				Get_Number_Real_Value(Top(1)) );

}


 
Public void Bignum_Make_Polar()

#define MAG Get_Number_Real_Value(Top(2))
#define ANG Get_Number_Real_Value(Top(1))

{
	Promote( 2, REAL_LEVEL);
	Promote( 1, REAL_LEVEL);

	Make_Complex_Number( (MAG) * cos(ANG) , (MAG) * sin(ANG) );

}

#undef MAG
#undef ANG


 
Public void Bignum_Real_Part()
{
	Value_Register = Top(1);
}


 
Public void Bignum_Imaginary_Part()
{
	Make_Real_Number(0.0);
}


 
Public void Bignum_Magnitude()
{
	Bignum_Abs();
}

 

Public void Bignum_Angle()
{
	Make_Real_Number(0.0);
}


 
 

/* OTHER CONVERSIONS INVOLVING BIGNUMS */


Public void Make_Small_Bignum(num)

	Integer num;
{
	Integer_To_Bignum( num );
}



Public Integer Bignum_To_Integer(num)

	Object num;
{
	switch (Get_Number_Length(num))
	{
	case 0:
		return( 0 );

	case 1:
		return( Integer_Digit(num,0) );

	case 2:
		return( Integer_Digit(num,1) * RADIX + Integer_Digit(num,0) );

	default:
		Display_Error("Implementation Restriction; integer too large:", 
				num);
		return( 0 );
	}
}



Public void Integer_To_Bignum(num)

	Integer num;
{
	if (num == 0)
	{
		Make_Bignum_Number(1);
	}
	else if (abs(num) < RADIX)
	{
		Make_Bignum_Number(1);
		Get_Number_Digit(Value_Register,0) = num;
	}
	else
	{
		Make_Bignum_Number(2);
		Get_Number_Digit(Value_Register,0) = Low_16(num);
		Get_Number_Digit(Value_Register,1) = High_16(num);
	}
}
 


/* Bignum Diagnostics */

/*

Private void Shownum( title , num )

	String title;
	Object num ;
{
	if ( Is_String(num) )
	{
		printf( "%s: <string \"%s\">" , title , Get_String_Value(num));
		return;
	}

	if ( Is_Number( num ) && Get_Number_Tower_Position(num) == REAL_LEVEL )
	{
		printf( "%s: <real %f>" , title , Get_Number_Real_Value(num));
		return;
	}
	if ( ! Is_Number(num) || Get_Number_Tower_Position(num) != BIGNUM_LEVEL)
	{
		printf( "%s: <NOT A BIGNUM>" , title );
	}
	else
	{
		Integer index;
		Integer len = Get_Number_Length(num);

		printf( "%s: { %d:" , title ,  len );
		for ( index = 0; index < len; index ++ )
		{
			printf( " %d" , Get_Number_Digit(num,index) );
		}

		printf( "}" );
	}
}
*/
