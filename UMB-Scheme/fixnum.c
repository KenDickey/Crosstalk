/* fixnum.c -- UMB Scheme, implementation of specific fixnum procedures. 

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



/* Predicates. */
Boolean Is_Fixnum_Zero()
{
	return ( Get_Number_Fixnum_Value(Top(1)) == 0 );
}



Boolean Is_Fixnum_Positive()
{
	return ( Get_Number_Fixnum_Value(Top(1)) > 0);
}



Boolean Is_Fixnum_Negative()
{
	return ( Get_Number_Fixnum_Value(Top(1)) < 0 );
}



Boolean Is_Fixnum_Odd()
{
	return ( Get_Number_Fixnum_Value(Top(1)) % 2 != 0 );
}



Boolean Is_Fixnum_Even()
{
	return ( Get_Number_Fixnum_Value(Top(1)) % 2 == 0 );
}



Boolean Is_Fixnum_Exact()
{
	return TRUE;
}



Boolean Is_Fixnum_Inexact()
{
	return FALSE;
}




/* Comparisons. */
Boolean Fixnum_Less_Than()
{
	return ( Get_Number_Fixnum_Value(Top(2)) <
		 Get_Number_Fixnum_Value(Top(1)) );
}



Boolean Fixnum_Greater_Than()
{
	return ( Get_Number_Fixnum_Value(Top(2)) > 
		 Get_Number_Fixnum_Value(Top(1)) );
}



Boolean Fixnum_Equal()
{
	return ( Get_Number_Fixnum_Value(Top(2)) == 
		 Get_Number_Fixnum_Value(Top(1)));
}



Boolean Fixnum_Less_Than_Or_Equal()
{
	return ( Get_Number_Fixnum_Value(Top(2)) <= 
		 Get_Number_Fixnum_Value(Top(1)));
}



Boolean Fixnum_Greater_Than_Or_Equal()
{
	return ( Get_Number_Fixnum_Value(Top(2)) >= 
		 Get_Number_Fixnum_Value(Top(1)));
}




/* Arithmetic. */
Public void Fixnum_Add()
{
	Integer result =  ( Get_Number_Fixnum_Value(Top(2)) +
			  Get_Number_Fixnum_Value(Top(1)) );

	if (abs(result) >= RADIX)
	{
		Make_Small_Bignum(result);
	}
	else
	{
		Make_Fixnum_Number( (Short) result);
	}
}



Public void Fixnum_Subtract()
{
	Integer result =  ( Get_Number_Fixnum_Value(Top(2)) -
			  Get_Number_Fixnum_Value(Top(1)) );

	if (abs(result) >= RADIX)
	{
		Make_Small_Bignum(result);
	}
	else
	{
		Make_Fixnum_Number( (Short) result);
	}
}



Public void Fixnum_Multiply()
{
	Integer result =  ( Get_Number_Fixnum_Value(Top(2)) *
			  Get_Number_Fixnum_Value(Top(1)) );

	if (abs(result) >= RADIX)
	{
		Make_Small_Bignum(result);
	}
	else
	{
		Make_Fixnum_Number( (Short) result);
	}
}



Public void Fixnum_Divide()
{
	Integer remainder = Get_Number_Fixnum_Value(Top(2)) % 
			  Get_Number_Fixnum_Value(Top(1));

	if (remainder)
	{
		Make_Rational_Number();
	}
	else
	{
		Make_Fixnum_Number( (Short) (Get_Number_Fixnum_Value(Top(2)) /
		    Get_Number_Fixnum_Value(Top(1))));
	}
}



Public void Fixnum_Quotient()
{
	Make_Fixnum_Number( (Short) ( Get_Number_Fixnum_Value(Top(2)) /
	    Get_Number_Fixnum_Value(Top(1)) ) );
}



Public void Fixnum_Remainder()
{
	Make_Fixnum_Number( (Short) ( Get_Number_Fixnum_Value(Top(2)) %
	    Get_Number_Fixnum_Value(Top(1)) ) );
}



Public void Fixnum_Modulo()
{
	Short dividend = Get_Number_Fixnum_Value(Top(2));
	Short divisor = Get_Number_Fixnum_Value(Top(1));

	Integer remainder = dividend % divisor;

	if( (divisor*dividend) >= 0 )
	{
		Make_Fixnum_Number( (Short) remainder );
	}

	else
	{
		Make_Fixnum_Number( (Short) (remainder + divisor) );	
	}
}


 

Public void Fixnum_Negate()
{
	Value_Register = Copy_Object(Top(1), Fixnum_Size);

	Get_Number_Fixnum_Value(Value_Register) = 
 	    - Get_Number_Fixnum_Value(Value_Register);
}		


	
Public void Fixnum_Abs()
{
	if (Is_Fixnum_Negative())
	{
		Fixnum_Negate();
	}
	else
	{
		Value_Register = Top(1);
	}
}



Public void Fixnum_Numerator()
{
	Value_Register = Top(1);
}


 
Public void Fixnum_Denominator()
{
	Make_Fixnum_Number(1);
}


 
Public void Fixnum_Rationalize()
{
	Error("Rationalize makes no sense on fixnums");
}




Public void Fixnum_Max()
{
	Make_Fixnum_Number( 
	    (Get_Number_Fixnum_Value(Top(2)) > Get_Number_Fixnum_Value(Top(1))
	    ? Get_Number_Fixnum_Value(Top(2))
	    : Get_Number_Fixnum_Value(Top(1))) );
}



Public void Fixnum_Min()
{
	Make_Fixnum_Number( 
	    (Get_Number_Fixnum_Value(Top(2)) < Get_Number_Fixnum_Value(Top(1))
	    ? Get_Number_Fixnum_Value(Top(2))
	    : Get_Number_Fixnum_Value(Top(1))) );
}



Public void Fixnum_GCD()
{
	
	/* make arguments positive since GCD is always positive */

	Push(Top(2));
	Fixnum_Abs();
	Replace( 1 , Value_Register );

	Push(Top(2));
	Fixnum_Abs();
	Replace( 1 , Value_Register );

	while (! Is_Fixnum_Zero() )
	{
		Fixnum_Remainder();
		Top(2) = Top(1);
		Top(1) = Value_Register;
	}

	Value_Register  = Top(2);
	Pop(2);
}


 
Public void Fixnum_LCM()
{
	/*  LCM(a,b) = (a*b)/GCD(a,b)  */
	
	Fixnum_Multiply();
	Push(Value_Register);

	Push(Top(3));
	Push(Top(3));

	Fixnum_GCD();
	Push(Value_Register);

	Top(2) = Top(4);
	Number_Divide();

	Push(Value_Register);
	Number_Abs();

	Pop(5);

}


 

Public void Fixnum_Floor()
{
	Value_Register = Top(1);
}


 
Public void Fixnum_Ceiling()
{
	Value_Register = Top(1);
}


 
Public void Fixnum_Truncate()
{
	Value_Register = Top(1);
}


 
Public void Fixnum_Round()
{
	Value_Register = Top(1);
}




Public void Fixnum_Sqrt()
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


 
Public void Fixnum_Exp()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( exp (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Log()
{
	if(Get_Number_Fixnum_Value(Top(1)) <= 0 )
	{
		Error("Argument of log must be positive");
	}
		
	Promote(1, REAL_LEVEL);

	Make_Real_Number( log (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Expt()
{
	Push( Top(2) );
	Is_Number_Zero();
	Pop(1);

	if((Value_Register == The_True_Object) &&
		(Get_Number_Fixnum_Value(Top(1)) <  0))
	{
		Error("Domain error for expt");
	}
	else if (Get_Number_Fixnum_Value(Top(1)) >= 0 )
	{
		Integer index = Get_Number_Fixnum_Value( Top(1) );
		
		Make_Fixnum_Number( 1 );

		while (index--)
		{
			Push( Top(2) );
			Push( Value_Register );
			Number_Multiply();
			Pop(2);
		}
	}
	else
	{
		/* Negative Exponent */

		Promote(1, REAL_LEVEL);
		Promote(2, REAL_LEVEL);


		Make_Real_Number( pow (Get_Number_Real_Value(Top(2)),
					Get_Number_Real_Value(Top(1))) );

	}
}
 
Public void Fixnum_Sin()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( sin (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Cos()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( cos (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Tan()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( tan (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Asin()
{
	if( (Get_Number_Fixnum_Value(Top(1)) < -1) ||
		(Get_Number_Fixnum_Value(Top(1)) > 1) )
	{
		Error("Argument to asin must lie between -1 and 1, inclusive");
	}
	 
	Promote(1, REAL_LEVEL);

	Make_Real_Number( asin (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Acos()
{
	if( (Get_Number_Fixnum_Value(Top(1)) < -1) ||
		(Get_Number_Fixnum_Value(Top(1)) > 1) )
	{
		Error("Argument to acos must lie between -1 and 1, inclusive");
	}
	 
	Promote(1, REAL_LEVEL);

	Make_Real_Number( acos (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Atan()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( atan (Get_Number_Real_Value(Top(1))) );
}


 
Public void Fixnum_Atan2()
{
	Promote(1, REAL_LEVEL);
	Promote(2, REAL_LEVEL);

	Make_Real_Number( atan2 (Get_Number_Real_Value(Top(2)), 
					Get_Number_Real_Value(Top(1))) );
}
 
/* Transfer functions */


Public void Fixnum_Exact_To_Inexact()
{
	Make_Real_Number( (double) Get_Number_Fixnum_Value( Top(1) ) );
}


 
Public void Fixnum_Inexact_To_Exact()
{
	Value_Register = Top(1);
}


 

Public void Fixnum_To_String()
{
	Character fixed_string[MAX_CHARS_PER_INT];
	Integer	radix = Number_To_Integer( Top(1) );
	Integer	value = Get_Number_Fixnum_Value( Top(2) );

	switch ( radix )
	{
	case 2:
	case 8:
	case 16:
		Make_Constant_String( 
		    Integer_To_Cstring( value , fixed_string , radix , 0 ) );
		return;
	case 10:
		sprintf(fixed_string, "%d", value );
		Make_Constant_String( fixed_string );
		return;
	default:
		Display_Error( "Unknown radix : " , Top(1) );
	}
}

Public void Fixnum_Make_Rectangular()
{
	Make_Complex_Number( (Double) Get_Number_Fixnum_Value(Top(2)),
			     (Double) Get_Number_Fixnum_Value(Top(1)));
}


 
Public void Fixnum_Make_Polar()
{
	Double	mag = Get_Number_Real_Value(Top(2));
	Double	ang = Get_Number_Real_Value(Top(1));

	Make_Complex_Number( (mag) * cos(ang) , (mag) * sin(ang) );
}



Public void Fixnum_Real_Part()
{
	Value_Register = Top(1);
}


 
Public void Fixnum_Imaginary_Part()
{
	Make_Fixnum_Number(0);
}


 
Public void Fixnum_Magnitude()
{
	Fixnum_Abs();
}

 

Public void Fixnum_Angle()
{
	Make_Fixnum_Number(0);
}
