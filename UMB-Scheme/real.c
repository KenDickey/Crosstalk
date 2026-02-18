/* real.c -- UMB Scheme, specific realnum procedures.

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

/* Reals are implemented as the native double precision floating
point type. As a result, all the operations can use native (C)
functions.  */

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
 


/* Predicates. */
Public Boolean Is_Real_Zero()
{
	return Get_Number_Real_Value(Top(1)) == 0.0;
}

Public Boolean Is_Real_Positive()
{
	return Get_Number_Real_Value(Top(1)) > 0.0;
}

Public Boolean Is_Real_Negative()
{
	return Get_Number_Real_Value(Top(1)) < 0.0;
}

Public Boolean Is_Real_Odd()
{
	Error("Reals aren't odd or even");
	return FALSE;
}

Public Boolean Is_Real_Even()
{
	Error("Reals aren't even or odd");
	return FALSE;
}

Public Boolean Is_Real_Exact()
{
	return FALSE;
}

Public Boolean Is_Real_Inexact()
{
	return TRUE;
}




/* Comparisons. */
Public Boolean Real_Less_Than()
{
	return Get_Number_Real_Value(Top(2)) < Get_Number_Real_Value(Top(1));
}

Public Boolean Real_Greater_Than()
{
	return Get_Number_Real_Value(Top(2)) > Get_Number_Real_Value(Top(1));
}

Public Boolean Real_Equal()
{
	return Get_Number_Real_Value(Top(2)) == Get_Number_Real_Value(Top(1));
}

Public Boolean Real_Less_Than_Or_Equal()
{
	return Get_Number_Real_Value(Top(2)) <= Get_Number_Real_Value(Top(1));
}

Public Boolean Real_Greater_Than_Or_Equal()
{
	return Get_Number_Real_Value(Top(2)) >= Get_Number_Real_Value(Top(1));
}




/* Arithmetic. */
Public void Real_Add()
{
	Make_Real_Number(Get_Number_Real_Value(Top(2))+
	Get_Number_Real_Value(Top(1)));
}



Public void Real_Subtract()
{
	Make_Real_Number(Get_Number_Real_Value(Top(2))-
	Get_Number_Real_Value(Top(1)));
}



Public void Real_Multiply()
{
	Make_Real_Number(Get_Number_Real_Value(Top(2))*
	Get_Number_Real_Value(Top(1)));
}



Public void Real_Divide()
{
	Make_Real_Number(Get_Number_Real_Value(Top(2))/
	Get_Number_Real_Value(Top(1)));
}



Public void Real_Quotient()
{
	Error("Quotient makes no sense on reals");
}



Public void Real_Remainder()
{
	Error("Remainder makes no sense on reals");
}



Public void Real_Modulo()
{
	Error("Modulo makes no sense on reals");
}



Public void Real_Negate()
{
	Value_Register = Copy_Object(Top(1), Real_Size);

	Get_Number_Real_Value(Value_Register) =
	    - Get_Number_Real_Value(Value_Register);
}



Public void Real_Abs()
{
	if (Is_Real_Negative())
	{
		Real_Negate();		
	}
	else
	{
		Value_Register = Top(1);
	}
}


 
Public void Real_Numerator()
{
	Error("Numerator makes no sense on reals");
}


 
Public void Real_Denominator()
{
	Error("Denominator makes no sense on reals");
}


 
Public void Real_Rationalize()
{
	Error("Real_Rationaize is not yet implemented");
}

/* And other operations. */

Public void Real_Max()
{
	Value_Register =
	    (Get_Number_Real_Value(Top(2)) > Get_Number_Real_Value(Top(1)))
	    ? Top(2)
	    : Top(1) ;
}



Public void Real_Min()
{
	Value_Register =
	    (Get_Number_Real_Value(Top(2)) < Get_Number_Real_Value(Top(1)))
	    ? Top(2)
	    : Top(1) ;
}



Public void Real_GCD()
{
	Error("GCD makes no sense on reals");
}


 
Public void Real_LCM()
{
	Error("LCM makes no sense on reals");
}


 

Public void Real_Floor()
{
	Make_Real_Number( floor( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Ceiling()
{
	Make_Real_Number( ceil( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Truncate()
{
	if (Is_Real_Positive())
	{
		Real_Floor();
	}
	else
	{
		Real_Ceiling();
	}

}


 
Public void Real_Round()
{
	/* Compare real to average of floor and ceiling and
	   choose either floor or ceiling depending on result;
	   reals ending in .5 round to even */

	Double input = Get_Number_Real_Value(Top(1));

	if (input < (floor(input) + ceil(input))/2 )
	{
		Real_Floor();
	}
	else if (input > (floor(input) + ceil(input))/2 )
	{
		Real_Ceiling();
	}
	else
	{
		Number_Floor();
		Push(Value_Register);
		Number_Inexact_To_Exact(); 
		Push( Value_Register );
		Is_Number_Even(); Pop(1);
		if (Value_Register == The_True_Object)
		{
			Value_Register = Top(1);
			Pop(1);
		}
		else
		{
			Pop(1);
			Real_Ceiling();
		}
	}
}




Public void Real_Sqrt()
{
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


 
Public void Real_Exp()
{
	Make_Real_Number( exp( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Log()
{
	if(Get_Number_Real_Value(Top(1)) <= 0)
	{
		Error("Argument of log must be positive");
	}

	Make_Real_Number( log( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Expt()
{
	Promote( 2 , REAL_LEVEL );
	Make_Real_Number( pow( Get_Number_Real_Value(Top(2)),
				Get_Number_Real_Value(Top(1))));
}



 
Public void Real_Sin()
{
	Make_Real_Number( sin( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Cos()
{
	Make_Real_Number( cos( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Tan()
{
	Make_Real_Number( tan( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Asin()
{
	if( (Get_Number_Real_Value(Top(1)) < -1) ||
		(Get_Number_Real_Value(Top(1)) > 1) )
	{
		Error("Argument of asin must lie between -1 and 1, inclusive");
	}

	Make_Real_Number( asin( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Acos()
{
	if( (Get_Number_Real_Value(Top(1)) < -1) ||
		(Get_Number_Real_Value(Top(1)) > 1) )
	{
		Error("Argument of acos must lie between -1 and 1, inclusive");
	}

	Make_Real_Number( acos( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Atan()
{
	Make_Real_Number( atan( Get_Number_Real_Value(Top(1))));
}


 
Public void Real_Atan2()
{
	Make_Real_Number( atan2( Get_Number_Real_Value(Top(2)),
				Get_Number_Real_Value(Top(1))));
}


 
 
/* Transfer functions */


Public void Real_Exact_To_Inexact()
{
	Value_Register = Top(1);
}


Public	void Real_Inexact_To_Exact()
{
	Push( Top(1) );
	Make_Real_Number( DBL_MIN );
	Push( Value_Register );
	Number_Rationalize(); Pop(2);
	Is_Exact_Number( Value_Register ) = TRUE;
}

 
Public void XReal_Inexact_To_Exact()
{
	Double 	whole;
	Double	fraction;

	Double  quotient;
	Integer remainder;


	whole = floor( fabs( Get_Number_Real_Value( Top(1) ) ) );
	fraction = fabs( Get_Number_Real_Value( Top(1) ) ) - whole;

	/* The whole part */

	quotient = floor( whole / RADIX );
	remainder = whole - (quotient * RADIX);
	whole = quotient;

	Integer_To_Number( remainder );
	Push( Value_Register );

	if ( whole > 0.0 )
	{
		Integer_To_Number( 1 );
		Push( Value_Register );

		while ( whole > 0.0 )
		{
			Push( Top(1) );
			Integer_To_Number( RADIX );
			Push( Value_Register );
			Number_Multiply(); Pop(2);
			Replace( 1 , Value_Register );

			quotient = floor( whole / RADIX );
			remainder = whole - (quotient * RADIX);
			whole = quotient;
			Push( Top(1) );
			Integer_To_Number( remainder );
			Push( Value_Register );
			Number_Multiply(); Pop(2);

			Push( Top(2) );
			Push( Value_Register );
			Number_Add(); Pop( 2 );
			Replace( 2 , Value_Register );
		}
		Pop( 1 );
	}

	/* The fraction */

	Integer_To_Number( 0 );
	Push( Value_Register );

	if ( fraction > 0.0 )
	{
		Integer_To_Number( 1 );
		Push( Value_Register );

		while ( fraction > 0.0 )
		{
			Push( Top(1) );
			Integer_To_Number( 10000 );
			Push( Value_Register );
			Number_Divide(); Pop(2);
			Replace( 1 , Value_Register );

			quotient =  floor( fraction * 10000 );
			fraction = (fraction * 10000) - quotient;
			Push( Top(1) );
			Integer_To_Number( (Integer) quotient );
			Push( Value_Register );
			Number_Multiply(); Pop(2);

			Push( Top(2) );
			Push( Value_Register );
			Number_Add(); Pop( 2 );
			Replace( 2 , Value_Register );
		}
		Pop( 1 );
	}
	Number_Add(); Pop(2);

	if ( Is_Real_Negative() )
	{
		Push( Value_Register );
		Number_Negate(); Pop(1);
	}

	Is_Exact_Number( Value_Register ) = TRUE;
}
	

 
Public void Real_To_String()
{
	Character real_string[MAX_CHARS_PER_REAL];
	Integer	radix = Number_To_Integer( Top(1) );
	Double	r = Get_Number_Real_Value( Top(2) );
	
	if (radix != 10)
	{
		Error( "(string->number <real> radix!=10) not implemented" );
	}

	if ( floor( r ) == r )
	{
		sprintf(real_string, "%.16g.0" , r ); 
	}
	else
	{
		sprintf(real_string, "%.16g" , r ); 
	}
	Make_Constant_String(real_string);
}

Public void Real_Make_Rectangular()
{
	Make_Complex_Number( Get_Number_Real_Value(Top(2)),
				Get_Number_Real_Value(Top(1)));
}


 
Public void Real_Make_Polar()

#define MAG Get_Number_Real_Value(Top(2))
#define ANG Get_Number_Real_Value(Top(1))

{
	Make_Complex_Number( (MAG) * cos(ANG) , (MAG) * sin(ANG) );	
}

#undef MAG
#undef ANG

 
 
Public void Real_Real_Part()
{
	Value_Register = Top(1);
}


 
Public void Real_Imaginary_Part()
{
	Make_Real_Number(0.0);
}


 
Public void Real_Magnitude()
{
	Real_Abs();
}

 

Public void Real_Angle()
{
	Make_Real_Number(0.0);
}
