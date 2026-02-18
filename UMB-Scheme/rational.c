/* rational.c -- UMB Scheme, specific rational number procedures.

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

Private Short Rational_Compare();



/* Predicates. */

Public Boolean Is_Rational_Zero()
{
  	return (Number_Sign(Get_Number_Rational_Numerator(Top(1))) == 0);;
}

Public Boolean Is_Rational_Positive()
{
	return (Number_Sign(Get_Number_Rational_Numerator(Top(1))) > 0);
}

Public Boolean Is_Rational_Negative()
{
	return(Number_Sign(Get_Number_Rational_Numerator(Top(1))) < 0);
}

Public Boolean Is_Rational_Odd()
{
	Error("Rationals aren't odd or even");
	return FALSE;
}

Public Boolean Is_Rational_Even()
{
	Error("Rationals aren't odd or even");
	return FALSE;
}

Public Boolean Is_Rational_Exact()
{
	return (Is_Exact_Number(Top(1)));
}

Public Boolean Is_Rational_Inexact()
{
	return (! Is_Exact_Number(Top(1)));
}



/* Comparisons. */

Public Boolean Rational_Less_Than()
{
	return Rational_Compare() < 0;
}

Public Boolean Rational_Greater_Than()
{
	return Rational_Compare() > 0;
}

Public Boolean Rational_Equal()
{
	return Rational_Compare() == 0;
}

Public Boolean Rational_Less_Than_Or_Equal()
{
	return Rational_Compare() <= 0;
}

Public Boolean Rational_Greater_Than_Or_Equal()
{
	return Rational_Compare() >= 0;
}

Private Short Rational_Compare()
{
	Rational_Subtract();
	return Number_Sign( Get_Number_Rational_Numerator( Value_Register ));
}




/* Arithmetic. */

Public void Rational_Add()
{
	/* rat 1 = a/b, rat 2 = c/d, result = (a*d + b*c)/b*d 	*/

	Push(Get_Number_Rational_Numerator(Top( 2 )));    /* numer rat 1 */
	Push(Get_Number_Rational_Denominator(Top( 2 )));  /* denom rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );

	Push(Get_Number_Rational_Denominator(Top( 3 )));  /* denom rat 1 */
	Push(Get_Number_Rational_Numerator(Top( 3 )));	/* numer rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );

	/* add 2 new numerators */
	Number_Add();
	Pop( 2 );
	Push( Value_Register );

	Push(Get_Number_Rational_Denominator(Top( 3 )));  /* denom rat 1 */
	Push(Get_Number_Rational_Denominator(Top( 3 ))); /* denom rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );	/* denominator */

	Make_Rational_Number();
	Pop( 2 );

	if ( ! Is_Exact_Number( Top(1) ) || ! Is_Exact_Number( Top(2) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}



Public void Rational_Subtract()
{
	/* rat 1 = a/b, rat 2 = c/d, result = (a*d - b*c)/b*d 	*/

	Push(Get_Number_Rational_Numerator(Top( 2 )));    /* numer rat 1 */
	Push(Get_Number_Rational_Denominator(Top( 2 )));  /* denom rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );

	Push(Get_Number_Rational_Denominator(Top( 3 )));  /* denom rat 1 */
	Push(Get_Number_Rational_Numerator(Top( 3 )));	/* numer rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );

	/* subtract 2 new numerators */
	Number_Subtract();
	Pop( 2 );
	Push( Value_Register );

	Push(Get_Number_Rational_Denominator(Top( 3 )));  /* denom rat 1 */
	Push(Get_Number_Rational_Denominator(Top( 3 ))); /* denom rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );	/* denominator */

	Make_Rational_Number();
	Pop( 2 );

	if ( ! Is_Exact_Number( Top(1) ) || ! Is_Exact_Number( Top(2) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}



Public void Rational_Multiply()
{
	/* rat 1 = a/b, rat 2 = c/d, result = a*c/b*d	*/

	Push(Get_Number_Rational_Numerator(Top( 2 )));	/* numer rat 1 */
	Push(Get_Number_Rational_Numerator(Top( 2 )));	/* numer rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );	/* new numer of result rat */

	Push(Get_Number_Rational_Denominator(Top( 2 ))); /* denom rat 2 */
	Push(Get_Number_Rational_Denominator(Top( 4 ))); /* denom rat 1 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );	/* new denom of result rat */

	Make_Rational_Number();
	Pop( 2 );

	if ( ! Is_Exact_Number( Top(1) ) || ! Is_Exact_Number( Top(2) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}


Public void Rational_Divide()
{
	/* rat 1 = a/b, rat 2 = c/d, result = a*d/c*b	*/

	Push(Get_Number_Rational_Numerator(Top( 2 )));	/* numer rat 1 */
	Push(Get_Number_Rational_Denominator(Top( 2 ))); /* denom rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );	/* new numer of result rat */

	Push(Get_Number_Rational_Denominator(Top( 3 ))); /* denom rat 1 */
	Push(Get_Number_Rational_Numerator(Top( 3 )));	/* numer rat 2 */
	Number_Multiply();
	Pop( 2 );
	Push( Value_Register );	/* new denom of result rat */

	Make_Rational_Number();
	Pop( 2 );

	if ( ! Is_Exact_Number( Top(1) ) || ! Is_Exact_Number( Top(2) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}


Public void Rational_Quotient()
{
	Error("Quotient makes no sense on rationals");
}

Public void Rational_Remainder()
{
	Error("Remainder makes no sense on rationals");
}


Public void Rational_Modulo()
{
	Error("Modulo makes no sense on rationals");
}


Public void Rational_Negate()
{
	Push(Get_Number_Rational_Numerator(Top(1)));
	Number_Negate();
	Replace(1, Value_Register);
	Push(Get_Number_Rational_Denominator(Top(2)));
	Make_Rational_Number();
	Pop(2);	

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}


Public void Rational_Abs()
{
	if (Is_Rational_Negative())
	{
		Rational_Negate();
	}
	else
	{
		Value_Register = Top(1);
	}
}

Public void Rational_Numerator()
{
	
	Value_Register = Get_Number_Rational_Numerator(Top( 1 ));

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}


Public void Rational_Denominator()
{
	
	Value_Register = Get_Number_Rational_Denominator(Top( 1 ));

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}


Public void Rational_Rationalize()
{
	Error("Rational_Rationalize is not yet implemented");
}



Public void Rational_Max()
{
	Value_Register = Rational_Greater_Than() ? Top(2) : Top(1);

	if ( ! Is_Exact_Number( Top(1) ) || ! Is_Exact_Number( Top(2) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}

Public void Rational_Min()
{
	Value_Register = Rational_Less_Than() ? Top(2) : Top(1);

	if ( ! Is_Exact_Number( Top(1) ) || ! Is_Exact_Number( Top(2) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}

Public void Rational_GCD()
{
	Error("GCD makes no sense on rationals");
}


Public void Rational_LCM()
{
	Error("LCM makes no sense on rationals");
}

Public void Rational_Floor()
{
	Value_Register = Copy_Object(Top(1), Rational_Size);

	Push(Get_Number_Rational_Numerator(Value_Register));
	Push(Get_Number_Rational_Denominator(Value_Register));
	Push(Top(2));

	Is_Number_Negative();
	Pop(1);

	if (Value_Register == The_True_Object )
	{
		Number_Quotient();
		Push(Value_Register);
		Make_Fixnum_Number(1);
		Push(Value_Register);
		Number_Subtract();
		Pop(4);
	}
	else
	{
		Number_Quotient();
		Pop(2);
	}

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}
 
Public void Rational_Ceiling()
{
	Value_Register = Copy_Object(Top(1), Rational_Size);

	Push(Get_Number_Rational_Numerator(Value_Register));
	Push(Get_Number_Rational_Denominator(Value_Register));

	Number_Remainder();
	Push(Value_Register);
	Is_Number_Zero();
	Pop(1);

	if (Value_Register == The_True_Object)
	{
		Push(Top(3));
		Rational_Floor();	
		Pop(3);
	}
	else
	{
		Push(Top(3));
		Rational_Floor();
		Push(Value_Register);
		Make_Fixnum_Number(1);
		Push(Value_Register);
		Number_Add();
		Pop(5);
	}

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}


 
Public void Rational_Truncate()
{
	if (Number_Sign(Get_Number_Rational_Numerator(Top(1))) > 0)
	{
		Rational_Floor();
	}
	else
	{
		Rational_Ceiling();
	}

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}

 

Public void Rational_Round()
{
	/* Compare the given rat to average of floor and ceiling and choose
	   either floor or ceiling depending on result and sign of the rat;
	   Numbers halfway between two integers round to even. */

	
	Rational_Floor();
	Push(Value_Register);
	Push(Top(2));
	Rational_Ceiling();
	Push(Value_Register);
	Push(Top(3));
	Number_Add();
	Push(Value_Register);
	Make_Fixnum_Number(2);
	Push(Value_Register);
	Number_Divide();
	Push(Value_Register);
	Push(Top(6));
	Number_Less_Than();

	if (Value_Register == The_True_Object)
	{
		Push(Top(7));
		Rational_Ceiling();
		Pop(9);
	}
	else 
	{
		Number_Greater_Than();
		if(Value_Register == The_True_Object)
		{
			Push(Top(7));
			Rational_Floor();
			Pop(9);
		}
		else
		{
			Push(Top(7));
			Rational_Floor();
			Push(Value_Register);
			Is_Number_Even();
			if(Value_Register == The_True_Object)
			{
				Value_Register = Top(1);
				Pop(10);
			}
			else
			{
				Pop(1);
				Rational_Ceiling();
				Pop(9);
			}
		}
	}

	if ( ! Is_Exact_Number( Top(1) ) )
	{
		Push( Value_Register );
		Number_Exact_To_Inexact(); Pop(1);
	}
}




Public void Rational_Sqrt()
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


 
Public void Rational_Exp()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( exp (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Log()
{
	if(Number_Sign(Get_Number_Rational_Numerator(Top(1))) < 0)
	{
		Error("Argument of log must be nonnegative");
	}

	Promote(1, REAL_LEVEL);

	Make_Real_Number( log (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Expt()
{
	Promote(1, REAL_LEVEL);
	Promote(2, REAL_LEVEL);

	Make_Real_Number( pow ((Get_Number_Real_Value(Top(2))),
				(Get_Number_Real_Value(Top(1))) ) );
}



 
Public void Rational_Sin()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( sin (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Cos()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( cos (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Tan()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( tan (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Asin()
{
	Promote(1, REAL_LEVEL);

	if(((Get_Number_Real_Value(Top(1))) < -1) ||
		((Get_Number_Real_Value(Top(1))) > 1))
	{
		Error("Argument of asin must lie between -1 and 1, inclusive");
	}

	Make_Real_Number( asin (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Acos()
{
	Promote(1, REAL_LEVEL);

	if(((Get_Number_Real_Value(Top(1))) < -1) ||
		((Get_Number_Real_Value(Top(1))) > 1))
	{
		Error("Argument of acos must lie between -1 and 1, inclusive");
	}

	Make_Real_Number( acos (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Atan()
{
	Promote(1, REAL_LEVEL);

	Make_Real_Number( atan (Get_Number_Real_Value(Top(1))) );
}


 
Public void Rational_Atan2()
{
	Promote(1, REAL_LEVEL);
	Promote(2, REAL_LEVEL);

	Make_Real_Number( atan2 (Get_Number_Real_Value(Top(2)),   
				Get_Number_Real_Value(Top(1))) );

}


 
 
/* Transfer functions */


Public void Rational_Exact_To_Inexact()
{
	Value_Register = Copy_Object(Top(1), Rational_Size);
	Is_Exact_Number( Value_Register ) = FALSE;
}


 
Public void Rational_Inexact_To_Exact()
{
	Value_Register = Copy_Object(Top(1), Rational_Size);
	Is_Exact_Number( Value_Register ) = TRUE;
}
 

Public void Rational_To_String()
{
	Import	void String_Append();

	Integer radix = Number_To_Integer( Top(1) );

	Make_Constant_String( Is_Exact_Number( Top(2) ) ? "" : "#i" );
	Push( Value_Register );

	Push(Get_Number_Rational_Numerator(Top(3)));
	Push( Top(3) );
	Number_To_String(); Pop(2);
	Push( Value_Register );
	String_Append(); Pop(2);
	Push( Value_Register );

	Make_Constant_String( "/" );
	Push( Value_Register );
	String_Append(); Pop(2);
	Push( Value_Register );

	Push(Get_Number_Rational_Denominator(Top(3)));
	Push( Top(3) );
	Number_To_String(); Pop(2);
	Push( Value_Register );
	if ( radix != 10)
	{
		/* Remove radix prefix from denominator */

		Make_Constant_String( Get_String_Value( Top(1) ) + 2 ); Pop(1);
		Push( Value_Register );
	}
	String_Append(); Pop(2);
}

Public void Rational_Make_Rectangular()
{
	Promote(1, REAL_LEVEL);
	Promote(2, REAL_LEVEL);

	Make_Complex_Number( Get_Number_Real_Value(Top(2)),
				Get_Number_Real_Value(Top(1)));
}


 
Public void Rational_Make_Polar()

#define MAG Get_Number_Real_Value(Top(2))
#define ANG Get_Number_Real_Value(Top(1))

{
	Promote( 2, REAL_LEVEL);
	Promote( 1, REAL_LEVEL);

	Make_Complex_Number( (MAG) * cos(ANG) , (MAG) * sin(ANG) );

}

#undef MAG
#undef ANG



 
Public void Rational_Real_Part()
{
	Promote(1, REAL_LEVEL);
	Value_Register = Top(1);
}


 
Public void Rational_Imaginary_Part()
{
	Make_Real_Number(0.0);
}


 
Public void Rational_Magnitude()
{
	Rational_Abs();
}

 

Public void Rational_Angle()
{
	Make_Real_Number(0.0);
}



Public	void Reduce_Rational()
{
	/* To reduce a rational you divide the numerator and
	   denominator by their GCD */


	Push( Value_Register ); /* The rational */

	Push(Get_Number_Rational_Numerator(Value_Register));
	Push(Get_Number_Rational_Denominator(Value_Register));
	Number_GCD(); 
	Pop(2);
	Push( Value_Register );	/* The gcd */

	Push( Get_Number_Rational_Numerator(Top(2)) )
	Push(Top(2));
	Number_Quotient();
	Pop(2);
	Get_Number_Rational_Numerator(Top(2)) = Value_Register;

	Push( Get_Number_Rational_Denominator( Top(2) ) );
	Push(Top(2));
	Number_Quotient();
	Pop(2);
	Get_Number_Rational_Denominator(Top(2)) = Value_Register;

	Value_Register = Top(2);
	Pop(2);
}	
