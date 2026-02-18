/* complex.c -- UMB Scheme, complex number package.

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

#define A (Get_Number_Complex_Real_Part(Top(2)))
#define B (Get_Number_Complex_Imaginary_Part(Top(2)))
#define C (Get_Number_Complex_Real_Part(Top(1)))
#define D (Get_Number_Complex_Imaginary_Part(Top(1)))

#define PI	3.14159265358979323846
#define HALF_PI	1.57079632679489661923


/* Predicates. */

Public Boolean Is_Complex_Zero()
{
	return ( (C == 0.0) && (D == 0.0) );
}


Public Boolean Is_Complex_Positive()
{
        Error("No Positive/Negative Concept about Complex!");
        return FALSE;
}


Public Boolean Is_Complex_Negative()
{
        Error("No Positive/Negative Concept about Complex!");
        return FALSE;
}



Public Boolean Is_Complex_Odd()
{
        Error("No Odd/Even Concept about Complex!");
        return FALSE;
}



Public Boolean Is_Complex_Even()
{
        Error("No Odd/Even Concept about Complex!");
        return FALSE;
}



Public Boolean Is_Complex_Exact()
{
	return FALSE;	/* Complex numbers are always inexact */
}


Public Boolean Is_Complex_Inexact()
{
        return TRUE;	/* Complex numbers are always inexact */
}

/* Comparisons. */


Public Boolean Complex_Less_Than()
{
        Error("No Inequality about Complex!");
	return FALSE;
}



Public Boolean Complex_Greater_Than()
{
        Error("No Inequality about Complex!");
	return FALSE;
}



Public Boolean Complex_Equal()
{
	return ( (A == C) && (B == D) );
}


Public Boolean Complex_Less_Than_Or_Equal()
{
        Error("No Inequality about Complex!");
	return FALSE;
}


Public Boolean Complex_Greater_Than_Or_Equal()
{
        Error("No Inequality about Complex!");
        return FALSE;
}


/* Arithmetic. */


Public void Complex_Add()
{
	Make_Complex_Number( A + C, B + D );
}



Public void Complex_Subtract()
{
	Make_Complex_Number( A - C, B - D );
}



Public void Complex_Multiply()
{
	Make_Complex_Number( ( (A*C) - (B*D) ), ( (A*D) + (B*C) ) );
}



Public void Complex_Divide()
{
	if (Is_Complex_Zero())           /* C+Di = 0+0i, it's on top(1) */
		Error("Complex division by ZERO !!!!");
	else
		Make_Complex_Number( 	( (A*C)+(B*D) ) / ( (C*C)+(D*D) ),
			     		( (B*C)-(A*D) ) / ( (C*C)+(D*D) ) );
}



Public void Complex_Quotient()
{
        Error("No Quotient Operation on Complex!");
}



Public void Complex_Remainder()
{
        Error("No Remainder Operation on Complex!");
}



Public void Complex_Modulo()
{
	Error("No Modulo Operation on Complex!");
} 




Public void Complex_Negate()
{
	Value_Register = Copy_Object(Top(1), Complex_Size);

	Get_Number_Complex_Real_Part(Value_Register) =
		- Get_Number_Complex_Real_Part(Value_Register);


	Get_Number_Complex_Imaginary_Part(Value_Register) =
		- Get_Number_Complex_Imaginary_Part(Value_Register); 

}



Public void Complex_Abs()
{
        Make_Real_Number(sqrt( (C*C) + (D*D) ));
}



Public void Complex_Numerator()
{
	Error("No Numerator Operation on Complex!");
}



Public void Complex_Denominator()
{
	Error("No Denominator Operation on Complex!");
}
 


Public void Complex_Rationalize()
{
	Error("No Rationalize Operation on Complex!");
}


 
Public void Complex_Max()
{
        Error("No Max Operation on Complex!");
}



Public void Complex_Min()
{
        Error("No Min Operation on Complex!");
}



Public void Complex_GCD()
{
	Error("No GCD Operation on Complex!");
}


 
Public void Complex_LCM()
{
	Error("No LCM Operation on Complex!");
}



Public void Complex_Floor()
{
	Error("No Floor Operation on Complex!");
}

 

Public void Complex_Ceiling()
{
	Error("No Ceiling Operation on Complex!");
}

 

Public void Complex_Truncate()
{
	Error("No Truncate Operation on Complex!");
}


 
Public void Complex_Round()
{
	Error("No Round Operation on Complex!");
}


 
 
Public void Complex_Sqrt()
	/* sqrtz = sqrt(|z|)*( cos((angle+2kPi)/2) + isin((angle+2kPi)/2) ) */
{
        Double mag_sqrt = sqrt(sqrt( (C*C) + (D*D) ));
	Double ang = ( C == 0.0 && D > 0.0 ) ? HALF_PI :
		     ( ( C == 0.0 && D < 0.0 ) ? -HALF_PI : atan2(D,C) );

	/* select a proper angle so that 
				the real part of the result is non-negative */

        if (cos(ang/2.0) >= 0.0) ang /= 2.0;
        else ang = (ang/2.0) + PI;

        Make_Complex_Number(mag_sqrt * cos(ang), mag_sqrt * sin(ang));
}



Public void Complex_Exp()
{
        Make_Complex_Number( exp(C) * cos(D) , exp(C) * sin(D) );
}



Public void Complex_Log()
	/* logz = log(magnitude(z)) + angle(z)i */
{
	Double ang = ( C == 0.0 && D > 0.0 ) ? HALF_PI :
		     ( ( C == 0.0 && D < 0.0 ) ? -HALF_PI : atan2(D,C) );

	if (Is_Complex_Zero()) Display_Error("Bad arg to Log : ", Top(1) );
	else  Make_Complex_Number( log(sqrt(C*C + D*D)), ang ); 
}



Public void Complex_Expt()
        /* z1**z2 = e**z2log(z1)   ;  0**0 is defined to be 1 */
{

#define Z1_and_Z2_are_Zeros (A == 0.0 && B == 0.0) && (C == 0.0 && D == 0.0)
#define Z1_is_Zero  A == 0.0 && B == 0.0

	Promote( 2 , COMPLEX_LEVEL );
	if ( Z1_and_Z2_are_Zeros )  Make_Complex_Number(1.0, 0.0);
	else if ( Z1_is_Zero )    Display_Error("Bad arg to Expt:", Top(2));
	else 
	{
        	Push(Top(2)); 		/* push z1 */
        	Complex_Log();  	/* compute logz1 */
        	Pop(1);			/* remove z1 */
        	Push(Value_Register);	/* push logz1 */
        	Complex_Multiply();     /* compute z2logz1 */
        	Pop(1);			/* remove logz1 */
        	Push(Value_Register);	/* push z2logz1 */
        	Complex_Exp();		/* compute e**z2logz1 */
        	Pop(1);			/* remove z2logz1 */
     	}
#undef Z1_and_Z2_are_Zeros
#undef Z1_is_Zero 
}



Public void Complex_Sin()
	/* sinz = (e**iz - e**-iz) / 2i */
{
        Make_Complex_Number( ((exp(-D) * sin(C)) - (exp(D) * sin(-C))) / 2.0 , 
			     ((exp(D) * cos(-C)) - (exp(-D) * cos(C))) / 2.0 );
}



Public void Complex_Cos()
	/* cosz = (e**iz + e**-iz) / 2 */
{
        Make_Complex_Number(((exp(-D) * cos(C)) + (exp(D) * cos(-C))) / 2.0 ,
                            ((exp(-D) * sin(C)) + (exp(D) * sin(-C))) / 2.0 );
}



Public void Complex_Tan()
	/* tanz = sinz / cosz */
{
        Complex_Sin();          /* compute sin(z) */
        Push( Value_Register ); /* push sin(z) onto stack */
        Push( Top(2) );         /* swap z upto top to compute cos(z) */
        Complex_Cos();          /* compute cos(z) */
        Pop(1);                 /* remove the temp z */
        Push( Value_Register ); /* push cos(z) onto stack */
        Complex_Divide();       /* compute tan(z) = sin(z) / cos(z) */
        Pop(2);                 /* remove sin(z) and cos(z) off the stack */
}



Public void Complex_Asin()
	/* asinz = -ilog(iz + sqrt(1-z**2)) */
{
        Make_Complex_Number( 1.0 - (C*C) + (D*D), -(2.0*(C*D)) ); 
						/* Make 1-z**2 */
	Push( Value_Register ); 	/* push 1-z**2 onto stack */
        Complex_Sqrt();         	/* compute sqrt (1-z**2) */
        Pop(1);				/* remove 1-z**2 */
        Push( Value_Register ); 	/* push sqrt(1-z**2) */
        Make_Complex_Number( -B, A);	/* make iz */
        Push( Value_Register ); 	/* push iz */
        Complex_Add();			/* compute iz + sqrt(1-z**2) */
        Pop(2);				/* remove iz and sqrt(1-z**2) */
        Push( Value_Register ); 	/* push iz + sqrt(1-z**2) */
        Complex_Log();			/* compute log(iz + sqrt(1-z**2)) */
        Pop(1);				/* remove iz + sqrt(1-z**2) */
        Push( Value_Register );		/* push log(iz + sqrt(1-z**2)) */
        Make_Complex_Number( D, -C);	/* compute -ilog(iz + sqrt(1-z**2)) */
        Pop(1);				/* remove log(iz + sqrt(1-z**2)) */

}




Public void Complex_Acos()
	/* acosz = -ilog(z + i*sqrt(1-z**2)) */
{
        Make_Complex_Number(1.0-(C*C)+(D*D), -(2.0*(C*D)) ); /* make 1-z**2 */
        Push( Value_Register ); 	/* push 1-z**2 */
        Complex_Sqrt();         	/* compute sqrt (1-z**2) */
        Pop(1);				/* remove 1-z*2 */
        Push( Value_Register );		/* push sqrt(1-z**2) */
        Make_Complex_Number( 0.0,1.0);  /* make i */
        Push( Value_Register ); 	/* push i */
        Complex_Multiply();		/* compute i*sqrt(1-z**2) */
        Pop(2);				/* remove i and sqrt(1-z**2) */
        Push( Value_Register ); 	/* push i*sqrt(1-z**2) */
        Complex_Add();			/* compute z + i*sqrt(1-z**2) */
        Pop(1);				/* remove i*sqrt(1-z**2) */
        Push( Value_Register );		/* push z + i*sqrt(1-z**2) */
        Complex_Log();			/* compute log(z + i*sqrt(1-z**2)) */
        Pop(1);				/* remove z + i*sqrt(1-z**2) */
        Push( Value_Register );		/* push log(z + i*sqrt(1-z**2)) */
        Make_Complex_Number( D, -C);	/* compute -ilog(z + i*sqrt(1-z**2)) */
        Pop(1);				/* remove log(z + i*sqrt(1-z**2)) */

}



Public void Complex_Atan()
	/* atanz = -ilog((1+iz)*sqrt(1/(1+z**2))) */
{
        Make_Complex_Number(1.0,0.0);	/* make 1 */
        Push(Value_Register);		/* push 1 */
        Make_Complex_Number(1.0+(A*A)-(B*B), 2.0*(A*B)); /* make 1+z**2 */
        Push( Value_Register ); 	/* push 1+z**2 */
        Complex_Divide();		/* compute 1 / (1+z**2) */
        Pop(2);				/* remove 1 and 1+z**2 */
        Push(Value_Register);		/* push 1/(1+z**2) */
        Complex_Sqrt();         	/* compute sqrt(1/(1+z**2)) */
        Pop(1);				/* remove 1/(1+z**2) */
        Push( Value_Register );		/* push sqrt(1/(1+z**2)) */
        Make_Complex_Number( 1.0 - B, A); /* make 1+iz */
        Push( Value_Register );		/* push 1+iz */
	Complex_Multiply();     	/* compute (1+iz)(sqrt(1/(1+z**2))) */
        Pop(2); 			/* remove 1+iz and sqrt(1/(1+z**2)) */
        Push( Value_Register ); 	/* push (1+iz)(sqrt(1/(1+z**2))) */
        Complex_Log(); 		/* compute log((1+iz)(sqrt(1/(1+z**2))) */
        Pop(1);				/* remove (1+iz)(sqrt(1/(1+z**2))) */
        Push( Value_Register ); 	/* push log((1+iz)(sqrt(1/(1+z**2))) */
        Make_Complex_Number( D, -C);/* compute -ilog((1+iz)(sqrt(1/(1+z**2))) */
        Pop(1);			/* remove log((1+iz)(sqrt(1/(1+z**2))) */

}



Public void Complex_Atan2()
{
	Error("No Atan2 Operation on Complex!");
}


 

Public void Complex_Exact_To_Inexact()
{
	Value_Register = Top( 1 );
}



Public void Complex_Inexact_To_Exact()
{
        Error("inexact->to->exact is not applicable to complex numbers");
}




Public void Complex_To_String()
{
	Import	void	String_Append();
	Integer	radix = Number_To_Integer( Top(1) );

	if ( A == 0.0 )
	{
		Make_Constant_String( 	radix == 2 ? "#b" :
					radix == 8 ? "#o" :
					radix == 10 ? "" : "#x" );
		Push( Value_Register );
	}
	else
	{
		Make_Real_Number( A );
		Push( Value_Register );
		Push( Top(2) );
		Number_To_String(); Pop(2);
		Push( Value_Register );
	}

	if ( Get_Number_Complex_Imaginary_Part( Top(3) ) >= 0 )
	{
		Make_Constant_String( "+" );
		Push( Value_Register );
		String_Append(); Pop(2);
		Push( Value_Register );
	}

	Make_Real_Number( Get_Number_Complex_Imaginary_Part( Top(3) ) );
	Push( Value_Register );
	Push( Top(3) );
	Number_To_String(); Pop(2);
	Push( Value_Register );
	if ( radix != 10 )
	{
		/* remove radix prefix from imaginary part */

		Make_Constant_String( Get_String_Value( Top(1) ) + 2 ); Pop(1);
		Push( Value_Register );
	}
	String_Append(); Pop(2);
	Push( Value_Register );

	Make_Constant_String( "i" );
	Push( Value_Register );
	String_Append(); Pop(2);
}

Public void Complex_Make_Rectangular()
        /* Make a Complex in rectangular form; z = x + yi */
{
        Tower_Position argtypeX = Get_Number_Tower_Position(Top(2));
        Tower_Position argtypeY = Get_Number_Tower_Position(Top(1));

        if (argtypeX == COMPLEX_LEVEL || argtypeY == COMPLEX_LEVEL)
        {
                Error("Bad arguments to Make-Rectangular.");
        }

}



Public void Complex_Make_Polar()
{
        Tower_Position argtypeX = Get_Number_Tower_Position(Top(2));
        Tower_Position argtypeY = Get_Number_Tower_Position(Top(1));

        if (argtypeX == COMPLEX_LEVEL || argtypeY == COMPLEX_LEVEL)
        {
                Error("Bad arguments to Make-Rectangular.");
        }
}



Public void Complex_Real_Part()
        /* Return the real part of a complex in rectangular form */
{
        Make_Real_Number( C );
}




Public void Complex_Imaginary_Part()
        /* Return the imaginary part of a complex in rectangular form */
{
        Make_Real_Number( D );
}




Public void Complex_Magnitude()
        /* Return the Magnitude of a complex, */
{
        Make_Real_Number( sqrt( (C*C) + (D*D) ) );
}




Public void Complex_Angle()
        /* Return the angle of a complex */
{
	Double ang;
	
	ang = ( C == 0.0 && D > 0.0 ) ? HALF_PI :
		     ( ( C == 0.0 && D < 0.0 ) ? -HALF_PI : atan2(D,C) );

	if (Is_Complex_Zero()) Make_Real_Number( 0.0 );
        else Make_Real_Number( ang );

}


#undef A
#undef B
#undef C
#undef D
