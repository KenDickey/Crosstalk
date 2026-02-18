/* number.h -- UMB Scheme, number interface.

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

#define	Is_Fixnum(x)	(Get_Number_Tower_Position(x)==FIXNUM_LEVEL)
#define	Is_Bignum(x)	(Get_Number_Tower_Position(x)==BIGNUM_LEVEL)
#define	Is_Rational(x)	(Get_Number_Tower_Position(x)==RATIONAL_LEVEL)
#define	Is_Real(x)	(Get_Number_Tower_Position(x)==REAL_LEVEL)
#define	Is_Complex(x)	(Get_Number_Tower_Position(x)==COMPLEX_LEVEL)

#define MAX_CHARS_PER_INT	64



Import void 
	Is_Number_Zero(), Is_Number_Positive(), Is_Number_Negative(),
        Is_Number_Odd(), Is_Number_Even(), Is_Number_Exact(), 
	Is_Number_Inexact();

Import void
	Number_Less_Than(), Number_Greater_Than(), Number_Equal(),
        Number_Less_Than_Or_Equal(), Number_Greater_Than_Or_Equal();

Import void
	Number_Add(), Number_Subtract(), Number_Multiply(),
        Number_Divide(), Number_Quotient(), Number_Remainder(),
	Number_Modulo(), Number_Negate(), Number_Abs(),
	Number_Numerator(), Number_Denominator(), Number_Rationalize();
		
Import void
	Number_Max(), Number_Min(), Number_GCD(), Number_LCM();

Import void
	Number_Floor(), Number_Ceiling(), Number_Truncate(), Number_Round();

Import void
	Number_Sqrt(), Number_Exp(), Number_Log(), Number_Expt();

Import void
        Number_Sin(), Number_Cos(), Number_Tan(),
        Number_Asin(), Number_Acos(), Number_Atan(), Number_Atan2();

Import void
	Number_Exact_To_Inexact(), Number_Inexact_To_Exact(),
	Number_To_String(), Cstring_To_Number();

Import	String	Integer_To_Cstring();

Import void
        Number_Make_Rectangular(), Number_Make_Polar(), 
        Number_Real_Part(), Number_Imaginary_Part(), 
        Number_Magnitude(), Number_Angle(); 


Import Integer	Number_To_Integer();
Import void	Integer_To_Number(/* Integer */);

Import void Initialize_Number();
Import struct Object_Struct * Make_Bignum_Zero();

Import void Promote();
Import void Demote();


Import Integer Number_Sign();
