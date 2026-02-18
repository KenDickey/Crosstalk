/* complex.h -- UMB Scheme, complex interface.

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

/* Complex numbers are two reals. */

Import Boolean
	Is_Complex_Zero(), Is_Complex_Positive(), Is_Complex_Negative(),
        Is_Complex_Odd(), Is_Complex_Even(), Is_Complex_Exact(), 
	Is_Complex_Inexact();

Import Boolean 
	Complex_Less_Than(), Complex_Greater_Than(), Complex_Equal(),
        Complex_Less_Than_Or_Equal(), Complex_Greater_Than_Or_Equal();

Import void
	Complex_Add(), Complex_Subtract(), Complex_Multiply(),
        Complex_Divide(), Complex_Quotient(), Complex_Remainder(),
	Complex_Modulo(), Complex_Negate(), Complex_Abs(),
	Complex_Numerator(), Complex_Denominator(), Complex_Rationalize();
		
Import void
	Complex_Max(), Complex_Min(), Complex_GCD(), Complex_LCM();

Import void
	Complex_Floor(), Complex_Ceiling(), Complex_Truncate(), Complex_Round();

Import void
	Complex_Sqrt(), Complex_Exp(), Complex_Log(), Complex_Expt();

Import void
        Complex_Sin(), Complex_Cos(), Complex_Tan(),
        Complex_Asin(), Complex_Acos(), Complex_Atan(), Complex_Atan2();

Import void
	Complex_Exact_To_Inexact(), Complex_Inexact_To_Exact(),
	Complex_To_String();

Import void
        Complex_Make_Rectangular(), Complex_Make_Polar(), 
        Complex_Real_Part(), Complex_Imaginary_Part(), 
        Complex_Magnitude(), Complex_Angle(); 
