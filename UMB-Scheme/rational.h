/* rational.h -- UMB Scheme, rational number interface (not implemented)./exter

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
	Telephone: 617-929-7971		Internet: bill@umb.umb.edu

*/


Import Boolean
	Is_Rational_Zero(), Is_Rational_Positive(), Is_Rational_Negative(),
        Is_Rational_Odd(), Is_Rational_Even(), Is_Rational_Exact(), 
	Is_Rational_Inexact();

Import Boolean
	Rational_Less_Than(), Rational_Greater_Than(), Rational_Equal(),
        Rational_Less_Than_Or_Equal(), Rational_Greater_Than_Or_Equal();

Import void
	Rational_Add(), Rational_Subtract(), Rational_Multiply(),
        Rational_Divide(), Rational_Quotient(), Rational_Remainder(),
	Rational_Modulo(), Rational_Negate(), Rational_Abs(),
	Rational_Numerator(), Rational_Denominator(), Rational_Rationalize();
		
Import void
	Rational_Max(), Rational_Min(), Rational_GCD(), Rational_LCM();

Import void
	Rational_Floor(), Rational_Ceiling(), Rational_Truncate(),
	Rational_Round();

Import void
	Rational_Sqrt(), Rational_Exp(), Rational_Log(), Rational_Expt();

Import void
        Rational_Sin(), Rational_Cos(), Rational_Tan(),
        Rational_Asin(), Rational_Acos(), Rational_Atan(), Rational_Atan2();

Import void
	Rational_Exact_To_Inexact(), Rational_Inexact_To_Exact(),
	Rational_To_String();

Import void
        Rational_Make_Rectangular(), Rational_Make_Polar(), 
        Rational_Real_Part(), Rational_Imaginary_Part(), 
        Rational_Magnitude(), Rational_Angle(); 
