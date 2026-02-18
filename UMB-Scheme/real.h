/* real.h -- UMB Scheme, real interface.

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

#define MAX_CHARS_PER_REAL 255

Import Boolean
	Is_Real_Zero(), Is_Real_Positive(), Is_Real_Negative(),
        Is_Real_Odd(), Is_Real_Even(), Is_Real_Exact(), 
	Is_Real_Inexact();

Import  Boolean
	Real_Less_Than(), Real_Greater_Than(), Real_Equal(),
        Real_Less_Than_Or_Equal(), Real_Greater_Than_Or_Equal();

Import void
	Real_Add(), Real_Subtract(), Real_Multiply(),
        Real_Divide(), Real_Quotient(), Real_Remainder(),
	Real_Modulo(), Real_Negate(), Real_Abs(),
	Real_Numerator(), Real_Denominator(), Real_Rationalize(); 
		
Import void
	Real_Max(), Real_Min(), Real_GCD(), Real_LCM();

Import void
	Real_Floor(), Real_Ceiling(), Real_Truncate(), Real_Round();

Import void
	Real_Sqrt(), Real_Exp(), Real_Log(), Real_Expt();

Import void
        Real_Sin(), Real_Cos(), Real_Tan(),
        Real_Asin(), Real_Acos(), Real_Atan(), Real_Atan2();

Import void
	Real_Exact_To_Inexact(), Real_Inexact_To_Exact(),
	Real_To_String();

Import void
        Real_Make_Rectangular(), Real_Make_Polar(), 
        Real_Real_Part(), Real_Imaginary_Part(), 
        Real_Magnitude(), Real_Angle(); 
