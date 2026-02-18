/* fixnum.h -- UMB Scheme, fixnum interface.

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

Import Boolean
	Is_Fixnum_Zero(), Is_Fixnum_Positive(), Is_Fixnum_Negative(),
        Is_Fixnum_Odd(), Is_Fixnum_Even(), Is_Fixnum_Exact(), 
	Is_Fixnum_Inexact();

Import Boolean
	Fixnum_Less_Than(), Fixnum_Greater_Than(), Fixnum_Equal(),
        Fixnum_Less_Than_Or_Equal(), Fixnum_Greater_Than_Or_Equal();

Import void
	Fixnum_Add(), Fixnum_Subtract(), Fixnum_Multiply(),
        Fixnum_Divide(), Fixnum_Quotient(), Fixnum_Remainder(),
	Fixnum_Modulo(), Fixnum_Negate(), Fixnum_Abs(),
	Fixnum_Numerator(), Fixnum_Denominator(), Fixnum_Rationalize();
		
Import void
	Fixnum_Max(), Fixnum_Min(), Fixnum_GCD(), Fixnum_LCM();

Import void
	Fixnum_Floor(), Fixnum_Ceiling(), Fixnum_Truncate(), Fixnum_Round();

Import void
	Fixnum_Sqrt(), Fixnum_Exp(), Fixnum_Log(), Fixnum_Expt();

Import void
        Fixnum_Sin(), Fixnum_Cos(), Fixnum_Tan(),
        Fixnum_Asin(), Fixnum_Acos(), Fixnum_Atan(), Fixnum_Atan2();

Import void
	Fixnum_Exact_To_Inexact(), Fixnum_Inexact_To_Exact(),
	Fixnum_To_String();

Import void
        Fixnum_Make_Rectangular(), Fixnum_Make_Polar(), 
        Fixnum_Real_Part(), Fixnum_Imaginary_Part(), 
        Fixnum_Magnitude(), Fixnum_Angle(); 
