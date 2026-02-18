/* bignum.h -- UMB Scheme, bignum interface.

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
	Is_Bignum_Zero(), Is_Bignum_Positive(), Is_Bignum_Negative(),
        Is_Bignum_Odd(), Is_Bignum_Even(), Is_Bignum_Exact(), 
	Is_Bignum_Inexact();

Import Boolean
	Bignum_Less_Than(), Bignum_Greater_Than(), Bignum_Equal(),
        Bignum_Less_Than_Or_Equal(), Bignum_Greater_Than_Or_Equal();

Import void
	Bignum_Add(), Bignum_Subtract(), Bignum_Multiply(),
        Bignum_Divide(), Bignum_Quotient(), Bignum_Remainder(),
	Bignum_Modulo(), Bignum_Negate(), Bignum_Abs(),
	Bignum_Numerator(), Bignum_Denominator(), Bignum_Rationalize();
		
Import void
	Bignum_Max(), Bignum_Min(), Bignum_GCD(), Bignum_LCM();

Import void
	Bignum_Floor(), Bignum_Ceiling(), Bignum_Truncate(), Bignum_Round();

Import void
	Bignum_Sqrt(), Bignum_Exp(), Bignum_Log(), Bignum_Expt();

Import void
        Bignum_Sin(), Bignum_Cos(), Bignum_Tan(),
        Bignum_Asin(), Bignum_Acos(), Bignum_Atan(), Bignum_Atan2();

Import void
	Bignum_Exact_To_Inexact(), Bignum_Inexact_To_Exact(),
	Bignum_To_String();

Import void
        Bignum_Make_Rectangular(), Bignum_Make_Polar(), 
        Bignum_Real_Part(), Bignum_Imaginary_Part(), 
        Bignum_Magnitude(), Bignum_Angle(); 


Import void 	Make_Small_Bignum();
Import Integer	Bignum_To_Integer();
Import void	Integer_To_Bignum();
