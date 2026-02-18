/* eval.h -- UMB Scheme, eval interface 

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

typedef enum
{
	EVAL_EXPRESSION,	EVAL_APPLY,		EVAL_ARGUMENTS,
	STACK_ARGUMENT,		PERFORM_APPLICATION,	EVAL_DEFINITION,
	EVAL_CONDITIONAL,	EVAL_SEQUENCE,		EVAL_ASSIGNMENT,
	RETURN,			APPLICATION_COMPLETE,   OVERWRITE_PROMISE
} ELabel;

Import void Eval();
Import void Self_Eval();

Import void Apply_Eval();
Import void Lambda_Eval();
Import void Conditional_Eval();
Import void Assignment_Eval();
Import void Definition_Eval();
Import void Defmacro_Call_Eval();
Import void Sequence_Eval();
Import void Delay_Eval();
Import void State_Frame_Eval();
Import void Variable_Eval();
