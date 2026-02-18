/* architecture.h -- UMB Scheme, architecture interface.

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

Modified to run on linux by Thomas Mullaly.

For additional information about UMB Scheme, contact the author:

	Bill Campbell
	Department of Mathematics and Computer Science
	University of Massachusetts at Boston
	Harbor Campus
	Boston, MA 02125

	Telephone: 617-287-6449		Internet: bill@cs.umb.edu

*/

/* Register set. */

Import	Object	Expression_Register ;
Import	Object	Value_Register ;
Import	Object	Environment_Register ;
Import	Object	Function_Register ;
Import	Object	Arguments_Register ;
Import  Object  State_Register ;
Import	ELabel	PC_Register ;

/* Place for saving top level registers while in debug mode. */

Import	Object	Value_Debugged ;
Import  Object  State_Debugged ;

/* Argument stack. */

#define ARG_STACK_LIMIT 50000
#define	ARG_STACK_SIZE	(ARG_STACK_LIMIT+2)

Import	Object	Arg_Stack[] ;
Import	Integer	Arg_Stack_Ptr ;

#define Push(x)		{Object Xxxx = (x);\
			 if (Arg_Stack_Ptr >= ARG_STACK_LIMIT)\
				 Error( "Argument Stack Overflow." );\
			 else Arg_Stack[Arg_Stack_Ptr++]=Xxxx;}
#define	Top(n)		(Arg_Stack[Arg_Stack_Ptr-(n)])
#define	Pop(n)		Arg_Stack_Ptr-=(n)
#define Replace(n,x)	Arg_Stack[Arg_Stack_Ptr-(n)]=(x)
#define Reset_Stack(n)	Arg_Stack_Ptr=(n)
#define	Current_Stack()	Arg_Stack_Ptr;

/* State stack. */

Import	void	Save() ;
Import	void	Restore() ;

/* The environment. */

Import	void	Assign( /* Object sym, val, env */ ) ;
Import	void	Define( /* Object sym, val, env */ ) ;
Import	void	Extend_Environment( /* Integer frame_size */) ;
Import	Object  Intern_Name( /* String name */ ) ;
Import	void    Symbol_Hash_Iterate();

/* Memory management. */
Import	void	Get_Heap_Size();
Import	void	Get_Arg_Stack_Ptr();
Import	Object	Allocate();
Import	void	Freeze_Stable_Space();
Import	Boolean	Allocating;
Import	Boolean	Show_GC_Messages;


Import void Initialize_Architecture();

/* The garbage collector. */
Import Object Move_Object();
Import void Relocate();

#define Is_Forwarded(o) (((Integral_Pointer) Get_Type(o)) & 1)
#define Get_Forwarding_Address(o) \
		((Object) (((Integral_Pointer) Get_Type(o)) & ~1))
#define Set_Forwarding_Address(old,new) \
		Get_Type(old) = (Scheme_Type) ((Integral_Pointer)(new) | 1)

/* General */
Import Object Copy_Object();


