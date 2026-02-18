/* debug.h -- UMB Scheme, debugger interface.

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

Import	void	Steer_Debugging();
Import	void	Initialize_Debug();

Import	Boolean	Control_C;
Import	Boolean	Debugger_Switched_On;
Import	Boolean	Debugger_Activated;
Import	Boolean Debugging;
Import	Boolean	Go_Processed;
Import	Boolean	Evaluating;
Import	Boolean	Evaluation_Broken;
Import	Boolean	Tracing;
Import	Boolean	Tracing_All;
Import	Integer	Stepping;
Import	Integer	Stepper;

Import	Object	Traced_Procedures;
Import	Integer	Trace_Margin;

#define	TRACE_INDENT		4
#define	MAX_TRACE_MARGIN 	40

#define	Traced(p)	((Is_Procedure(p)&&Get_Procedure_Tracing(p)) || \
			 (Is_Primitive(p)&&Get_Primitive_Tracing(p)))
#define Name_For(p)	(Is_Procedure(p)?Get_Procedure_Name(p):\
			 Is_Primitive(p)?Get_Primitive_Name(p):\
			 "<continuation>")
#define	Trace_Right()	(Trace_Margin=\
		((Trace_Margin>=MAX_TRACE_MARGIN)?0:Trace_Margin+TRACE_INDENT))
#define	Trace_Left()	(Trace_Margin=\
		((Trace_Margin<=0)?MAX_TRACE_MARGIN:Trace_Margin-TRACE_INDENT))
